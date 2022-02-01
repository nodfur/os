#include "wisp.h"

bool
wisp_find_binding_in_scope (wisp_word_t scope,
                            wisp_word_t symbol,
                            wisp_word_t *result)
{
  wisp_word_t *header = wisp_deref (scope);
  wisp_word_t *slots = header + 2;
  int size = wisp_header_word_data (header[0]) / 2;
  for (int i = 0; i < size; i++)
    {
      wisp_word_t variable = slots[i * 2];
      if (variable == symbol)
        {
          *result = slots[i * 2 + 1];
          return true;
        }
    }

  return false;
}

bool
wisp_find_binding (wisp_word_t scopes,
                   wisp_word_t symbol,
                   wisp_word_t *result)
{
  if (scopes == NIL)
    return false;

  wisp_word_t *scopes_cons = wisp_deref (scopes);

  wisp_word_t scopes_car = scopes_cons[0];
  wisp_word_t scopes_cdr = scopes_cons[1];

  if (wisp_find_binding_in_scope (scopes_car, symbol, result))
    return true;

  return wisp_find_binding (scopes_cdr, symbol, result);
}

wisp_word_t
wisp_make_apply_plan (wisp_word_t callee,
                      wisp_word_t values,
                      wisp_word_t terms,
                      wisp_word_t scopes,
                      wisp_word_t next)
{
  return wisp_make_instance
    (APPLY, 5, callee, values, terms, scopes, next);
}

wisp_apply_plan_t *
wisp_get_apply_plan (wisp_word_t *header)
{
  return (wisp_apply_plan_t *) (header + 2);
}

bool
wisp_term_irreducible (wisp_word_t term)
{
  switch (term & WISP_LOWTAG_MASK)
    {
    case WISP_LOWTAG_FIXNUM_0:
    case WISP_LOWTAG_FIXNUM_1:
      return true;

    default:
      if (term == NIL)
        return true;

      if (WISP_IS_LIST_PTR (term))
        return false;

      if (WISP_IS_STRUCT_PTR (term))
        return true;

      if (wisp_is_symbol (term))
        return false;

      if (WISP_LOWTAG (term) == WISP_LOWTAG_OTHER_PTR)
        {
          wisp_word_t *header = wisp_deref (term);
          if (WISP_WIDETAG (header[0]) == WISP_WIDETAG_STRING)
            return true;
        }

      wisp_crash ("strange term");
    }
}

wisp_closure_t *
wisp_get_closure (wisp_word_t value)
{
  wisp_word_t *symbol_header = wisp_is_symbol (value);

  wisp_word_t *closure_header =
    symbol_header
    ? wisp_is_instance (symbol_header[6], CLOSURE)
    : wisp_is_instance (value, CLOSURE);

  if (closure_header)
    return (wisp_closure_t *) (closure_header + 2);
  else
    wisp_crash ("not a function");
}

wisp_word_t
wisp_lambda_list_to_params (wisp_word_t lambda_list)
{
  int length = wisp_length (lambda_list);
  wisp_word_t slots[length];

  /* fprintf (stderr, ";; %d ", length); */
  /* wisp_dump (lambda_list); */
  /* fprintf (stderr, "\n"); */

  for (int i = 0; i < length; i++)
    {
      wisp_word_t *cons = wisp_deref (lambda_list);
      slots[i] = cons[0];
      lambda_list = cons[1];
    }

  wisp_word_t params =
    wisp_make_instance_with_slots (PARAMS, length, slots);

  return params;
}


wisp_machine_t
wisp_step_into_function (wisp_machine_t *machine,
                         bool backwards,
                         wisp_apply_plan_t *apply_plan)
{
  // We've evaluated all the subforms of a function application.
  // Now switch the machine term to the closure body, the machine
  // scopes to the closure scopes extended with the bound parameters,
  // and the machine plan to the continuation of the apply plan.

  wisp_closure_t *closure =
    wisp_get_closure (apply_plan->callee);

  wisp_word_t *parameter_struct =
    wisp_deref (closure->params);

  wisp_word_t parameter_count =
    wisp_header_word_data (parameter_struct[0]) - 1;

  /* fprintf (stderr, "; stepping into "); */
  /* wisp_dump (apply_plan->callee); */
  /* fprintf (stderr, " %d\n", parameter_count); */

  wisp_word_t *parameter_names =
    parameter_struct + 2;

  wisp_word_t scope_slots[2 * parameter_count];

  wisp_word_t values =
    apply_plan->values;

  for (int i = 0; i < parameter_count; i++)
    {
      int parameter_index = backwards
        ? parameter_count - i - 1
        : i;

      assert (values != NIL);

      wisp_word_t *cons = wisp_deref (values);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      scope_slots[parameter_index * 2] =
        parameter_names[parameter_index];

      scope_slots[parameter_index * 2 + 1] =
        car;

      values = cdr;
    }

  wisp_word_t parameter_scope =
    wisp_make_instance_with_slots
    (SCOPE, 2 * parameter_count, scope_slots);

  if (WISP_WIDETAG (closure->body) == WISP_WIDETAG_BUILTIN)
    {
      wisp_word_t builtin = closure->body >> 8;
      /* fprintf (stderr, ";; builtin %d\n", builtin); */

      switch (builtin)
        {
        case WISP_BUILTIN_LAMBDA:
          {
            wisp_word_t lambda_list = scope_slots[1];
            wisp_word_t lambda_body = scope_slots[3];

            wisp_word_t lambda_params =
              wisp_lambda_list_to_params (lambda_list);

            wisp_word_t closure = wisp_make_instance
              (CLOSURE, 4,
               lambda_params,
               lambda_body,
               machine->scopes,
               NIL);

            return (wisp_machine_t) {
              .term = closure,
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next
            };
          }

        case WISP_BUILTIN_MACRO:
          {
            wisp_word_t macro_list = scope_slots[1];
            wisp_word_t macro_body = scope_slots[3];

            wisp_word_t macro_params =
              wisp_lambda_list_to_params (macro_list);

            wisp_word_t closure = wisp_make_instance
              (CLOSURE, 4,
               macro_params,
               macro_body,
               machine->scopes,
               MACRO);

            return (wisp_machine_t) {
              .term = closure,
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next
            };
          }

        case WISP_BUILTIN_SET_SYMBOL_FUNCTION:
          {
            wisp_set_symbol_function
              (scope_slots[1], scope_slots[3]);

            return (wisp_machine_t) {
              .term = scope_slots[3],
              .value = false,
              .scopes = machine->scopes,
              .plan = apply_plan->next
            };
          }

        case WISP_BUILTIN_CONS:
          {
            return (wisp_machine_t) {
              .term = wisp_cons (scope_slots[1], scope_slots[3]),
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next,
            };
          }

        case WISP_BUILTIN_SAVE_HEAP:
          {
            char *heap_path = getenv ("WISP_HEAP");
            wisp_save_heap (heap_path);

            return (wisp_machine_t) {
              .term = T,
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next,
            };
          }

        default:
          wisp_crash ("unknown builtin");
        }
    }

  if (closure->macro == NIL)
    return (wisp_machine_t) {
      .term = closure->body,
      .value = false,
      .scopes = wisp_cons (parameter_scope, closure->scopes),
      .plan = apply_plan->next
    };

  else
    {
      wisp_word_t eval_plan =
        wisp_make_instance (EVAL, 2,
                            machine->scopes,
                            apply_plan->next);

      return (wisp_machine_t) {
        .term = closure->body,
        .value = false,
        .scopes = wisp_cons (parameter_scope, closure->scopes),
        .plan = eval_plan
      };
    }

}

bool
wisp_follow_plan (wisp_machine_t *machine)
{
  wisp_word_t term = machine->term;
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  if (!WISP_IS_STRUCT_PTR (plan))
    wisp_crash ("bad plan");

  wisp_word_t *header = wisp_deref (plan);
  wisp_word_t type = wisp_struct_header_type (header);

  if (type == APPLY)
    {
      wisp_apply_plan_t *apply_plan =
        wisp_get_apply_plan (header);

      wisp_word_t terms = apply_plan->terms;
      if (terms == NIL)
        {
          wisp_word_t new_apply_plan =
            wisp_make_apply_plan
            (apply_plan->callee,
             wisp_cons (term, apply_plan->values),
             NIL,
             apply_plan->scopes,
             apply_plan->next);

          *machine = wisp_step_into_function
            (machine,
             true,
             wisp_get_apply_plan
             (wisp_deref (new_apply_plan)));

          return true;
        }
      else
        {
          wisp_word_t *cons =
            wisp_deref (terms);

          wisp_word_t car = cons[0];
          wisp_word_t cdr = cons[1];

          wisp_word_t next_apply_plan =
            wisp_make_apply_plan
            (apply_plan->callee,
             wisp_cons (term, apply_plan->values),
             cdr,
             apply_plan->scopes,
             apply_plan->next);

          machine->term = car;
          machine->value = false;
          machine->plan = next_apply_plan;

          return true;
        }
    }

  else if (type == EVAL)
    {
      /* fprintf (stderr, "; eval\n"); */
      machine->value = false;
      machine->scopes = header[2];
      machine->plan = header[3];
      return true;
    }

  wisp_crash ("bad plan");
}

bool
wisp_step_into_nullary_call (wisp_machine_t *machine,
                             wisp_word_t callee)
{
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  // XXX: It's not necessary to allocate a plan on the heap here.

  wisp_word_t apply_plan =
    wisp_make_apply_plan
    (callee, NIL, NIL, scopes, plan);

  *machine = wisp_step_into_function
    (machine,
     false,
     wisp_get_apply_plan
     (wisp_deref (apply_plan)));

  return true;
}

bool
wisp_start_evaluating_arguments (wisp_machine_t *machine,
                                 wisp_word_t callee,
                                 wisp_word_t args)
{
  wisp_word_t *term_list = wisp_deref (args);
  wisp_word_t first_term = term_list[0];
  wisp_word_t remaining_terms = term_list[1];

  wisp_word_t apply_plan = wisp_make_apply_plan
    (callee, NIL, remaining_terms,
     machine->scopes, machine->plan);

  machine->term = first_term;
  machine->value = false;
  machine->plan = apply_plan;

  return true;
}

bool
wisp_step_into_macro_call (wisp_machine_t *machine,
                           wisp_word_t callee,
                           wisp_word_t args)
{
  wisp_word_t apply_plan = wisp_make_apply_plan
    (callee, args, NIL, machine->scopes, machine->plan);

  *machine = wisp_step_into_function
    (machine,
     false,
     wisp_get_apply_plan
     (wisp_deref (apply_plan)));

  return true;
}

bool
wisp_step_into_call (wisp_machine_t *machine,
                     wisp_word_t callee,
                     wisp_word_t args)
{
  wisp_word_t term = machine->term;
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  if (wisp_is_symbol (callee))
    {
      wisp_closure_t *closure =
        wisp_get_closure (callee);

      if (args == NIL)
        return wisp_step_into_nullary_call
          (machine, callee);

      else if (closure->macro == NIL)
        return wisp_start_evaluating_arguments
          (machine, callee, args);

      else
        return wisp_step_into_macro_call
          (machine, callee, args);
    }

  wisp_crash ("bad call");
}

bool
wisp_step (wisp_machine_t *machine)
{
  wisp_word_t term = machine->term;
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  if (machine->value || wisp_term_irreducible (term))
    {
      if (plan == NIL)
        return false;
      else
        return wisp_follow_plan (machine);
    }

  else if (WISP_IS_LIST_PTR (term))
    {
      wisp_word_t *cons = wisp_deref (term);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      if (car == QUOTE)
        {
          machine->term = wisp_car (cdr);
          machine->value = true;
          return true;
        }

      else
        return wisp_step_into_call (machine, car, cdr);
    }

  else if (wisp_is_symbol (term))
    {
      wisp_word_t binding;

      if (wisp_find_binding (machine->scopes, term, &binding))
        {
          machine->term = binding;
          machine->value = true;
          return true;
        }

      else
        wisp_crash ("unbound variable");
    }

  wisp_crash ("bad term");
}
