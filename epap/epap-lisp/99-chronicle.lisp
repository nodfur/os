(uiop:define-package :epap-chronicle-base
  (:use :spinneret :common-lisp :trivia)
  (:export define-chronicle-category))

(common-lisp:in-package :epap-chronicle-base)

(defparameter *chronicle* '())

(defun add-to-chronicle (item)
  (setf *chronicle* (cons item *chronicle*)))

(defmacro define-chronicle-category (name args)
  `(defmacro ,name (,@args &body body)
     (defparameter ,name nil)
     (list 'add-to-chronicle
           (list 'list (list 'quote ',name) (list 'quote (list ,@args))
                 (list 'quote body)))))

(defun sexp-html (item)
  (ematch item
    ((type number) `(:span :class number ,item))
    ((type string) `(:span :class string ,(format nil "~s" item)))
    ((type null) `(:div))
    ((list (eql 'function) x) `(:div :class function-name ,(sexp-html x)))
    ((type list) `(:div :class list ,@(mapcar #'sexp-html item)))
    ((type symbol)
     (let* ((symbol-name (symbol-name item))
            (package-name (package-name (symbol-package item)))
            (external (not (find package-name
                                 '("KEYWORD" "EPAP-CHRONICLE" "COMMON-LISP")
                                 :test #'equal))))
       `(:span :class symbol
               :data-name ,symbol-name
               :data-package ,package-name
               :data-external ,(if external "yes" "no")
               (:span :class package-name
                      :data-package ,package-name
                      ,(if (equal package-name "KEYWORD") "" package-name))
               (:span :class symbol-name ,symbol-name))))))

(hunchentoot:define-easy-handler (chronicle :uri "/chronicle") ()
  (with-html-string
    (:html
     (:head
      (:title "EPAP-CHRONICLE")
      (:style (:raw "
html { font: 18px \"DM Mono\"; }
html { color: #222; }
* { box-sizing: border-box; }
.list { display: flex; flex-wrap: wrap; align-items: center; }
.list { border: 1px solid #bbb; padding: .1rem .5rem; }
.list { margin: .25rem; margin-right: .5rem; }
.list { border-radius: .25rem; }
.list { background: #0000ff06; }
.list { max-width: 40em; }
[data-name=SHOULD] { color: #a00; }
[data-name=SHOULD],
[data-name=HMM],
[data-name=DATE],
[data-name=SEE],
[data-name=TODO],
[data-name=YAY] { font-weight: bold; }
{ color: #a00; }
.symbol { text-transform: lowercase; }
.symbol, .number, .string { margin-right: .5rem; }
.string { font-family: helvetica; }
.function-name { margin-right: .5rem; }
*:last-child { margin-right: 0; }
.package-name[data-package=EPAP-CHRONICLE] { display: none; }
.package-name[data-package=COMMON-LISP] { display: none; }
.package-name::after { content: ':'; }
.symbol[data-external=yes] { text-transform: uppercase; }

.symbol[data-external=yes] { opacity: 0.7; }
         ")))
     (:body
      (:main
       (dolist (item *chronicle*)
         (:div :style "margin-bottom: .5rem"
               (let ((spinneret:*suppress-inserted-spaces* t)
                     (spinneret:*html-style* :tree)
                     (*print-pretty* nil))
                 (interpret-html-tree (sexp-html item))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(uiop:define-package :epap-chronicle
  (:use :epap-chronicle-base)
  (:import-from :common-lisp quote))

(common-lisp:in-package :epap-chronicle)

(common-lisp:defparameter *categories* 
  '(
    ask
    hey
    hmm
    ugh
    umm
    wat
    yay
    did
    ))

(common-lisp:dolist
    (category *categories*)
  (common-lisp:eval
   `(define-chronicle-category ,category (id))))

(define-chronicle-category todo (id))
(define-chronicle-category done (id))
(define-chronicle-category code ())
(define-chronicle-category date (weekday day month year))
(define-chronicle-category see ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :tuesday 7 :december 2021)

(yay CHRONICLE-STARTED
  (this is a funny idea)
  (like an issue tracker inside the program)
  (we can reference symbols)
  (there is a function #'EPAP::DRAW-LETTER))

(todo NEED-CLEAN-SCREEN-RESET
  (now I can only blank the screen in the quick mode)
  (but this does not wipe away the e-ink ghosting)
  (should implement the slow blanking function)
  (should fix #'EPAP::COPY-AREA-TO-FRAMEBUFFER)
  (should fix #'EPAP::DISPLAY-AREA))

(hmm CONSIDERING-GRAYSCALE-BUFFER
  (now our local buffer is a bitmap)
  (but maybe it should be a 4-bit grayscale matrix)
  (then we can render it as a bitmap or a grayscale)
  (but let's do NEED-CLEAN-SCREEN-RESET in a simple way))

(done NEED-CLEAN-SCREEN-RESET
  (made #'EPAP::INITIALIZE-BLANK-DISPLAY))

(hmm VARIABLE-WIDTH-POETRY
  (now we assume fixed-width fonts)
  (but I want to look at poems in the :CONCRETE-ROMAN font)
  (well we can already do text shaping with HARFBUZZ)
  (so I can just do that with live updates)
  (see #'EPAP::DRAW-TEXT-LINE)
  (todo LIVE-RENDER-POEM-WITH-SHAPING))

(yay SELF-CHRONICLING-SYSTEM
  (imagine viewing the chronicle itself on the e-ink screen)
  (this would be a beautiful way to start the day))

(yay VARIABLE-WIDTH-POETRY
  (done LIVE-RENDER-POEM-WITH-SHAPING
    (see #'EPAP::POEM)
    (see #'EPAP::TYPESET-LINE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :wednesday 8 :december 2021)

(yay EXHIBITING-HAN-SHAN
  (now there is #'EPAP::COLD-MOUNTAIN)
  (it is beautiful to see poems appearing like this)
  (hmm STREAMING-GPT3-POETRY
    (todo SIMPLE-GPT3-API
      (same thing as my old Telegram thing))
    (maybe GPT-3 integration will be a primary fun thing)))

(hmm TYPESETTING
  (now we can do basic text shaping and rendering)
  (but we want a more capable typesetting language)
  (like (CENTERING-HEADINGS)
        (PARAGRAPH-WRAPPING (like Knuth))
        (MULTI-COLUMN-LAYOUTS)
        (CONSTRAINT-SOLVING))
  (code
    (page
     (center
      (with-font :sans 92
        "Words from Cold Mountain")
      (with-font :sans 64
        "Twenty-Seven Poems by Han-Shan"))))
  (todo RESEARCH-LISP-TYPESETTING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :thursday 9 :december 2021)

(hmm KNUTH-PLASS-ALGORITHM
  (the TeX standard algorithm for paragraph layout)
  (see
    (KNUTH-PLASS-1981
     (article "Breaking Paragraphs into Lines")
     (url "http://www.eprg.org/G53DOC/pdfs/knuth-plass-breaking.pdf"))
    (KNUTH-PLASS-THOUGHTS
     (github :user "jaroslov" :repo "knuth-plass-thoughts")))
  
  (the basic idea is to minimize a measure of badness
       (but brute force would be extravagantly expensive)
       (elasticity parameters rule out most breakpoints)
       (then we explore a search tree with pruning))

  (todo TRY-IMPLEMENTING-KNUTH-PLASS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :friday 10 :december 2021)

(hmm DRY-RUN
  (now we can run #'EPAP::COLD-MOUNTAIN with #'EPAP::DRY-RUN)
  (now we have #'EPAP::DEFUN-WITH-DRY-RUN)
  (like a primitive object dispatch system))

(hmm BROWSER-INTEGRATION
  (now we have a super basic web server)
  (see #'EPAP::WEB-APP)
  (? but what is it going to do)
  (maybe it should display the chronicle)
  (todo CHRONICLE-ON-THE-WEB))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :saturday 11 :december 2021
  "I'm alone overnight and playing with Lisp.")

;; (hmm (thinking about "TeX")
;;   ((1) well knuth did a good job)
;;   ((2) and we can just install it)
;;   ((3) then typeset everything))

