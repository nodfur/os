import * as ReactDOM from "react-dom"

import React, {
  useState, useCallback, useEffect
} from "react"

import {
  RecoilRoot, atom, useRecoilState, useRecoilValue
} from "recoil"

const NIL = {
  type: Symbol.for("SYMBOL"),
  name: "NIL",
  "function": null
}

NIL["function"] = NIL

const Atoms = {
  lines: atom({
    key: "lines",
    default: [],
  }),

  booted: atom({
    key: "booted",
    default: false,
  }),
}

let WispModule

function Wisp() {
  let [, setLines] = useRecoilState(Atoms.lines)
  let [booted, setBooted] = useRecoilState(Atoms.booted)

  function print(text, tag) {
    let parts = text.split(/[«»]/)
    console.log(parts)

    let things = []

    for (let i = 0; i < parts.length; i++) {
      if (i % 2 == 0) {
        if (parts[i] !== "")
          things.push({ text: parts[i], tag })
      } else {
        if (parts[i].match(/^.*? 0x(.*)$/)) {
          let value = grokValue(parseInt(RegExp.$1, 16))
          things.push({ text: <Object value={value} />, tag })
        }
      }
    }

    setLines(lines => [...lines, things])
  }

  useEffect(async () => {
    let Module = await loadWisp({
      printErr: x => {
        console.info(x)
        print(x, "stderr")
      },
      print(x) {
        console.log(x)
        print(x, "stdout")
      },
      preRun(Module) {
        Module.ENV.WISP_HEAP = "/wisp/heap"
      }
    })

    console.log("loaded Wisp")
    Module.FS.mkdir("/wisp")
    Module.FS.mount(Module.IDBFS, {}, "/wisp")
    Module.FS.syncfs(true, err => {
      if (err) {
        throw err
      } else {
        console.log("syncfs loaded")
        Module.ccall("wisp_main", null, null, [])
        setBooted(true)
      }
    })

    window.WispModule = WispModule = Module
  }, [])


  return (
    <div
      style={{
        display: "flex",
        flexDirection: "row",
        gap: ".75rem",
        height: "100%"
      }}
      className={ booted ? "fade-in now" : "fade-in later" }>
      { booted ? <><Browser /><REPL /></> : null }
    </div>
  )
}

function Line({ data }) {
  if (data.text) {
    return (
      <div className={data.tag}>
        {data.text}
      </div>
    )
  } else debugger
}

function lowtag(x) {
  return x & ((1 << 3) - 1)
}

function widetag(x) {
  return x & ((1 << 8) - 1)
}

function deref(x) {
  return x & ~((1 << 3) - 1)
}

let heapCache = {}

function grok(heap, x) {
  switch (lowtag(x)) {
  case 0:
  case 4:
    return x >> 2

  case 1:
  case 5:
  case 7:
    return grokPointer(heap, deref(x))

  case 3:
    return grokList(heap, x)

  case 2:
  case 6:
    return grokImmediate(heap, x)

  default:
    throw new Error(`lowtag ${lowtag(x)}`)
  }
}

const widetags = {
  0xC2: "INSTANCE",
  0x32: "STRING",
  0xAE: "SYMBOL",
  0xA2: "BUILTIN",
}

function grokImmediate(heap, x) {
  let value = heapCache[x] = {}

  let widetagNumber = x & ((1 << 8) - 1)
  let widetag = widetags[widetagNumber]

  switch (widetag) {
  case "BUILTIN":
    value.builtin = headerData(x)
    break

  default:
    throw new Error(`${widetagNumber}`)
  }

  return value
}

function grokPointer(heap, x) {
  if (heapCache[x])
    return heapCache[x]

  let header = heap.getUint32(x, true)
  let widetagNumber = header & ((1 << 8) - 1)
  let widetag = widetags[widetagNumber]

  switch (widetag) {
  case "INSTANCE":
    return grokInstance(heap, x, headerData(header) - 1, x + 4)

  case "SYMBOL":
    return grokSymbol(heap, x)

  case "STRING":
    return grokString(heap, headerData(header), x + 4)

  default:
    debugger
    throw new Error(widetag)
  }
}

function headerData(x) {
  return x >> 8
}

function grokInstance(heap, address, slotCount, x) {
  let value = {
    type: Symbol.for("INSTANCE"),
    address,
    klass: undefined,
    slots: undefined,
  }

  heapCache[x] = value

  value.klass = grok(heap, heap.getUint32(x, true))

  let slots = []
  for (let i = 0; i < slotCount; i++) {
    let value = grok(heap, heap.getUint32(x + 4 * (i + 1), true))
    slots.push(value)
  }

  value.slots = slots

  return value
}

function grokSymbol(heap, x) {
  let value = heapCache[x] = {
    type: Symbol.for("SYMBOL"),
    name: undefined,
    "function": undefined,
  }

  value.name = grok(heap, heap.getUint32(x + 4 * 4, true))
  value.value = grok(heap, heap.getUint32(x + 1 * 4, true))
  value["function"] = grok(heap, heap.getUint32(x + 4 * 6, true))

  return value
}

function grokString(heap, length, x) {
  let decoder = new TextDecoder
  return decoder.decode(
    heap.buffer.slice(
      heap.byteOffset + x,
      heap.byteOffset + x + length,
    )
  )
}

function grokList(heap, x) {
  if (x === 3) {
    return NIL
  }

  let y = deref(x)
  return {
    type: Symbol.for("CONS"),
    car: grok(heap, heap.getUint32(y, true)),
    cdr: grok(heap, heap.getUint32(y + 4, true)),
  }
}

function grokValue(x) {
  let heapBase = WispModule.ccall(
    "wisp_get_heap_pointer", null, ["u8*"])

  let heap = new DataView(
    WispModule.HEAPU8.buffer,
    heapBase,
    4 * 1024 * 1024)

  return grok(heap, x)
}

function listElements(cons) {
  let items = []
  while (cons != NIL) {
    items.push(cons.car)
    cons = cons.cdr
  }

  return items
}

function sortBy (list, key) {
  return list.concat().sort(
    (a, b) => (a[key] > b[key])
      ? 1
      : ((b[key] > a[key])
         ? -1
         : 0)
  )
};

function SymbolList({ title, symbols }) {
  if (symbols.length === 0)
    return null
  else
    return (
      <section>
        <header>{title}</header>
        <ul>
          {
            symbols.map((x, i) =>
              <li key={i}>
                {x.name}
              </li>)
          }
        </ul>
      </section>
    )
}

function Package({ instance }) {
  let allSymbols = sortBy(
    listElements(instance.slots[1]),
    "name"
  )

  let builtins = []
  let functions = []
  let macros = []
  let variables = []
  let symbols = []

  for (let symbol of allSymbols) {
    let f = symbol["function"]
    let v = symbol.value
    if (v !== NIL) {
      variables.push(symbol)
    } else if (f !== NIL) {
      if (typeof f.builtin === "number") {
        builtins.push(symbol)
      } else if (f.slots[3] !== NIL) {
        macros.push(symbol)
      } else {
        functions.push(symbol)
      }
    } else {
      symbols.push(symbol)
    }
  }

  return (
    <div>
      <section>
        <SymbolList title="Macros" symbols={macros} />
        <SymbolList title="Functions" symbols={functions} />
        <SymbolList title="Builtins" symbols={builtins} />
        <SymbolList title="Variables" symbols={variables} />
      </section>
    </div>
  )
}

function Browser() {
  let booted = useRecoilValue(Atoms.booted)

  let [value, setValue] = React.useState(null)

  React.useEffect(() => {
    if (!booted) return

    setValue(grokValue(0x7D))

  }, [booted])

  return (
    <div className="browser">
      <header className="titlebar">
        <span>
          <b>WISP</b>
        </span>
        <span>
          Package
        </span>
      </header>
      <div className="bg p-1 scroll-y">
        {value ? <Package instance={value} /> : "..."}
      </div>
    </div>
  )
}

const slotNames = {
  CLOSURE: ["Params", "Code", "Scopes", "Macro"],
  "DOM-ELEMENT": ["Tag", "Attributes", "Body"],
}

function slotName(value, i) {
  let names = slotNames[value.klass.name]
  return names ? names[i] : `Slot ${i}`
}


function Object({ value }) {
  let [expanded, setExpanded] = React.useState(false)

  let toggle = React.useCallback((e) => {
    e.stopPropagation()
    setExpanded(x => !x)
  })

  if (value.type === Symbol.for("INSTANCE")) {
    if (expanded)
      return (
        <table className="instance" onClick={toggle}>
          <tbody>
            <tr>
              <td>Class</td>
              <td><Object value={value.klass} /></td>
            </tr>
            {
              value.slots.map((slot, i) => (
                <tr key={i}>
                  <td>{slotName(value, i)}</td>
                  <td><Object value={slot} /></td>
                </tr>
              ))
            }
          </tbody>
        </table>
      )
    else
      return (
        <div className="instance" onClick={toggle}>
          <Object value={value.klass} />
          {` @${value.address.toString(10)}`}
        </div>
      )

  } else if (value.type === Symbol.for("SYMBOL")) {
    return (
      <span className="symbol">{value.name}</span>
    )

  } else if (value.type === Symbol.for("CONS")) {
    let items = []
    let list = value
    let last

    while (true) {
      items.push(list.car)
      list = list.cdr

      if (list === NIL) {
        break
      } else if (list.type != Symbol.for("CONS")) {
        last = list
        break
      }
    }

    return (
      <div className="list">
        {items.map((x, i) => <Object value={x} key={i} />)}{
          last ? <span>. <Object value={last} /></span> : null
        }
      </div>
    )

  } else if (value === NIL) {
    return <span className="symbol">NIL</span>

  } else if (typeof value === "number") {
    return <span className="number">{value}</span>

  } else if (typeof value.builtin === "number") {
    return (
      <span className="builtin">
        {`«BUILTIN ${value.builtin}»`}
      </span>
    )

  } else {
    return <span className="string">{`"${value}"`}</span>
  }
}

function REPL() {
  let [lines, setLines] = useRecoilState(Atoms.lines)

  let [input, setInput] = useState("")

  let outputRef = React.useRef(null)

  let handleChange = useCallback(e => {
    setInput(e.target.value)
  })

  function evalCode(code) {
    console.log(code)

    let result = WispModule.ccall(
      "wisp_eval_code",
      "number",
      ["string"],
      [code]
    )

    let sexp = grokValue(
      WispModule.ccall(
        "wisp_read_from_string",
        "number",
        ["string"],
        [code]
      )
    )

    console.log(result)

    let value = grokValue(result)
    console.log(value)

    setLines(lines => [
      ...lines, [
        {
          text: <Object value={sexp} />,
          tag: "stdin"
        },
        {
          text: "→",
        },
        {
          text: <Object value={value} />,
          tag: "stdout"
        }
      ]
    ])
  }

  let handleSubmit = useCallback(e => {
    e.preventDefault()
    evalCode(input)
    setInput("")
  })

  useEffect(() => {
    outputRef.current.scrollTop = outputRef.current.scrollHeight
  }, [lines])

  useEffect(() => {
    evalCode("(defun foo (x y) (cons y x))")
  }, [])

  return (
    <div id="repl">
      <header className="titlebar">
        <span>
          <b>Notebook</b>
        </span>
        <span>
          Package: <em>WISP</em>
        </span>
      </header>
      <div id="output" ref={outputRef}>
        {
          lines.map((xs, i) =>
            <div key={i} className="chunk">
              {
                xs.map((x, j) =>
                  <Line data={x} key={j} />
                )
              }
            </div>
          )
        }
      </div>
      <form id="form" onSubmit={handleSubmit}>
        <input
          id="input" autoFocus autoComplete="off"
          onChange={handleChange}
          value={input}
        />
      </form>
    </div>
  )
}

onload = () => {
  ReactDOM.render(
    <RecoilRoot>
      <Wisp />
    </RecoilRoot>,
    document.querySelector("#app")
  )
}
