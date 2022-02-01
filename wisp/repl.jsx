import * as ReactDOM from "react-dom"

import React, {
  useState, useCallback, useEffect
} from "react"

import {
  RecoilRoot, atom, useRecoilState, useRecoilValue
} from "recoil"

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
  let [, setBooted] = useRecoilState(Atoms.booted)

  function print(text, tag) {
    setLines(lines => [...lines, [{ text, tag }]])
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
    <div style={{ display: "flex", flexDirection: "column" }}>
      <REPL />
      <Browser />
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
  }
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
    return grokInstance(heap, headerData(header) - 1, x + 4)

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

function grokInstance(heap, slotCount, x) {
  let value = {
    type: Symbol.for("INSTANCE"),
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
    return Symbol.for("NIL")
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

function Browser() {
  let booted = useRecoilValue(Atoms.booted)

  let [value, setValue] = React.useState(null)

  React.useEffect(() => {
    if (!booted) return
    
    setValue(grokValue(0x7D))
    
  }, [booted])

  return (
    <div className="browser">
      {value ? <Object value={value} /> : "..."}
    </div>
  )
}

function Object({ value }) {
  let [expanded, setExpanded] = React.useState(false)

  if (value.type === Symbol.for("INSTANCE")) {
    return (
      <table className="instance">
        <tbody>
          <tr>
            <td>Class</td>
            <td><Object value={value.klass} /></td>
          </tr>
          {
            value.slots.map((slot, i) => (
              <tr key={i}>
                <td>Slot {`${i}`}</td>
                <td><Object value={slot} /></td>
              </tr>
            ))
          }
        </tbody>
      </table>
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

      if (list === Symbol.for("NIL")) {
        break
      } else if (list.type != Symbol.for("CONS")) {
        last = list
        break
      } 
    }

    return (
      <div className="list">
        ({items.map((x, i) => <Object value={x} key={i} />)}{
          last ? <span>. <Object value={last} /></span> : null
        })
      </div>
    )

  } else if (value === Symbol.for("NIL")) {
    return <span className="symbol">NIL</span>
    
  } else if (typeof value === "number") {
    return <span className="number">{value}</span>
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

  let handleSubmit = useCallback(e => {
    e.preventDefault()

    console.log(input)

    let result = WispModule.ccall(
      "wisp_eval_code",
      "number",
      ["string"],
      [input]
    )

    console.log(result)

    let value = grokValue(result)
    console.log(value)

    setLines(lines => [
      ...lines, [
        {
          text: input,
          tag: "stdin"
        },
        {
          text: <Object value={value} />,
          tag: "stdout"
        }
      ]
    ])

    // WispModule.ccall(
    //   "wisp_dump_stdout",
    //   null,
    //   ["number"],
    //   [result]
    // )

    setInput("")
  })

  useEffect(() => {
    outputRef.current.scrollTop = outputRef.current.scrollHeight
  }, [lines])

  return (
    <div id="repl">
      <div id="output" ref={outputRef}>
        <header className="stderr">
          {";;; code: "}
          <a href="https://github.com/nodfur/os/tree/main/wisp">
            https://github.com/nodfur/os/tree/main/wisp
          </a>
        </header>
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
        <span>{">"}</span>
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
