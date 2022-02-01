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

  wisp: atom({
    key: "wisp",
    default: null,
  }),
}

function Wisp() {
  let [wisp, setWisp] = useRecoilState(Atoms.wisp)
  let [lines, setLines] = useRecoilState(Atoms.lines)

  function print(text, tag) {
    setLines(lines => [...lines, { text, tag }])
  }

  useEffect(async () => {
    let Module = await loadWisp({
      ENV: { WISP_HEAP: "/wisp/heap" },
      printErr: x => {
        console.info(x)
        print(x, "stderr")
      },
      print(x) {
        console.log(x)
        print(x, "stdout")
      },
    })
    
    console.log("loaded Wisp")
    Module.FS.mkdir("/wisp")
    Module.FS.mount(Module.IDBFS, {}, "/wisp")
    Module.ccall("wisp_main", null, null, [])
    // Module.FS.syncfs(true, err => {
    //   if (err) {
    //     throw err
    //   } else {
    //     console.log("syncfs loaded")
    //     Module.ccall("wisp_main", null, null, [])
    //   }
    // })

    window.WispModule = Module
    setWisp(Module)
  }, [])

  
  return (
    wisp ? <REPL /> : "Loading..." 
  )
}

function Line({ data }) {
  return (
    <div className={data.tag}>
      {data.text}
    </div>
  )
}

function REPL() {
  let [lines, setLines] = useRecoilState(Atoms.lines)
  let [input, setInput] = useState("")
  let outputRef = React.useRef(null)
  let wisp = useRecoilValue(Atoms.wisp)

  let handleSubmit = useCallback(e => {
    e.preventDefault()
    
    let result = wisp.ccall(
      "wisp_eval_code",
      "number",
      ["string"],
      [input]
    )

    // wisp.ccall(
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
        {lines.map((x, i) => <Line data={x} key={i} />)}
      </div>
      <form id="form" onSubmit={handleSubmit}>
        <span>{">"}</span>
        <input id="input" autoFocus autoComplete="off" />
      </form>
    </div>
  )
}

ReactDOM.render(
  <RecoilRoot>
    <Wisp />
  </RecoilRoot>,
  document.querySelector("#app")
)
