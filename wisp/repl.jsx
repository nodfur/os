import * as ReactDOM from "react-dom"

function print(text, klass) {
  let x = document.createElement("DIV")
  x.innerText = text
  if (klass) x.classList.add(...klass.split(" "))
  output.appendChild(x)
  output.scrollTop = output.scrollHeight
}

Module = {
  printErr: x => print(x, "stderr"),
  print: x => print(x, "stdout"),

  preRun: [
    () => {
      FS.mkdir("/wisp")
      FS.mount(IDBFS, {}, "/wisp")
      FS.syncfs(true, err => {
        if (err)
          console.error(err)
        else {
          console.log("syncfs loaded")
          Module.ccall("wisp_main", null, null, [])
        }
      })

      ENV.WISP_HEAP = "/wisp/heap"
    }
  ]
}

form.onsubmit = e => {
  e.preventDefault()

  let code = input.value

  print(`> ${code}`, "stdin")

  let result = Module.ccall(
    "wisp_eval_code",
    "number",
    ["string"],
    [code]
  )

  console.log(`${code}`)
  console.log(`${result}`)

  Module.ccall(
    "wisp_dump_stdout",
    null,
    ["number"],
    [result]
  )

  input.value = ""
  return false
}
