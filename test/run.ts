import { decode } from "https://deno.land/std@0.204.0/encoding/base64.ts";
import { TextLineStream } from "https://deno.land/std@0.204.0/streams/mod.ts";

const lines = Deno.stdin.readable
  .pipeThrough(new TextDecoderStream())
  .pipeThrough(new TextLineStream());

let exit = false;
let module, instance;
for await (let line of lines) {
  const msg = JSON.parse(line);
  switch (msg.header) {
    case "instantiate":
      const code = decode(msg.code);
      let instance_results = await WebAssembly.instantiate(code, {
          "bootstrap": {
              "jl_exception_tag": new WebAssembly.Tag({parameters: []})
          }
      });
      module = instance_results.module;
      instance = instance_results.instance;
      console.log(JSON.stringify({ result: "ok", }))
      break;
    case "call":
      const func = instance.exports[msg.func];
      const res = func(...msg.params);
      console.log(
        JSON.stringify({ call: msg.func, params: msg.params, result: res }),
      );
      break;
    case "exit":
      exit = true;
      break;
    default:
      throw (`invalid header ${msg.header}`);
  }
  if (exit) {
    break;
  }
}

console.log(JSON.stringify({ result: "exit" }));

// const code = Deno.readFileSync("f.wasm")
// const tag_type = {parameters: []}
// const {instance,} = await WebAssembly.instantiate(code, {
//   "bootstrap": {
//     "jl_exception_tag": new WebAssembly.Tag(tag_type),
//   },
// })
// const {f,} = instance.exports
//
// console.log(f(3.))
// console.log(f(-3.))
