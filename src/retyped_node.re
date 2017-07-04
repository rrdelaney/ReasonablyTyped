let node_compile name def => {
  let (module_id, flow_code, bs_code) =
    Retyped.Compiler.compile (Js.to_string name) (Js.to_string def);
  let js_result = [|
    Js.string module_id,
    Js.string flow_code,
    Js.string bs_code
  |];
  Js.array js_result
};

Js.export "compile" node_compile;
