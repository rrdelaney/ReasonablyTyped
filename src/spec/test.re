type test_status =
  | Pass
  | Fail(string);

type test_result = {
  name: string,
  status: test_status
};

type test_case = {
  name: string,
  fn: unit => unit
};

let suite = ref([]);

let run_test = ({name, fn}) => {
  let result =
    switch (fn()) {
    | result => {name, status: Pass}
    | exception _ => {name, status: Fail(":P")}
    };
  let ok =
    if (result.status == Pass) {
      "ok"
    } else {
      "not ok"
    };
  let reason = result.name;
  let output = ok ++ " - " ++ reason;
  print_endline(output)
};

let it = (name, fn) => suite := suite^ @ [{name, fn}];

let collect = ((), ()) => ();