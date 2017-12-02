type test_status =
  | Pass
  | Fail(string);

type test_result = {
  name: string,
  status: test_status
};

let run_test = ({filename, name, fn}: Test.test_case) =>
  switch (fn()) {
  | result => {name, status: Pass}
  | exception (Expect.Expectation(message)) =>
    let trace = Printexc.get_backtrace();
    {
      name,
      status:
        Fail(
          switch message {
          | Some(message) => "Expected " ++ message
          | None => "Failed at" ++ trace
          }
        )
    }
  | exception (Assert_failure((file, line, col))) => {
      name,
      status: Fail("Failed at " ++ file ++ ":" ++ string_of_int(line) ++ ":" ++ string_of_int(col))
    }
  };

module Runner = {
  type runner_type =
    | TAP
    | Jest;
  type runner = {
    case: Test.test_case => unit,
    before_run: list(Test.test_case) => unit,
    after_case: (Test.test_case, test_result) => unit
  };
  let tap = {
    case: (case) => (),
    before_run: (cases) => {
      let length = List.length(cases);
      print_endline("1.." ++ string_of_int(length))
    },
    after_case: (case, result) => {
      let m =
        switch result.status {
        | Pass => "ok - " ++ case.filename ++ ":" ++ case.name
        | Fail(message) => "not ok - " ++ case.filename ++ ":" ++ case.name ++ " # " ++ message
        };
      print_endline(m)
    }
  };
  let jest = {case: (case) => (), before_run: (cases) => (), after_case: (case, result) => ()};
};

let run = (runner_type) => {
  let test_suite = Test.suite^;
  let runner =
    switch runner_type {
    | Runner.Jest => Runner.jest
    | Runner.TAP => Runner.tap
    };
  List.iter(runner.case, test_suite);
  runner.before_run(test_suite);
  List.map((case) => (case, run_test(case)), test_suite)
  |> List.iter(((case, result)) => runner.after_case(case, result))
};