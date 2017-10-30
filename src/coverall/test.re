type test_result =
  | Pass
  | Fail(string);

type test_context = {
  .
  test: (string, unit => unit) => unit, all: unit => list((string, unit => test_result))
};

let m = (module_name) : test_context => {
  val tests = ref([]);
  pub test = (description, test_fn) => {
    let wrapped_test_fn = () => {
      test_fn();
      Pass
    };
    let run_test_fn = () => {
      let result =
        try (wrapped_test_fn()) {
        | Assert_failure((file_name, line, col)) =>
          Fail(
            "["
            ++ (
              file_name ++ (":" ++ (string_of_int(line) ++ (":" ++ (string_of_int(col) ++ "]"))))
            )
          )
        };
      result
    };
    this#add_test(description, run_test_fn)
  };
  pub all = () => tests^;
  pri add_test = (description, run_test_fn) => {
    let test_name = module_name ++ (" - " ++ description);
    tests := tests^ @ [(test_name, run_test_fn)]
  }
};
