type test_result =
  | Pass
  | Fail string;

let t description test_fn => {
  let wrapped_test_fn () => {
    test_fn ();
    Pass
  };
  let run_test_fn () => {
    let result =
      try (wrapped_test_fn ()) {
      | Assert_failure (file_name, line, col) =>
        Fail (
          "["
          ^ file_name
          ^ ":"
          ^ string_of_int line
          ^ ":"
          ^ string_of_int col
          ^ "]"
        )
      };
    result
  };
  (description, run_test_fn)
};
