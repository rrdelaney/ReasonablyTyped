type suite_result = {
  passed: int,
  failed: int
};

let run = (tests) => {
  print_endline("\027[1;36mStarting test run...\027[0m");
  let {passed, failed} =
    List.fold_left(
      (results, (description, run_test)) => {
        let status = run_test();
        switch status {
        | Test.Pass =>
          print_endline("\027[1;32m \226\156\148 " ++ (description ++ "\027[0m"));
          {...results, passed: results.passed + 1}
        | Test.Fail(reason) =>
          print_endline(
            "\027[1;31m \226\156\152 " ++ (description ++ (" " ++ (reason ++ "\027[0m")))
          );
          {...results, failed: results.failed + 1}
        }
      },
      {passed: 0, failed: 0},
      tests
    );
  print_newline();
  if (passed > 0) {
    print_endline("\027[1;32m" ++ (string_of_int(passed) ++ " tests passed!\027[0m"))
  };
  if (failed > 0) {
    print_endline("\027[1;31m" ++ (string_of_int(failed) ++ " tests failed!\027[0m"))
  };
  if (failed == 0 && passed == 0) {
    print_endline("No tests run")
  }
};
