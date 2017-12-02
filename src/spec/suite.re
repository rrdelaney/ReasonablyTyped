let run = () => {
  let test_suite = Test.suite^;
  let length = List.length(test_suite) |> string_of_int;
  print_endline("1.." ++ length);
  List.iter((case) => Test.run_test(case), test_suite)
};