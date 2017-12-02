Printexc.record_backtrace(true);

type test_case = {
  filename: string,
  name: string,
  fn: unit => unit
};

let suite = ref([]);

let it = (name, fn) => suite := suite^ @ [{name, fn, filename: ""}];

let collect = (filename, ()) =>
  suite :=
    List.map(
      (test_case) => {
        ...test_case,
        filename: test_case.filename == "" ? filename : test_case.filename
      },
      suite^
    );