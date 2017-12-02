open Spec.Expect;

Spec.Test.it("should test", () => assert false);

Spec.Test.it(
  "should be 10",
  () => {
    let a = 10;
    expect(a) |> not_ |> toBe(~message="a to be 5", 5)
  }
);

let test = Spec.Test.collect("genutils_test.re");