exception Expectation(option(string));

let assert_with_message = (assertion, inverted, message) =>
  if (assertion == false && inverted == false || assertion == true && inverted == true) {
    raise(Expectation(message))
  } else {
    ()
  };

type expectation('t) = {
  value: 't,
  inverted: bool
};

let expect = (t) => {value: t, inverted: false};

let toBe = (v, ~message=?, e) => assert_with_message(e.value == v, e.inverted, message);

let not_ = (exc) => {...exc, inverted: ! exc.inverted};