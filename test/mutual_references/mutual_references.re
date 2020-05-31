/* copied from https://github.com/jordwalke/rehp/blob/master/rehack_tests/mutual_references/mutual_references.re */
let rec f = (x, y) => 1
and z = Some((f, "Second Part Of Tuple:"));

switch (z) {
| None => ()
| Some((f, str)) => print_string(str ++ string_of_int(f(0, 0)))
};

