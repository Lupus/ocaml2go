/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/trunk/testsuite/tests/lib-option/test.ml */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/


let strf = Printf.sprintf;
let assert_raise_invalid_argument = (f, v) => {
  assert(
    try(
      {
        ignore(f(v));
        false;
      }
    ) {
    | Invalid_argument(_) => true
    },
  );
  ();
};

let test_none_some = () => {
  assert(Option.none == None);
  assert(Option.some(2) == Some(2));
  ();
};

let test_value = () => {
  assert(Option.value(None, ~default=5) == 5);
  assert(Option.value(Some(3), ~default=5) == 3);
  ();
};

let test_get = () => {
  assert_raise_invalid_argument(Option.get, None);
  assert(Option.get(Some(2)) == 2);
  ();
};

let test_bind = () => {
  assert(Option.bind(Some(3), x => Some(succ(x))) == Some(4));
  assert(Option.bind(Some(3), _ => None) == None);
  assert(Option.bind(None, x => Some(succ(x))) == None);
  assert(Option.bind(None, _ => None) == None);
  ();
};

let test_join = () => {
  assert(Option.join(Some(Some(3))) == Some(3));
  assert(Option.join(Some(None)) == None);
  assert(Option.join(None) == None);
  ();
};

let test_map = () => {
  assert(Option.map(succ, Some(3)) == Some(4));
  assert(Option.map(succ, None) == None);
  ();
};

let test_fold = () => {
  assert(Option.fold(~none=3, ~some=succ, Some(1)) == 2);
  assert(Option.fold(~none=3, ~some=succ, None) == 3);
  assert(Option.(fold(~none, ~some))(Some(1)) == Some(1));
  assert(Option.(fold(~none, ~some))(None) == None);
  ();
};

let test_iter = () => {
  let count = ref(0);
  let set_count = x => count := x;
  assert(count^ == 0);
  Option.iter(set_count, Some(2));
  assert(count^ == 2);
  Option.iter(set_count, None);
  assert(count^ == 2);
  ();
};

let test_is_none_some = () => {
  assert(Option.is_none(None) == true);
  assert(Option.is_some(None) == false);
  assert(Option.is_none(Some(2)) == false);
  assert(Option.is_some(Some(2)) == true);
  ();
};

let test_equal = () => {
  let eq = (v0, v1) => v0 mod 2 == v1 mod 2;
  let equal = Option.equal(eq);
  assert((!) @@ equal(Some(2), Some(3)));
  assert(equal(Some(2), Some(4)));
  assert((!) @@ equal(Some(2), None));
  assert((!) @@ equal(None, Some(3)));
  assert((!) @@ equal(None, Some(4)));
  assert(equal(None, None));
  ();
};

let test_compare = () => {
  let compare = (v0, v1) => - compare(v0, v1);
  let compare = Option.compare(compare);
  assert(compare(Some(2), Some(1)) == (-1));
  assert(compare(Some(2), Some(2)) == 0);
  assert(compare(Some(2), Some(3)) == 1);
  assert(compare(Some(2), None) == 1);
  assert(compare(None, Some(1)) == (-1));
  assert(compare(None, Some(2)) == (-1));
  assert(compare(None, Some(3)) == (-1));
  assert(compare(None, None) == 0);
  ();
};

let test_to_option_list_seq = () => {
  assert(Option.to_result(~none=6, Some(3)) == Ok(3));
  assert(Option.to_result(~none=6, None) == Error(6));
  assert(Option.to_list(Some(3)) == [3]);
  assert(Option.to_list(None) == []);
  switch ((Option.to_seq(Some(3)))()) {
  | [@implicit_arity] Seq.Cons(3, f) => assert(f() == Seq.Nil)
  | _ => assert(false)
  };
  assert((Option.to_seq(None))() == Seq.Nil);
  ();
};

let tests = () => {
  test_none_some();
  test_value();
  test_get();
  test_bind();
  test_join();
  test_map();
  test_fold();
  test_iter();
  test_is_none_some();
  test_equal();
  test_compare();
  test_to_option_list_seq();
  ();
};

let run = () => {
  tests();
  print_endline("OK");
};
