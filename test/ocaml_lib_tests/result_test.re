/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/trunk/testsuite/tests/lib-result/test.ml */
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

let test_ok_error = () => {
  assert(Result.ok(3) == Ok(3));
  assert(Result.error("ha!") == Error("ha!"));
  ();
};

let test_value = () => {
  assert(Result.value(Ok(3), ~default=5) == 3);
  assert(Result.value(Error("ha!"), ~default=5) == 5);
  ();
};

let test_get_ok_error = () => {
  assert(Result.get_ok(Ok(3)) == 3);
  assert_raise_invalid_argument(Result.get_ok, Error("ha!"));
  assert(Result.get_error(Error("ha!")) == "ha!");
  assert_raise_invalid_argument(Result.get_error, Ok(2));
  ();
};

let test_bind = () => {
  assert(Result.bind(Ok(3), x => Ok(succ(x))) == Ok(4));
  assert(
    Result.bind(Ok(3), x => Error(strf("hu%d!", x))) == Error("hu3!"),
  );
  assert(Result.bind(Error("ha!"), x => Ok(succ(x))) == Error("ha!"));
  assert(Result.bind(Error("ha!"), _ => Error("hu!")) == Error("ha!"));
  ();
};

let test_join = () => {
  assert(Result.join(Ok(Ok(3))) == Ok(3));
  assert(Result.join(Ok(Error("ha!"))) == Error("ha!"));
  assert(Result.join(Error("ha!")) == Error("ha!"));
  ();
};

let test_maps = () => {
  assert(Result.map(succ, Ok(3)) == Ok(4));
  assert(Result.map(succ, Error("ha!")) == Error("ha!"));
  assert(Result.map_error(succ, Error(3)) == Error(4));
  assert(Result.map_error(succ, Ok(2)) == Ok(2));
  ();
};

let test_fold = () => {
  assert(Result.fold(~ok=succ, ~error=succ, Ok(1)) == 2);
  assert(Result.fold(~ok=succ, ~error=succ, Error(1)) == 2);
  assert(Result.(fold(~ok, ~error))(Ok(1)) == Ok(1));
  assert(Result.(fold(~ok, ~error))(Error("ha!")) == Error("ha!"));
  ();
};

let test_iters = () => {
  let count = ref(0);
  let set_count = x => count := x;
  assert(count^ == 0);
  Result.iter(set_count, Ok(2));
  assert(count^ == 2);
  Result.iter(set_count, Error("ha!"));
  assert(count^ == 2);
  Result.iter_error(set_count, Error(3));
  assert(count^ == 3);
  Result.iter_error(set_count, Ok("ha!"));
  assert(count^ == 3);
  ();
};

let test_is_ok_error = () => {
  assert(Result.is_ok(Ok(2)) == true);
  assert(Result.is_error(Ok(2)) == false);
  assert(Result.is_ok(Error("ha!")) == false);
  assert(Result.is_error(Error("ha!")) == true);
  ();
};

let test_equal = () => {
  let ok = (v0, v1) => v0 mod 2 == v1 mod 2;
  let error = ok;
  let equal = Result.equal(~ok, ~error);
  assert((!) @@ equal(Ok(2), Ok(3)));
  assert(equal(Ok(2), Ok(4)));
  assert((!) @@ equal(Ok(2), Error(3)));
  assert((!) @@ equal(Ok(2), Error(4)));
  assert((!) @@ equal(Error(2), Ok(3)));
  assert((!) @@ equal(Error(2), Ok(4)));
  assert((!) @@ equal(Error(2), Error(3)));
  assert(equal(Error(2), Error(4)));
  ();
};

let test_compare = () => {
  let ok = (v0, v1) => - compare(v0, v1);
  let error = ok;
  let compare = Result.compare(~ok, ~error);
  assert(compare(Ok(2), Ok(1)) == (-1));
  assert(compare(Ok(2), Ok(2)) == 0);
  assert(compare(Ok(2), Ok(3)) == 1);
  assert(compare(Ok(2), Error(1)) == (-1));
  assert(compare(Ok(2), Error(2)) == (-1));
  assert(compare(Ok(2), Error(3)) == (-1));
  assert(compare(Error(2), Ok(1)) == 1);
  assert(compare(Error(2), Ok(2)) == 1);
  assert(compare(Error(2), Ok(3)) == 1);
  assert(compare(Error(2), Error(1)) == (-1));
  assert(compare(Error(2), Error(2)) == 0);
  assert(compare(Error(2), Error(3)) == 1);
  ();
};

let test_to_option_list_seq = () => {
  assert(Result.to_option(Ok(3)) == Some(3));
  assert(Result.to_option(Error("ha!")) == None);
  assert(Result.to_list(Ok(3)) == [3]);
  assert(Result.to_list(Error("ha!")) == []);
  switch ((Result.to_seq(Ok(3)))()) {
  | [@implicit_arity] Seq.Cons(3, f) => assert(f() == Seq.Nil)
  | _ => assert(false)
  };
  assert((Result.to_seq(Error("ha!")))() == Seq.Nil);
  ();
};

let tests = () => {
  test_ok_error();
  test_value();
  test_get_ok_error();
  test_bind();
  test_join();
  test_maps();
  test_fold();
  test_iters();
  test_is_ok_error();
  test_equal();
  test_compare();
  test_to_option_list_seq();
  ();
};

let run = () => {
  tests();
  print_endline("OK");
};
