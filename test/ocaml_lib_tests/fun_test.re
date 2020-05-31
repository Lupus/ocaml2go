/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/trunk/testsuite/tests/lib-fun/test.ml */
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


let test_id = () => {
  assert(Fun.id(true) == true);
  assert(Fun.id(1) == 1);
  assert(!(Fun.id(nan) == nan));
  ();
};

let test_const = () => {
  assert(Fun.const(true, false) == true);
  assert(Fun.const(0, false) == 0);
  assert(Fun.const(0, 4) == 0);
  ();
};

let test_flip = () => {
  assert(Fun.flip((++), "of order", "out ") == "out of order");
  assert(Fun.flip(List.append, [2], [1]) == [1, 2]);
  assert(Fun.flip(List.cons, [2], 1) == [1, 2]);
  ();
};

let test_negate = () => {
  assert(Fun.negate(Bool.equal(true), true) == false);
  assert(Fun.negate(Bool.equal(true), false) == true);
  ();
};

let test_protect = () => {
  let does_raise = (f, x) =>
    try(
      {
        f(x);
        false;
      }
    ) {
    | _ => true
    };

  let double_raise = () => {
    let f = () => raise(Exit);
    try(Fun.protect(~finally=f, f, ())) {
    | Exit => ()
    };
  };

  assert(does_raise(double_raise, ()));
};

let tests = () => {
  test_id();
  test_const();
  test_flip();
  test_negate();
  test_protect();
  ();
};

let run = () => {
  tests();
  print_endline("OK");
  ();
};
