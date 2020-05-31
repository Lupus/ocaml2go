/* adapted from https://github.com/ocaml/ocaml/blob/4.08.1/testsuite/tests/lib-bool/test.ml */
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


let test_not = () => {
  assert(Bool.(!)(false) == true);
  assert(Bool.(!)(true) == false);
  ();
};

let test_and = () => {
  let wit = ref(0);
  assert(
    Bool.(&&)(
      {
        incr(wit);
        false;
      },
      {
        incr(wit);
        false;
      },
    )
    == false,
  );
  assert(wit^ == 1);
  wit := 0;
  assert(
    Bool.(&&)(
      {
        incr(wit);
        false;
      },
      {
        incr(wit);
        true;
      },
    )
    == false,
  );
  assert(wit^ == 1);
  wit := 0;
  assert(
    Bool.(&&)(
      {
        incr(wit);
        true;
      },
      {
        incr(wit);
        false;
      },
    )
    == false,
  );
  assert(wit^ == 2);
  wit := 0;
  assert(
    Bool.(&&)(
      {
        incr(wit);
        true;
      },
      {
        incr(wit);
        true;
      },
    )
    == true,
  );
  assert(wit^ == 2);
  wit := 0;
  ();
};

let test_or = () => {
  let wit = ref(0);
  assert(
    Bool.(||)(
      {
        incr(wit);
        false;
      },
      {
        incr(wit);
        false;
      },
    )
    == false,
  );
  assert(wit^ == 2);
  wit := 0;
  assert(
    Bool.(||)(
      {
        incr(wit);
        false;
      },
      {
        incr(wit);
        true;
      },
    )
    == true,
  );
  assert(wit^ == 2);
  wit := 0;
  assert(
    Bool.(||)(
      {
        incr(wit);
        true;
      },
      {
        incr(wit);
        false;
      },
    )
    == true,
  );
  assert(wit^ == 1);
  wit := 0;
  assert(
    Bool.(||)(
      {
        incr(wit);
        true;
      },
      {
        incr(wit);
        true;
      },
    )
    == true,
  );
  assert(wit^ == 1);
  wit := 0;
  ();
};

let test_equal = () => {
  assert(Bool.equal(false, false) == true);
  assert(Bool.equal(false, true) == false);
  assert(Bool.equal(true, false) == false);
  assert(Bool.equal(true, true) == true);
  ();
};

let test_compare = () => {
  assert(Bool.compare(false, false) == 0);
  assert(Bool.compare(false, true) == (-1));
  assert(Bool.compare(true, false) == 1);
  assert(Bool.compare(true, true) == 0);
  ();
};

let test_to_int = () => {
  assert(Bool.to_int(false) == 0);
  assert(Bool.to_int(true) == 1);
  ();
};

let test_to_float = () => {
  assert(Bool.to_float(false) == 0.);
  assert(Bool.to_float(true) == 1.);
  ();
};

let test_to_string = () => {
  assert(Bool.to_string(false) == "false");
  assert(Bool.to_string(true) == "true");
  ();
};

let run = () => {
  test_not();
  test_and();
  test_or();
  test_equal();
  test_compare();
  test_to_int();
  test_to_float();
  test_to_string();
  print_endline("OK");
};
