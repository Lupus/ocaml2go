/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/trunk/testsuite/tests/lib-stack/test.ml */
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


module S = {
  include Stack;

  let to_list = s => {
    /* from bottom to top */
    let l = ref([]);
    iter(x => l := [x, ...l^], s);
    l^;
  };
};

let does_raise = (f, s) =>
  try(
    {
      ignore(f(s): int);
      false;
    }
  ) {
  | S.Empty => true
  };

let run = () => {
  let () = {
    let s = S.create();
    ();
    assert(S.to_list(s) == [] && S.length(s) == 0);
    S.push(1, s);
    assert(S.to_list(s) == [1] && S.length(s) == 1);
    S.push(2, s);
    assert(S.to_list(s) == [1, 2] && S.length(s) == 2);
    S.push(3, s);
    assert(S.to_list(s) == [1, 2, 3] && S.length(s) == 3);
    S.push(4, s);
    assert(S.to_list(s) == [1, 2, 3, 4] && S.length(s) == 4);
    assert(S.pop(s) == 4);
    assert(S.to_list(s) == [1, 2, 3] && S.length(s) == 3);
    assert(S.pop(s) == 3);
    assert(S.to_list(s) == [1, 2] && S.length(s) == 2);
    assert(S.pop(s) == 2);
    assert(S.to_list(s) == [1] && S.length(s) == 1);
    assert(S.pop(s) == 1);
    assert(S.to_list(s) == [] && S.length(s) == 0);
    assert(does_raise(S.pop, s));
  };

  let () = {
    let s = S.create();
    S.push(1, s);
    assert(S.pop(s) == 1);
    assert(does_raise(S.pop, s));
    S.push(2, s);
    assert(S.pop(s) == 2);
    assert(does_raise(S.pop, s));
    assert(S.length(s) == 0);
  };

  let () = {
    let s = S.create();
    S.push(1, s);
    assert(S.top(s) == 1);
    S.push(2, s);
    assert(S.top(s) == 2);
    S.push(3, s);
    assert(S.top(s) == 3);
    assert(S.top(s) == 3);
    assert(S.pop(s) == 3);
    assert(S.top(s) == 2);
    assert(S.pop(s) == 2);
    assert(S.top(s) == 1);
    assert(S.pop(s) == 1);
    assert(does_raise(S.top, s));
    assert(does_raise(S.top, s));
  };

  let () = {
    let s = S.create();
    for (i in 1 to 10) {
      S.push(i, s);
    };
    S.clear(s);
    assert(S.length(s) == 0);
    assert(does_raise(S.pop, s));
    assert(s == S.create());
    S.push(42, s);
    assert(S.pop(s) == 42);
  };

  let () = {
    let s1 = S.create();
    for (i in 1 to 10) {
      S.push(i, s1);
    };
    let s2 = S.copy(s1);
    assert(S.to_list(s1) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    assert(S.to_list(s2) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    assert(S.length(s1) == 10);
    assert(S.length(s2) == 10);
    for (i in 10 downto 1) {
      assert(S.pop(s1) == i);
    };
    for (i in 10 downto 1) {
      assert(S.pop(s2) == i);
    };
  };

  let () = {
    let s = S.create();
    assert(S.is_empty(s));
    for (i in 1 to 10) {
      S.push(i, s);
      assert(S.length(s) == i);
      assert(!S.is_empty(s));
    };
    for (i in 10 downto 1) {
      assert(S.length(s) == i);
      assert(!S.is_empty(s));
      ignore(S.pop(s): int);
    };
    assert(S.length(s) == 0);
    assert(S.is_empty(s));
  };

  let () = {
    let s = S.create();
    for (i in 10 downto 1) {
      S.push(i, s);
    };
    let i = ref(1);
    S.iter(
      j => {
        assert(i^ == j);
        incr(i);
      },
      s,
    );
  };

  let () = {
    let s1 = S.create();
    assert(S.length(s1) == 0);
    assert(S.to_list(s1) == []);
    let s2 = S.copy(s1);
    assert(S.length(s1) == 0);
    assert(S.to_list(s1) == []);
    assert(S.length(s2) == 0);
    assert(S.to_list(s2) == []);
  };

  let () = {
    let s1 = S.create();
    for (i in 1 to 4) {
      S.push(i, s1);
    };
    assert(S.length(s1) == 4);
    assert(S.to_list(s1) == [1, 2, 3, 4]);
    let s2 = S.copy(s1);
    assert(S.length(s1) == 4);
    assert(S.to_list(s1) == [1, 2, 3, 4]);
    assert(S.length(s2) == 4);
    assert(S.to_list(s2) == [1, 2, 3, 4]);
  };

  let () = print_endline("OK");
  ();
};
