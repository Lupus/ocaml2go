/* adapted from https://github.com/ocaml/ocaml/blob/4.08.1/testsuite/tests/lib-list/test.ml */
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


let string_of_even_opt = x =>
  if (x mod 2 == 0) {
    Some(string_of_int(x));
  } else {
    None;
  };

let run = () => {
  /* Standard test case */
  let () = {
    let l = List.init(10, x => x);
    assert(List.exists(a => a < 10, l));
    assert(List.exists(a => a > 0, l));
    assert(List.exists(a => a == 0, l));
    assert(List.exists(a => a == 1, l));
    assert(List.exists(a => a == 2, l));
    assert(List.exists(a => a == 3, l));
    assert(List.exists(a => a == 4, l));
    assert(List.exists(a => a == 5, l));
    assert(List.exists(a => a == 6, l));
    assert(List.exists(a => a == 7, l));
    assert(List.exists(a => a == 8, l));
    assert(List.exists(a => a == 9, l));
    assert(!List.exists(a => a < 0, l));
    assert(!List.exists(a => a > 9, l));
    assert(List.exists(_ => true, l));

    assert(List.compare_lengths([], []) == 0);
    assert(List.compare_lengths([1, 2], ['a', 'b']) == 0);
    assert(List.compare_lengths([], [1, 2]) < 0);
    assert(List.compare_lengths(['a'], [1, 2]) < 0);
    assert(List.compare_lengths([1, 2], []) > 0);
    assert(List.compare_lengths([1, 2], ['a']) > 0);

    assert(List.compare_length_with([], 0) == 0);
    assert(List.compare_length_with([], 1) < 0);
    assert(List.compare_length_with([], -1) > 0);
    assert(List.compare_length_with([], max_int) < 0);
    assert(List.compare_length_with([], min_int) > 0);
    assert(List.compare_length_with([1], 0) > 0);
    assert(List.compare_length_with(['1'], 1) == 0);
    assert(List.compare_length_with(['1'], 2) < 0);
    assert(
      List.filter_map(string_of_even_opt, l) == ["0", "2", "4", "6", "8"],
    );
    ();
  };

  /* Empty test case */
  let () = assert(List.init(0, x => x) == []);

  /* Erroneous test case */

  let () = {
    let result =
      try({
        let _ = List.init(-1, x => x);
        false;
      }) {
      | Invalid_argument(e) => true
      }; /* Exception caught */
    assert(result);
  };

  /* Evaluation order */
  let () = {
    let test = n => {
      let result = ref(false);
      let _ = List.init(n, x => result := x == n - 1);
      assert(result^);
    };

    /* Threshold must equal the value in stdlib/list.ml */
    let threshold = 10_000;
    test(threshold); /* Non tail-recursive case */
    test(threshold + 1);
  }; /* Tail-recursive case */

  print_endline("OK");
};
