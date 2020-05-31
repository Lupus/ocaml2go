/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/trunk/testsuite/tests/lib-set/testset.ml */
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


module S =
  Set.Make({
    type t = int;
    let compare = (x: t, y) => compare(x, y);
  });

let testvals = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

let check = (msg, cond) =>
  if (!List.for_all(cond, testvals)) {
    Printf.printf("Test %s FAILED\n%!", msg);
  };

let checkbool = (msg, b) =>
  if (!b) {
    Printf.printf("Test %s FAILED\n%!", msg);
  };

let normalize_cmp = c =>
  if (c == 0) {
    0;
  } else if (c > 0) {
    1;
  } else {
    (-1);
  };

let test = (x, s1, s2) => {
  checkbool(
    "is_empty",
    S.is_empty(s1) == List.for_all(i => !S.mem(i, s1), testvals),
  );

  check(
    "add",
    {
      let s = S.add(x, s1);
      i => S.mem(i, s) == (S.mem(i, s1) || i == x);
    },
  );

  check(
    "singleton",
    {
      let s = S.singleton(x);
      i => S.mem(i, s) == (i == x);
    },
  );

  check(
    "remove",
    {
      let s = S.remove(x, s1);
      i => S.mem(i, s) == (S.mem(i, s1) && i != x);
    },
  );

  check(
    "union",
    {
      let s = S.union(s1, s2);
      i => S.mem(i, s) == (S.mem(i, s1) || S.mem(i, s2));
    },
  );

  check(
    "inter",
    {
      let s = S.inter(s1, s2);
      i => S.mem(i, s) == (S.mem(i, s1) && S.mem(i, s2));
    },
  );

  checkbool("disjoint", S.is_empty(S.inter(s1, s2)) == S.disjoint(s1, s2));

  check(
    "diff",
    {
      let s = S.diff(s1, s2);
      i => S.mem(i, s) == (S.mem(i, s1) && !S.mem(i, s2));
    },
  );

  checkbool(
    "elements",
    S.elements(s1) == List.filter(i => S.mem(i, s1), testvals),
  );

  checkbool(
    "compare",
    normalize_cmp(S.compare(s1, s2))
    == normalize_cmp(compare(S.elements(s1), S.elements(s2))),
  );

  checkbool(
    "equal",
    S.equal(s1, s2) == (S.elements(s1) == S.elements(s2)),
  );

  check(
    "subset",
    {
      let b = S.subset(s1, s2);
      i =>
        if (b && S.mem(i, s1)) {
          S.mem(i, s2);
        } else {
          true;
        };
    },
  );

  checkbool(
    "subset2",
    {
      let b = S.subset(s1, s2);
      b || !S.is_empty(S.diff(s1, s2));
    },
  );

  checkbool(
    "map",
    S.elements(S.map(succ, s1)) == List.map(succ, S.elements(s1)),
  );

  checkbool("map2", S.map(x => x, s1) === s1);

  checkbool(
    "map3",
    {
      /* check that the traversal is made in increasing element order */
      let last = ref(min_int);
      S.map(
        x => {
          assert(last^ <= x);
          last := x;
          x;
        },
        s1,
      )
      === s1;
    },
  );

  checkbool(
    "for_all",
    {
      let p = x => x mod 2 == 0;
      S.for_all(p, s1) == List.for_all(p, S.elements(s1));
    },
  );

  checkbool(
    "exists",
    {
      let p = x => x mod 3 == 0;
      S.exists(p, s1) == List.exists(p, S.elements(s1));
    },
  );

  checkbool(
    "filter",
    {
      let p = x => x >= 3 && x <= 6;
      S.elements(S.filter(p, s1)) == List.filter(p, S.elements(s1));
    },
  );

  checkbool(
    "partition",
    {
      let p = x => x >= 3 && x <= 6;
      let (st, sf) = S.partition(p, s1)
      and (lt, lf) = List.partition(p, S.elements(s1));
      S.elements(st) == lt && S.elements(sf) == lf;
    },
  );

  checkbool("cardinal", S.cardinal(s1) == List.length(S.elements(s1)));

  checkbool(
    "min_elt",
    try({
      let m = S.min_elt(s1);
      S.mem(m, s1) && S.for_all(i => m <= i, s1);
    }) {
    | Not_found => S.is_empty(s1)
    },
  );

  checkbool(
    "max_elt",
    try({
      let m = S.max_elt(s1);
      S.mem(m, s1) && S.for_all(i => m >= i, s1);
    }) {
    | Not_found => S.is_empty(s1)
    },
  );

  checkbool(
    "choose",
    try({
      let x = S.choose(s1);
      S.mem(x, s1);
    }) {
    | Not_found => S.is_empty(s1)
    },
  );

  checkbool(
    "find_first",
    {
      let (l, p, r) = S.split(x, s1);
      if (!p && S.is_empty(r)) {
        try({
          let _ = S.find_first(k => k >= x, s1);
          false;
        }) {
        | Not_found => true
        };
      } else {
        let e = S.find_first(k => k >= x, s1);
        if (p) {
          e == x;
        } else {
          e == S.min_elt(r);
        };
      };
    },
  );

  checkbool(
    "find_first_opt",
    {
      let (l, p, r) = S.split(x, s1);
      let find_first_opt_result = S.find_first_opt(k => k >= x, s1);
      if (!p && S.is_empty(r)) {
        switch (find_first_opt_result) {
        | None => true
        | _ => false
        };
      } else {
        switch (find_first_opt_result) {
        | None => false
        | Some(e) =>
          if (p) {
            e == x;
          } else {
            e == S.min_elt(r);
          }
        };
      };
    },
  );

  checkbool(
    "find_last",
    {
      let (l, p, r) = S.split(x, s1);
      if (!p && S.is_empty(l)) {
        try({
          let _ = S.find_last(k => k <= x, s1);
          false;
        }) {
        | Not_found => true
        };
      } else {
        let e = S.find_last(k => k <= x, s1);
        if (p) {
          e == x;
        } else {
          e == S.max_elt(l);
        };
      };
    },
  );

  checkbool(
    "find_last_opt",
    {
      let (l, p, r) = S.split(x, s1);
      let find_last_opt_result = S.find_last_opt(k => k <= x, s1);
      if (!p && S.is_empty(l)) {
        switch (find_last_opt_result) {
        | None => true
        | _ => false
        };
      } else {
        switch (find_last_opt_result) {
        | None => false
        | Some(e) =>
          if (p) {
            e == x;
          } else {
            e == S.max_elt(l);
          }
        };
      };
    },
  );

  check(
    "split",
    {
      let (l, p, r) = S.split(x, s1);
      i =>
        if (i < x) {
          S.mem(i, l) == S.mem(i, s1);
        } else if (i > x) {
          S.mem(i, r) == S.mem(i, s1);
        } else {
          p == S.mem(i, s1);
        };
    },
  );

  checkbool("to_seq_of_seq", S.equal(s1, S.of_seq @@ S.to_seq(s1)));

  checkbool(
    "to_seq_from",
    {
      let seq = S.to_seq_from(x, s1);
      let ok1 = List.of_seq(seq) |> List.for_all(y => y >= x);
      let ok2 =
        S.elements(s1) |> List.filter(y => y >= x) == List.of_seq(seq);

      ok1 && ok2;
    },
  );

  ();
};

let relt = () => Random.int(10);

let rset = () => {
  let s = ref(S.empty);
  for (i in 1 to Random.int(10)) {
    s := S.add(relt(), s^);
  };
  s^;
};

let run = () => {
  let _ = {
    Random.init(42);
    for (i in 1 to 10000) {
      test(relt(), rset(), rset());
    };
  };

  let () = {
    /* #6645: check that adding an element to set that already contains
       it doesn't allocate and return the original set. */
    let s1 = ref(S.empty);
    for (i in 1 to 10) {
      s1 := S.add(i, s1^);
    };
    let s2 = ref(s1^);

    //let a0 = Gc.allocated_bytes();
    //let a1 = Gc.allocated_bytes();
    for (i in 1 to 10) {
      s2 := S.add(i, s2^);
    };
    //let a2 = Gc.allocated_bytes();

    assert(s2^ === s1^);
    //assert(a2 -. a1 == a1 -. a0);
  };

  let () = {
    /* check that removing an element from a set that is not present in this set
       (1) doesn't allocate and (2) return the original set */
    let s1 = ref(S.empty);
    for (i in 1 to 10) {
      s1 := S.add(i, s1^);
    };
    let s2 = ref(s1^);

    //let a0 = Gc.allocated_bytes();
    //let a1 = Gc.allocated_bytes();
    for (i in 11 to 30) {
      s2 := S.remove(i, s2^);
    };
    //let a2 = Gc.allocated_bytes();

    assert(s2^ === s1^);
    //assert(a2 -. a1 == a1 -. a0);
  };

  let () = {
    /* check that filtering a set where all elements are satisfied by
       the given predicate return the original set */
    let s1 = ref(S.empty);
    for (i in 1 to 10) {
      s1 := S.add(i, s1^);
    };
    let s2 = S.filter(e => e >= 0, s1^);
    assert(s2 === s1^);
  };

  let valid_structure = s =>
    /* this test should return 'true' for all set,
          but it can detect sets that are ill-structured,
          for example incorrectly ordered, as the S.mem
          function will make assumptions about the set ordering.

          (This trick was used to exhibit the bug in PR#7403)
       */
    List.for_all(n => S.mem(n, s), S.elements(s));

  let () = {
    /* PR#7403: map buggily orders elements according to the input
       set order, not the output set order. Mapping functions that
       change the value ordering thus break the set structure. */
    let test = S.of_list([1, 3, 5]);
    let f =
      fun
      | 3 => 8
      | n => n;
    assert(valid_structure(S.map(f, test)));
  };
  ();
};
