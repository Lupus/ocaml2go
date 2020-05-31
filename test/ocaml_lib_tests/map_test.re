/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/4.08.1/testsuite/tests/lib-set/testmap.ml */
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


module M =
  Map.Make({
    type t = int;
    let compare = (x: t, y) => compare(x, y);
  });

let img = (x, m) =>
  try(Some(M.find(x, m))) {
  | Not_found => None
  };

let testvals = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

let check = (msg, cond) =>
  if (!List.for_all(cond, testvals)) {
    Printf.printf("Test %s FAILED\n%!", msg);
  };

let checkbool = (msg, b) =>
  if (!b) {
    Printf.printf("Test %s FAILED\n%!", msg);
  };

let uncurry = (f: ('a, 'b) => 'c, (x, y): ('a, 'b)): 'c => f(x, y);

let test = (x, v, s1, s2) => {
  checkbool(
    "is_empty",
    M.is_empty(s1) == List.for_all(i => img(i, s1) == None, testvals),
  );

  check("mem", i => M.mem(i, s1) == (img(i, s1) != None));

  check(
    "add",
    {
      let s = M.add(x, v, s1);
      i =>
        img(i, s)
        == (
             if (i == x) {
               Some(v);
             } else {
               img(i, s1);
             }
           );
    },
  );

  check(
    "singleton",
    {
      let s = M.singleton(x, v);
      i =>
        img(i, s)
        == (
             if (i == x) {
               Some(v);
             } else {
               None;
             }
           );
    },
  );

  check(
    "remove",
    {
      let s = M.remove(x, s1);
      i =>
        img(i, s)
        == (
             if (i == x) {
               None;
             } else {
               img(i, s1);
             }
           );
    },
  );

  check(
    "merge-union",
    {
      let f = (_, o1, o2) =>
        switch (o1, o2) {
        | (Some(v1), Some(v2)) => Some(v1 +. v2)
        | (None, _) => o2
        | (_, None) => o1
        };
      let s = M.merge(f, s1, s2);
      i => img(i, s) == f(i, img(i, s1), img(i, s2));
    },
  );

  check(
    "merge-inter",
    {
      let f = (_, o1, o2) =>
        switch (o1, o2) {
        | (Some(v1), Some(v2)) => Some(v1 -. v2)
        | (_, _) => None
        };
      let s = M.merge(f, s1, s2);
      i => img(i, s) == f(i, img(i, s1), img(i, s2));
    },
  );

  checkbool(
    "bindings",
    {
      let rec extract =
        fun
        | [] => []
        | [hd, ...tl] =>
          switch (img(hd, s1)) {
          | None => extract(tl)
          | Some(v) => [(hd, v), ...extract(tl)]
          };
      M.bindings(s1) == extract(testvals);
    },
  );

  checkbool(
    "for_all",
    {
      let p = (x, y) => x mod 2 == 0;
      M.for_all(p, s1) == List.for_all(uncurry(p), M.bindings(s1));
    },
  );

  checkbool(
    "exists",
    {
      let p = (x, y) => x mod 3 == 0;
      M.exists(p, s1) == List.exists(uncurry(p), M.bindings(s1));
    },
  );

  checkbool(
    "filter",
    {
      let p = (x, y) => x >= 3 && x <= 6;
      M.bindings(M.filter(p, s1))
      == List.filter(uncurry(p), M.bindings(s1));
    },
  );

  checkbool(
    "partition",
    {
      let p = (x, y) => x >= 3 && x <= 6;
      let (st, sf) = M.partition(p, s1)
      and (lt, lf) = List.partition(uncurry(p), M.bindings(s1));
      M.bindings(st) == lt && M.bindings(sf) == lf;
    },
  );

  checkbool("cardinal", M.cardinal(s1) == List.length(M.bindings(s1)));

  checkbool(
    "min_binding",
    try({
      let (k, v) = M.min_binding(s1);
      img(k, s1) == Some(v) && M.for_all((i, _) => k <= i, s1);
    }) {
    | Not_found => M.is_empty(s1)
    },
  );

  checkbool(
    "max_binding",
    try({
      let (k, v) = M.max_binding(s1);
      img(k, s1) == Some(v) && M.for_all((i, _) => k >= i, s1);
    }) {
    | Not_found => M.is_empty(s1)
    },
  );

  checkbool(
    "choose",
    try({
      let (x, v) = M.choose(s1);
      img(x, s1) == Some(v);
    }) {
    | Not_found => M.is_empty(s1)
    },
  );

  checkbool(
    "find_first",
    {
      let (l, p, r) = M.split(x, s1);
      if (p == None && M.is_empty(r)) {
        try({
          let _ = M.find_first(k => k >= x, s1);
          false;
        }) {
        | Not_found => true
        };
      } else {
        let (k, v) = M.find_first(k => k >= x, s1);
        switch (p) {
        | None => (k, v) == M.min_binding(r)
        | Some(v1) => (k, v) == (x, v1)
        };
      };
    },
  );

  checkbool(
    "find_first_opt",
    {
      let (l, p, r) = M.split(x, s1);
      let find_first_opt_result = M.find_first_opt(k => k >= x, s1);
      if (p == None && M.is_empty(r)) {
        switch (find_first_opt_result) {
        | None => true
        | _ => false
        };
      } else {
        switch (find_first_opt_result) {
        | None => false
        | Some((k, v)) =>
          switch (p) {
          | None => (k, v) == M.min_binding(r)
          | Some(v1) => (k, v) == (x, v1)
          }
        };
      };
    },
  );

  checkbool(
    "find_last",
    {
      let (l, p, r) = M.split(x, s1);
      if (p == None && M.is_empty(l)) {
        try({
          let _ = M.find_last(k => k <= x, s1);
          false;
        }) {
        | Not_found => true
        };
      } else {
        let (k, v) = M.find_last(k => k <= x, s1);
        switch (p) {
        | None => (k, v) == M.max_binding(l)
        | Some(v1) => (k, v) == (x, v1)
        };
      };
    },
  );

  checkbool(
    "find_last_opt",
    {
      let (l, p, r) = M.split(x, s1);
      let find_last_opt_result = M.find_last_opt(k => k <= x, s1);
      if (p == None && M.is_empty(l)) {
        switch (find_last_opt_result) {
        | None => true
        | _ => false
        };
      } else {
        switch (find_last_opt_result) {
        | None => false
        | Some((k, v)) =>
          switch (p) {
          | None => (k, v) == M.max_binding(l)
          | Some(v1) => (k, v) == (x, v1)
          }
        };
      };
    },
  );

  check(
    "split",
    {
      let (l, p, r) = M.split(x, s1);
      i =>
        if (i < x) {
          img(i, l) == img(i, s1);
        } else if (i > x) {
          img(i, r) == img(i, s1);
        } else {
          p == img(i, s1);
        };
    },
  );

  checkbool("to_seq_of_seq", M.equal((==), s1, M.of_seq @@ M.to_seq(s1)));

  checkbool(
    "to_seq_from",
    {
      let seq = M.to_seq_from(x, s1);
      let ok1 = List.of_seq(seq) |> List.for_all(((y, _)) => y >= x);
      let ok2 =
        M.to_seq(s1)
        |> List.of_seq
        |> List.filter(((y, _)) => y >= x) == List.of_seq(seq);

      ok1 && ok2;
    },
  );

  ();
};

let rkey = () => Random.int(10);

let rdata = () => Random.float(1.0);

let rmap = () => {
  let s = ref(M.empty);
  for (i in 1 to Random.int(10)) {
    s := M.add(rkey(), rdata(), s^);
  };
  s^;
};

let run = () => {
  let _ = {
    Random.init(42);
    for (i in 1 to 10000) {
      test(rkey(), rdata(), rmap(), rmap());
    };
  };

  let () = {
    /* check that removing a binding from a map that is not present in this map
       (1) doesn't allocate and (2) return the original map */
    let m1 = ref(M.empty);
    for (i in 1 to 10) {
      m1 := M.add(i, float(i), m1^);
    };
    let m2 = ref(m1^);

    //let a0 = Gc.allocated_bytes();
    //let a1 = Gc.allocated_bytes();
    for (i in 11 to 30) {
      m2 := M.remove(i, m2^);
    };
    //let a2 = Gc.allocated_bytes();

    assert(m2^ === m1^);
    //assert(a2 -. a1 == a1 -. a0);
  };

  let () = {
    /* check that filtering a map where all bindings are satisfied by
       the given predicate returns the original map */
    let m1 = ref(M.empty);
    for (i in 1 to 10) {
      m1 := M.add(i, float(i), m1^);
    };
    let m2 = M.filter((e, _) => e >= 0, m1^);
    assert(m2 === m1^);
  };

  let () = {
    /* check that adding a binding "x -> y" to a map that already
       contains it doesn't allocate and return the original map. */
    let m1 = ref(M.empty);
    let tmp = ref(None);
    for (i in 1 to 10) {
      tmp := Some(float(i));
      m1 := M.add(i, tmp^, m1^);
    };
    let m2 = ref(m1^);

    //let a0 = Gc.allocated_bytes();
    //let a1 = Gc.allocated_bytes();

    /* 10 |-> !tmp is already present in !m2 */
    m2 := M.add(10, tmp^, m2^);

    //let a2 = Gc.allocated_bytes();

    assert(m2^ === m1^);
    //assert(a2 -. a1 == a1 -. a0);

    /* 4 |-> Some 84. is not present in !m2 */
    m2 := M.add(4, Some(84.), m2^);

    assert(!(m2^ === m1^));
  };
  ();
};
