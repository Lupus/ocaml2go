/*
 * Code example by Mikhail Mandrykin (schrodibear)
 * https://discuss.ocaml.org/t/how-would-i-implement-a-polymorphic-recursive-modules-for-generic-types/1808/9
 */

type ty(_) = ..;
type poly_val('a) = {value: 'a};
type ty(_) +=
  | Int: ty(int)
  | String: ty(string)
  | Poly_val(ty('a)): ty(poly_val('a));
type show = {f: 'a. (ty('a), 'a) => string};
let funs = ref([]);
let register = f => funs := [f, ...funs^];
let show = (t, x) => {
  let rec try_all =
    fun
    | [] => raise(Not_found)
    | [{f}, ...fs] =>
      try(f(t, x)) {
      | Not_found => try_all(fs)
      };
  try_all(funs^);
};

let show_int: type a. (ty(a), a) => string =
  (t, x) =>
    switch (t) {
    | Int => "int(" ++ string_of_int(x) ++ ")"
    | _ => raise(Not_found)
    };
let show_string: type a. (ty(a), a) => string =
  (t, x) =>
    switch (t) {
    | String => "string(" ++ x ++ ")"
    | _ => raise(Not_found)
    };
let show_poly_val: type a. (ty(a), a) => string =
  (t, x) =>
    switch (t) {
    | Poly_val(t) => "poly_val(value=" ++ show(t, x.value) ++ ")"
    | _ => raise(Not_found)
    };
let () = {
  register({f: show_int});
  register({f: show_string});
  register({f: show_poly_val});
};

let p = (t, x) => print_endline(show(t, x));
let () = {
  p(Poly_val(Int), {value: 5});
  p(Poly_val(String), {value: "line1"});
  p(Poly_val(Poly_val(String)), {
                                    value: {
                                      value: "line2",
                                    },
                                  });
};

/*
 let jane = {as _; pub name = "Jane"; pub home = "01234−654321"};
 let john = {as _; pub name = "John"; pub mobile = "07654−123456"};
 let print_name = r => print_endline("Name: " ++ r#name);
 let () = {
   print_name(jane);
   print_name(john);
 };
 */


/* 
 * Polymorphic variants sample code from
 * https://2ality.com/2018/01/polymorphic-variants-reasonml.html
 */

module MyModule = {
  type color =
    | Black
    | White
    | Other(string);

  let getNameNormal = (c: color) =>
    switch (c) {
    | Black => "Black"
    | White => "White"
    | Other(s) => s
    };
};

print_endline(MyModule.getNameNormal(MyModule.Black));

module MyModule2 = {
  type color = [ | `Black | `White | `Other(string)];

  let getNamePoly = (c: color) =>
    switch (c) {
    | `Black => "Black"
    | `White => "White"
    | `Other(s) => s
    };
};

print_endline(MyModule2.getNamePoly(`Black));

/* 
 * Code examples from
 * https://caml.inria.fr/pub/docs/manual-ocaml/polymorphism.html#s:polymorphic-recursion
 */

module A = {
  type regular_nested('a) =
    | List(list('a))
    | Nested(list(regular_nested('a)));
  let l =
    Nested([List([1]), Nested([List([2, 3])]), Nested([Nested([])])]);

  let rec maximal_depth =
    fun
    | List(_) => 1
    | Nested([]) => 0
    | Nested([a, ...q]) =>
      1 + max(maximal_depth(a), maximal_depth(Nested(q)));
};

print_endline("maximal_depth: " ++ string_of_int(A.maximal_depth(A.l)));

module B = {
  type nested('a) =
    | List(list('a))
    | Nested(nested(list('a)));

  let rec depth: 'a. nested('a) => int =
    fun
    | List(_) => 1
    | Nested(n) => 1 + depth(n);

  let l1 = Nested(List([[7], [8]]));

  let len = nested => {
    let map_and_sum = f => List.fold_left((acc, x) => acc + f(x), 0);
    let rec len: 'a. (list('a) => int, nested('a)) => int =
      (nested_len, n) =>
        switch (n) {
        | List(l) => nested_len(l)
        | Nested(n) => len(map_and_sum(nested_len), n)
        };

    len(List.length, nested);
  };

  let l2 =
    Nested(
      Nested(List([[[1, 2], [3]], [[], [4], [5, 6, 7]], [[]]])),
    );

  let rec loop = n =>
    if (n > 0) {
      let l1 = Nested(List([[n + 7], [n + 8]]));
      print_endline("depth: " ++ string_of_int(depth(l1)));
      let l2 =
        Nested(
          Nested(
            List([
              [[n + 1, n + 2], [n + 3]],
              [[], [n + 4], [n + 5, n + 6, n + 7]],
              [[]],
            ]),
          ),
        );
      print_endline("len: " ++ string_of_int(len(l2)));
      loop(n - 1);
    };
};

print_endline("depth: " ++ string_of_int(B.depth(B.l1)));
print_endline("len: " ++ string_of_int(B.len(B.l2)));

B.loop(10);
