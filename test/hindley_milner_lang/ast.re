type id = string;

type op =
  | Add
  | Mul
  | Gt
  | Lt
  | And
  | Or;

module CharMap = Map.Make(String);

type genericMap = CharMap.t(int);

type primitiveType =
  | TNum
  | TBool
  | T(string)
  | TFun(primitiveType, primitiveType);

type expr =
  | NumLit(int)
  | BoolLit(bool)
  | Val(string)
  | Binop(expr, op, expr)
  | Fun(id, expr)
  | App(expr, expr);

/* annotated expr -> expr with types */
type aexpr =
  | ANumLit(int, primitiveType)
  | ABoolLit(bool, primitiveType)
  | AVal(string, primitiveType)
  | ABinop(aexpr, op, aexpr, primitiveType)
  | AFun(id, aexpr, primitiveType)
  | AApp(aexpr, aexpr, primitiveType);

let string_of_op = (op: op) =>
  switch (op) {
  | Add => "+"
  | Mul => "*"
  | Lt => "<"
  | Gt => ">"
  | Or => "||"
  | And => "&&"
  };

let string_of_type = (t: primitiveType) => {
  let rec aux = (t: primitiveType, chr: int, map: genericMap) =>
    switch (t) {
    | TNum => ("int", chr, map)
    | TBool => ("bool", chr, map)
    | T(x) =>
      let (gen_chr, new_chr, new_map) =
        if (CharMap.mem(x, map)) {
          (Char.escaped(Char.chr(CharMap.find(x, map))), chr, map);
        } else {
          let c = Char.escaped(Char.chr(chr));
          (c, chr + 1, CharMap.add(x, chr, map));
        };

      (Printf.sprintf("'%s", gen_chr), new_chr, new_map);
    | TFun(t1, t2) =>
      let (st1, c1, m1) = aux(t1, chr, map);
      let (st2, c2, m2) = aux(t2, c1, m1);
      (Printf.sprintf("(%s -> %s)", st1, st2), c2, m2);
    };
  let (s, _, _) = aux(t, 97, CharMap.empty);
  s;
};

let rec string_of_aexpr = (ae: aexpr): string =>
  switch (ae) {
  | ANumLit(x, t) =>
    Printf.sprintf("(%s: %s)", string_of_int(x), string_of_type(t))
  | ABoolLit(b, t) =>
    Printf.sprintf("(%s: %s)", string_of_bool(b), string_of_type(t))
  | AVal(x, t) =>
    Printf.sprintf("(%s: %s)", x, string_of_type(t))
  | ABinop(e1, op, e2, t) =>
    let s1 = string_of_aexpr(e1);
    let s2 = string_of_aexpr(e2);
    let sop = string_of_op(op);
    let st = string_of_type(t);
    Printf.sprintf("(%s %s %s: %s)", s1, sop, s2, st);
  | AFun(id, ae, t) =>
    let s1 = string_of_aexpr(ae);
    let st = string_of_type(t);
    Printf.sprintf("(fun %s -> %s): %s", id, s1, st);
  | AApp(e1, e2, t) =>
    let s1 = string_of_aexpr(e1)
    and s2 = string_of_aexpr(e2)
    and st = string_of_type(t);
    Printf.sprintf("(%s %s): %s", s1, s2, st);
  };

let rec string_of_expr = (e: expr): string =>
  switch (e) {
  | NumLit(x) => string_of_int(x)
  | BoolLit(b) => string_of_bool(b)
  | Val(s) => s
  | Binop(e1, op, e2) =>
    let s1 = string_of_expr(e1)
    and s2 = string_of_expr(e2);
    let sop = string_of_op(op);
    Printf.sprintf("(%s %s %s)", s1, sop, s2);
  | Fun(id, e) =>
    let s1 = string_of_expr(e);
    Printf.sprintf("(fun %s -> %s)", id, s1);
  | App(e1, e2) =>
    let s1 = string_of_expr(e1)
    and s2 = string_of_expr(e2);
    Printf.sprintf("(%s %s)", s1, s2);
  };
