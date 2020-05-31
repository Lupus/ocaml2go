open Ast;

module NameMap = Map.Make(String);

let rec get_ids = (e: expr): list(id) => {
  let rec dedup =
    fun
    | [] => []
    | [x, y, ...xs] when x == y => [y, ...dedup(xs)]
    | [x, ...xs] => [x, ...dedup(xs)];
  let ids =
    switch (e) {
    | NumLit(_)
    | BoolLit(_) => []
    | Val(x) => [x]
    | Fun(x, y) => [x] @ get_ids(y)
    | Binop(e1, _, e2) => get_ids(e1) @ get_ids(e2)
    | App(fn, arg) => get_ids(fn) @ get_ids(arg)
    };
  dedup(ids);
};

let debug = (e: expr): string => {
  let ids = get_ids(e);
  let env =
    ListLabels.fold_left(~init=NameMap.empty, ids, ~f=(m, x) =>
      NameMap.add(x, Infer.gen_new_type(), m)
    );
  let aexpr = Infer.infer(env, e);
  string_of_type(Infer.type_of(aexpr));
};

let testcases = [|
  NumLit(10),
  BoolLit(true),
  Binop(NumLit(10), Add, NumLit(2)),
  Binop(BoolLit(true), Or, BoolLit(false)),
  Binop(Binop(Val("x"), Add, Val("y")), Mul, Val("z")),
  Binop(Binop(Val("x"), Add, Val("y")), Gt, Val("z")),
  Binop(Binop(Val("x"), Gt, Val("y")), Lt, Val("z")),
  Binop(
    Binop(Val("x"), Mul, Val("y")),
    Lt,
    Binop(Val("z"), Add, Val("w")),
  ),
  Fun("x", Binop(Val("x"), Add, NumLit(10))),
  Fun(
    "x",
    Binop(
      NumLit(20),
      Gt,
      Binop(Val("x"), Add, NumLit(10)),
    ),
  ),
  App(
    Fun("x", Binop(Val("x"), Add, NumLit(10))),
    NumLit(10),
  ),
  Fun(
    "f",
    Fun(
      "g",
      Fun(
        "x",
        App(Val("f"), App(Val("g"), Val("x"))),
      ),
    ),
  ),
|];

let check = (name, a, b) =>
  if (a != b) {
    failwith(
      Printf.sprintf("test `%s' failed, expected: %s, got: %s", name, a, b),
    );
  } else {
    Printf.printf("test `%s' passed!\n", name);
  };

let literals_check = () => {
  check("Integer", "int", debug(testcases[0]));
  check("Boolean", "bool", debug(testcases[1]));
};

let simple_expr_check = () => {
  check("Integer", "int", debug(testcases[2]));
  check("Boolean", "bool", debug(testcases[3]));
};

let var_expr_check = () => {
  check("x + y + z", "int", debug(testcases[4]));
  check("x + y > z", "bool", debug(testcases[5]));
  check("(x > y) < z", "bool", debug(testcases[6]));
  check(
    "(x * y) < (z + w)",
    "bool",
    debug(testcases[7]),
  );
};

let func_decl_check = () => {
  check(
    "fun x -> x + 10",
    "(int -> int)",
    debug(testcases[8]),
  );
  check(
    "fun x -> (20 > (x + 10))",
    "(int -> bool)",
    debug(testcases[9]),
  );
};

let func_appl_check = () => {
  check(
    "(fun x -> x + 10) 10",
    "int",
    debug(testcases[10]),
  );
  check(
    "compose function",
    "(('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))",
    debug(testcases[11]),
  );
};

let parse = s => {
  let lexbuf = Sedlex_menhir.create_lexbuf(Sedlexing.Utf8.from_string(s));
  Sedlex_menhir.sedlex_with_menhir(Lexer.token, Parser.main, lexbuf);
};

let parser_check = () => {
  let src = "(fun x -> (fun y -> x + y))";
  check(
    src,
    "(int -> (int -> int))",
    src |> parse |> debug,
  );
};

let () = {
  literals_check();
  simple_expr_check();
  var_expr_check();
  func_decl_check();
  func_appl_check();
  parser_check();
  exit(0);
};
