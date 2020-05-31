open Ast;

module NameMap = Map.Make(String);
type environment = NameMap.t(primitiveType);

/* Unknown type,  resolved type. eg.[(T, TNum); (U, TBool)] */
type substitutions = list((id, primitiveType));

let type_variable = ref(Char.code('a'));

/* generates a new unknown type placeholder.
   returns T(string) of the generated alphabet */
let gen_new_type = () => {
  let c1 = type_variable^;
  incr(type_variable);
  T(Char.escaped(Char.chr(c1)));
};

let rec annotate_expr = (e: expr, env: environment): aexpr =>
  switch (e) {
  | NumLit(n) => ANumLit(n, TNum)
  | BoolLit(b) => ABoolLit(b, TBool)
  | Val(x) =>
    if (NameMap.mem(x, env)) {
      AVal(x, NameMap.find(x, env));
    } else {
      raise(failwith("variable not defined"));
    }
  | Binop(e1, op, e2) =>
    let et1 = annotate_expr(e1, env)
    and et2 = annotate_expr(e2, env)
    and new_type = gen_new_type();
    ABinop(et1, op, et2, new_type);
  | Fun(id, e) =>
    let ae = annotate_expr(e, env);
    let t = NameMap.find(id, env);
    AFun(id, ae, TFun(t, gen_new_type()));
  | App(fn, arg) =>
    let afn = annotate_expr(fn, env);
    let aarg = annotate_expr(arg, env);
    AApp(afn, aarg, gen_new_type());
  }

/* returns the type of an annotated expression */
and type_of = (ae: aexpr): primitiveType =>
  switch (ae) {
  | ANumLit(_, t)
  | ABoolLit(_, t) => t
  | AVal(_, t) => t
  | ABinop(_, _, _, t) => t
  | AFun(_, _, t) => t
  | AApp(_, _, t) => t
  };

let rec collect_expr = (ae: aexpr): list((primitiveType, primitiveType)) =>
  switch (ae) {
  | ANumLit(_)
  | ABoolLit(_) => [] /* no constraints to impose on literals */
  | AVal(_) => [] /* single occurence of val gives us no info */
  | ABinop(ae1, op, ae2, t) =>
    let et1 = type_of(ae1)
    and et2 = type_of(ae2);

    /* impose constraints based on binary operator */
    let opc =
      switch (op) {
      | Add
      | Mul => [(et1, TNum), (et2, TNum), (t, TNum)]
      /* we return et1, et2 since these are generic operators */
      | Gt
      | Lt => [(et1, et2), (t, TBool)]
      | And
      | Or => [(et1, TBool), (et2, TBool), (t, TBool)]
      };

    /* opc appended at the rightmost since we apply substitutions right to left */
    collect_expr(ae1) @ collect_expr(ae2) @ opc;
  | AFun(id, ae, t) =>
    switch (t) {
    | TFun(idt, ret_type) =>
      collect_expr(ae) @ [(type_of(ae), ret_type)]
    | _ => raise(failwith("not a function"))
    }

  /* 1. In application expressions, the first expression should be of TFun type or it
           could be a unknown type placeholder. Otherwise it's an error.
        2. Case 1: TFun(argt, ret_type)
           - In this case the parameter type of the function should be same as that of
             the argument passed in the function.
           - Second, the return type of the function, will be equal to the return type
             of the function application expression.
        3. Case 2: T(_)  (unknown type placeholder)
           - Since we do not know the type information of the first expression in an
             application expression, we cannot use the above approach.
           - But we do know that the first expression has to be a function. Also a function
             whose parameter type is same as that of argument type and that has a return type
             same as that of the entire expression.
           - Thus we use this information to impose a contraint on the unknown type placeholder.
     */
  | AApp(fn, arg, t) =>
    switch (type_of(fn)) {
    | TFun(argt, ret_type) =>
      collect_expr(fn)
      @ collect_expr(arg)
      @ [(t, ret_type), (argt, type_of(arg))]
    | T(_) =>
      collect_expr(fn)
      @ collect_expr(arg)
      @ [(type_of(fn), TFun(type_of(arg), t))]
    | _ => raise(failwith("incorrect function application"))
    }
  };

let rec substitute =
        (u: primitiveType, x: id, t: primitiveType): primitiveType =>
  switch (t) {
  | TNum
  | TBool => t
  | T(c) =>
    if (c == x) {
      u;
    } else {
      t;
    }
  | TFun(t1, t2) =>
    TFun(substitute(u, x, t1), substitute(u, x, t2))
  };

let apply = (subs: substitutions, t: primitiveType): primitiveType =>
  List.fold_right(((x, u), t) => substitute(u, x, t), subs, t);

let rec unify =
        (constraints: list((primitiveType, primitiveType))): substitutions =>
  switch (constraints) {
  | [] => []
  | [(x, y), ...xs] =>
    /* generate substitutions of the rest of the list */
    let t2 = unify(xs);
    /* resolve the LHS and RHS of the constraints from the previous substitutions */
    let t1 = unify_one(apply(t2, x), apply(t2, y));
    t1 @ t2;
  }

and unify_one = (t1: primitiveType, t2: primitiveType): substitutions =>
  switch (t1, t2) {
  | (TNum, TNum)
  | (TBool, TBool) => []
  | (T(x), z)
  | (z, T(x)) => [(x, z)]

  /* This case is particularly useful when you are calling a function that returns a function */
  | (TFun(a, b), TFun(x, y)) =>
    unify([(a, x), (b, y)])
  | _ => failwith("mismatched types")
  };

/* applies a final set of substitutions on the annotated expr */
let rec apply_expr = (subs: substitutions, ae: aexpr): aexpr =>
  switch (ae) {
  | ABoolLit(b, t) =>
    ABoolLit(b, apply(subs, t))
  | ANumLit(n, t) =>
    ANumLit(n, apply(subs, t))
  | AVal(s, t) =>
    AVal(s, apply(subs, t))
  | ABinop(e1, op, e2, t) =>
    ABinop(apply_expr(subs, e1), op, apply_expr(subs, e2), apply(subs, t))
  | AFun(id, e, t) =>
    AFun(id, apply_expr(subs, e), apply(subs, t))
  | AApp(fn, arg, t) =>
    AApp(apply_expr(subs, fn), apply_expr(subs, arg), apply(subs, t))
  };

let infer = (env: environment, e: expr): aexpr => {
  let annotated_expr = annotate_expr(e, env);
  let constraints = collect_expr(annotated_expr);
  let subs = unify(constraints);
  /* reset the type counter after completing inference */
  type_variable := Char.code('a');
  apply_expr(subs, annotated_expr);
};
