/*
 MIT License

 Copyright (c) 2020 Konstantin Olkhovskiy <lupus@oxnull.net>

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
 */
open Go;

let tval = TRef("V");

let ta_bool = primexpr => {
  TAssert(Bool, primexpr);
};

let ta_arr = primexpr => {
  TAssert(TSlice(tval), primexpr);
};

let ta_map = primexpr => {
  TAssert(TRef("Vmap"), primexpr);
};

let ta_int = primexpr => {
  TAssert(Int, primexpr);
};

let ta_expr = (ta, e) => Unary(Primary(Operand(Parens(e)) |> ta));

let fn_typ = nargs => {
  let args = List.init(nargs, _ => TRef("V"));
  FuncTyp(args, TRef("V"));
};

let ta_func = (primexpr, nargs) => {
  TAssert(fn_typ(nargs), primexpr);
};

let mk_typ_closure_expr = (arg_names, typ, body) =>
  Unary(
    Primary(
      Closure(
        Some(List.map(arg_names, ~f=name => ([name], tval))),
        Some(typ),
        body,
      ),
    ),
  );

let mk_unit_closure_expr = (arg_names, body) =>
  Unary(
    Primary(
      Closure(
        Some(List.map(arg_names, ~f=name => ([name], tval))),
        None,
        body,
      ),
    ),
  );

class hoist_requires = {
  as self;
  inherit class Traverse_builtins.fold_map(list((string, string)));
  inherit class IR.fold_map(list((string, string))) as super;
  pub statement = (s, acc) => {
    switch (s) {
    | IR.SAssignment({
        sassign_lvalue: LVar(as_),
        sassign_expr: ERequire(what),
      }) => (
        IR.SEmpty,
        [(what, as_), ...acc],
      )
    | IR.SAssignment({
        sassign_lvalue: LVar("runtime" as as_),
        sassign_expr: ERuntime,
      }) => (
        IR.SEmpty,
        [("caml_runtime", as_), ...acc],
      )
    | _ => super#statement(s, acc)
    };
  };
  pub program = ({prg_locals, prg_elements}, acc) => {
    let (prg_elements, acc) = self#source_elements(prg_elements, acc);
    let imports =
      List.map(acc, ~f=((_, as_)) => as_) |> Set.of_list((module String));
    let prg_locals = List.filter(~f=id => !Set.mem(imports, id), prg_locals);
    ({prg_locals, prg_elements}, acc);
  };
};

let declare_locals = (locals: IR.locals) => {
  switch (locals) {
  | [] => EmptyStmt(N)
  | _ =>
    Decl(
      N,
      VarDecls(List.map(locals, ~f=id => VarSpecTyp(([id], tval), None))),
    )
  };
};

let mark_vars_as_used = vars =>
  Simple(
    N,
    Expr(
      Unary(
        Primary(
          FunApp(
            Operand(Var("Mark_as_used")),
            List.map(vars, ~f=p => Unary(Primary(Operand(Var(p))))),
          ),
        ),
      ),
    ),
  );

let rec from_unop = unop =>
  switch (unop) {
  | IR.UNot => "Unop_not__"
  | UIsInt => "Unop_is_int__"
  | UToInt => "Unop_to_int__"
  | UToBool => "Unop_to_bool__"
  | UIntToString => "Unop_int_to_string__"
  | UFloatToInt => "Unop_float_to_int__"
  | UIntToFloat => "Unop_int_to_float__"
  | UNeg => "Unop_neg__"
  | UFloatNeg => "Unop_float_neg__"
  | UBnot => "Unop_bnot__"
  }
and from_binop =
  fun
  | IR.BEqEqEq => "Binop_eq_eq_eq__"
  | BOr
  | BAnd => failwith("BOr and BAnd must be treated separately")
  | BBor => "Binop_bor__"
  | BBxor => "Binop_bxor__"
  | BBand => "Binop_band__"
  | BNotEqEq => "Binop_not_eq_eq__"
  | BLt => "Binop_lt__"
  | BLe => "Binop_le__"
  | BGt => "Binop_gt__"
  | BGe => "Binop_ge__"
  | BFloatLt => "Binop_float_lt__"
  | BFloatLe => "Binop_float_le__"
  | BFloatGt => "Binop_float_gt__"
  | BFloatGe => "Binop_float_ge__"
  | BLsl => "Binop_lsl__"
  | BLsr => "Binop_lsr__"
  | BAsr => "Binop_asr__"
  | BPlus => "Binop_plus__"
  | BFloatPlus => "Binop_float_plus__"
  | BIntPlus => "Binop_int_plus__"
  | BMinus => "Binop_minus__"
  | BMul => "Binop_mul__"
  | BDiv => "Binop_div__"
  | BMod => "Binop_mod__"
  | BFloatMinus => "Binop_float_minus__"
  | BFloatMul => "Binop_float_mul__"
  | BFloatDiv => "Binop_float_div__"
  | BFloatMod => "Binop_float_mod__"
  | BEqEq => "Binop_eq_eq__"
  | BNotEq => "Binop_not_eq__"
  | BFloatEqEq => "Binop_float_eq_eq__"
  | BFloatNotEq => "Binop_float_not_eq__"
and from_variable_declaration =
  fun
  | (ident, None) => (ident, None)
  | (ident, Some((init_expr, init_loc))) => (
      ident,
      Some((from_expression(init_expr), init_loc)),
    )
and from_arguments = args => List.map(~f=from_expression, args)
and from_expression = (e: IR.expression): Go.expr =>
  switch (e) {
  | IR.ECall({ecall_expr: e, ecall_args: args}) =>
    Unary(
      Primary(
        FunApp(
          from_expression(e)
          |> (
            fun
            | Unary(Primary(Sel(Operand(Var("runtime")), _) as pe)) => pe
            | Unary(Primary(pe)) => ta_func(pe, List.length(args))
            | _ => failwith("unexpected expression for function")
          ),
          from_arguments(args),
        ),
      ),
    )
  | EVar(ident) => Unary(Primary(Operand(Var(ident))))
  | EBool(b) => Unary(Primary(Operand(BoolLit(b))))
  | EFloat(flt) => Unary(Primary(Operand(FloatLit(flt))))
  | EInt(i) => Unary(Primary(Operand(IntLit(i))))
  | EStr({estr_lit: s, estr_kind: _}) =>
    Unary(Primary(Operand(StrLit(s))))
  | EStruct(items) =>
    Unary(
      Primary(
        Operand(ArrayLit(tval, List.map(~f=from_expression, items))),
      ),
    )
  | EArr(arr_literal) =>
    Unary(
      Primary(
        Operand(ArrayLit(tval, List.map(~f=from_expression, arr_literal))),
      ),
    )
  | ETag({etag_tag: index, etag_items: itms}) =>
    Unary(
      Primary(
        Operand(
          ArrayLit(
            tval,
            [
              Unary(Primary(Operand(IntLit(index)))),
              ...List.map(~f=from_expression, itms),
            ],
          ),
        ),
      ),
    )
  | EAccess({
      eacc_expr: EVar("runtime"),
      eacc_index: EStr({estr_lit: name, _}),
    }) =>
    Unary(
      Primary(Sel(Operand(Var("runtime")), name |> String.capitalize)),
    )
  | EAccess({eacc_expr: e1, eacc_index: e2}) =>
    let e1m =
      switch (from_expression(e1)) {
      | Unary(Primary(pe)) => pe
      | _ => failwith("unexpected expression for array access")
      };
    Unary(Primary(ArrAccess(e1m |> ta_map, from_expression(e2))));
  | EStructAccess({estruct_expr: e1, estruct_index: index}) =>
    let e1m =
      switch (from_expression(e1)) {
      | Unary(Primary(pe)) => pe
      | _ => failwith("unexpected expression for array access")
      };
    Unary(
      Primary(
        ArrAccess(e1m |> ta_arr, Unary(Primary(Operand(IntLit(index))))),
      ),
    );
  | EArrAccess({earr_expr: e1, earr_index: e2}) =>
    let e1m =
      switch (from_expression(e1)) {
      | Unary(Primary(pe)) => pe
      | _ => failwith("unexpected expression for array access")
      };
    Unary(
      Primary(
        ArrAccess(
          e1m |> ta_arr,
          switch (e2) {
          | EInt(_) => from_expression(e2)
          | _ => from_expression(e2) |> ta_expr(ta_int)
          },
        ),
      ),
    );
  | EBin({ebin_op: BOr, ebin_lhs: e1, ebin_rhs: e2}) =>
    Binary(
      LOr,
      Unary(
        Primary(
          FunApp(Operand(Var("Is_true__")), [from_expression(e1)]),
        ),
      ),
      Unary(
        Primary(
          FunApp(Operand(Var("Is_true__")), [from_expression(e2)]),
        ),
      ),
    )
  | EBin({ebin_op: BAnd, ebin_lhs: e1, ebin_rhs: e2}) =>
    Binary(
      LAnd,
      Unary(
        Primary(
          FunApp(Operand(Var("Is_true__")), [from_expression(e1)]),
        ),
      ),
      Unary(
        Primary(
          FunApp(Operand(Var("Is_true__")), [from_expression(e2)]),
        ),
      ),
    )
  | EBin({ebin_op: binop, ebin_lhs: e1, ebin_rhs: e2}) =>
    Unary(
      Primary(
        FunApp(
          Operand(Var(from_binop(binop))),
          [from_expression(e1), from_expression(e2)],
        ),
      ),
    )
  | ECond(_) =>
    failwith("ECond unsupported by Go backend, please use Econd_rewriter")
  | EArityTest(e) =>
    Unary(
      Primary(FunApp(Operand(Var("Arity__")), [from_expression(e)])),
    )
  | EVectlength(e) =>
    let len_call =
      Unary(
        Primary(
          FunApp(
            Operand(Var("len")),
            [from_expression(e) |> ta_expr(ta_arr)],
          ),
        ),
      );
    Binary(Minus, len_call, Unary(Primary(Operand(IntLit(1)))));
  | EArrLen(e) =>
    Unary(
      Primary(
        FunApp(
          Operand(Var("len")),
          [from_expression(e) |> ta_expr(ta_arr)],
        ),
      ),
    )
  | EDot({edot_expr: e, edot_id: ident}) =>
    let em =
      switch (from_expression(e)) {
      | Unary(Primary(pe)) => pe
      | _ => failwith("unexpected expression for dot access")
      };
    Unary(
      Primary(
        ArrAccess(em |> ta_map, Unary(Primary(Operand(StrLit(ident))))),
      ),
    );
  | ECopy(e) =>
    Unary(Primary(FunApp(Operand(Var("Copy__")), [from_expression(e)])))
  | EUn({eun_op: unop, eun_expr: e}) =>
    Unary(
      Primary(
        FunApp(Operand(Var(from_unop(unop))), [from_expression(e)]),
      ),
    )
  | EFun({params, locals, body, loc}) =>
    gen_func_closure(params, locals, body)
  | ESeq((e1, e2)) =>
    Unary(
      Primary(
        FunApp(
          Operand(Var("Seq__")),
          [from_expression(e1), from_expression(e2)],
        ),
      ),
    )
  | ERaw(segments) =>
    Raw(
      List.map(
        segments,
        ~f=
          fun
          | IR.RawText(text) => RawText(text)
          | RawSubstitution(expr) => RawSubstitution(from_expression(expr)),
      ),
    )
  | ERuntime => failwith("unexpected ERuntime")
  | ECustomRequire(_) => failwith("unsupported ECustomRequire")
  | ECustomRegister(_) => failwith("unsupported ECustomRegister")
  | ERequire(_) => failwith("unexpected ERequire")
  }
and from_statement_list = lst =>
  List.map(~f=((stmt, loc)) => from_statement(loc, stmt), lst)
and from_statement = (loc, e) =>
  switch (e) {
  | IR.SRaw((_provides, _requires, s)) => RawStmt(loc, s)
  | SBlock(stms) => Block(loc, from_statement_list(stms))
  | SEmpty => EmptyStmt(loc)
  | SAssignment({sassign_lvalue: LVar(id), sassign_expr: e}) =>
    Simple(loc, AssignVarEquals([id], [from_expression(e)]))
  /*
   | SAssignment({
       sassign_lvalue: LVar(id),
       sassign_expr: e,
     }) =>
     let go_op =
       switch (assignop) {
       | AEq => failwith("Eq should be handled by another switch case")
       | AStarEq => "Binop_mul__"
       | ASlashEq => "Binop_div__"
       | AModEq => "Binop_mod__"
       | APlusEq => "Binop_plus__"
       | AMinusEq => "Binop_minus__"
       };
     let e_call =
       Unary(
         Primary(
           FunApp(
             Operand(Var(go_op)),
             [Unary(Primary(Operand(Var(id)))), from_expression(e)],
           ),
         ),
       );
     Simple(loc, AssignVarEquals([id], [e_call]));
     */
  | SAssignment({sassign_lvalue: lvalue, sassign_expr: e}) =>
    let go_lvalue =
      switch (lvalue) {
      | IR.LStructAccess({estruct_expr: e, estruct_index: i}) =>
        let em =
          switch (from_expression(e)) {
          | Unary(Primary(pe)) => pe |> ta_arr
          | _ => failwith("unexpected expression for array access")
          };
        LArrAccess(em, Unary(Primary(Operand(IntLit(i)))));
      | LArrAccess({earr_expr: e1, earr_index: e2}) =>
        let e1m =
          switch (from_expression(e1)) {
          | Unary(Primary(pe)) => pe |> ta_arr
          | _ => failwith("unexpected expression for array access")
          };
        LArrAccess(e1m, from_expression(e2) |> ta_expr(ta_int));
      | LVar(_) => failwith("LVar should be handled by another switch case")
      };
    Simple(loc, AssignEquals([go_lvalue], [from_expression(e)]));
  /*
   | SAssignment({
       sassign_op: assignop,
       sassign_lvalue: lvalue,
       sassign_expr: e,
     }) =>
     let go_op =
       switch (assignop) {
       | AEq => failwith("Eq should be handled by another switch case")
       | AStarEq => "Binop_mul__"
       | ASlashEq => "Binop_div__"
       | AModEq => "Binop_mod__"
       | APlusEq => "Binop_plus__"
       | AMinusEq => "Binop_minus__"
       };
     let go_lvalue =
       switch (lvalue) {
       | IR.LStructAccess({estruct_expr: e, estruct_index: i}) =>
         let em =
           switch (from_expression(e)) {
           | Unary(Primary(pe)) => pe
           | _ => failwith("unexpected expression for array access")
           };
         LArrAccess(em |> ta_arr, Unary(Primary(Operand(IntLit(i)))));
       | LArrAccess({earr_expr: e1, earr_index: e2}) =>
         let e1m =
           switch (from_expression(e1)) {
           | Unary(Primary(pe)) => pe |> ta_arr
           | _ => failwith("unexpected expression for array access")
           };
         LArrAccess(e1m, from_expression(e2) |> ta_expr(ta_int));
       | LVar(_) => failwith("LVar should be handled by another switch case")
       };
     let go_value =
       switch (lvalue) {
       | IR.LStructAccess(x) => IR.EStructAccess(x) |> from_expression
       | LArrAccess(_) => failwith("LArrAccess is unsupported")
       | LVar(_) => failwith("LVar should be handled by another switch case")
       };
     let e_call =
       Unary(
         Primary(
           FunApp(Operand(Var(go_op)), [go_value, from_expression(e)]),
         ),
       );
     Simple(loc, AssignEquals([go_lvalue], [e_call]));
     */
  | SExpression(EVar(id)) => mark_vars_as_used([id])
  | SExpression(expr) => Simple(loc, Expr(from_expression(expr)))
  | SReturn(eo) =>
    switch (eo) {
    | Some(e) => Return(loc, Some(from_expression(e)))
    | None => failwith("unexpected return without value")
    }
  | SIf((expr, (ifstmt, ifloc), elsopt)) =>
    let me =
      Unary(
        Primary(
          FunApp(Operand(Var("Is_true__")), [from_expression(expr)]),
        ),
      );
    switch (elsopt) {
    | None =>
      If(loc, IfOnly(IfCond(None, me), [from_statement(ifloc, ifstmt)]))
    | Some((elstmt, elloc)) =>
      If(
        loc,
        IfElse(
          IfCond(None, me),
          [from_statement(ifloc, ifstmt)],
          [from_statement(elloc, elstmt)],
        ),
      )
    };
  | SLoop((stmt, stmtloc)) =>
    For(loc, InfLoop([from_statement(stmtloc, stmt)]))
  | SThrow(e) =>
    let call =
      Unary(
        Primary(FunApp(Operand(Var("panic")), [from_expression(e)])),
      );
    Simple(loc, Expr(call));
  | STry((b1, (ident, b2))) =>
    let block_closure = mk_unit_closure_expr([], from_statement_list(b1));
    let call = {
      let catch_closure =
        mk_unit_closure_expr([ident], from_statement_list(b2));

      Unary(
        Primary(
          FunApp(
            Operand(Var("Try_catch__")),
            [block_closure, catch_closure],
          ),
        ),
      );
    };
    Simple(loc, Expr(call));
  | SLabelled((lbl, (stmt, stmtloc))) =>
    LabelledStmt(loc, lbl, from_statement(stmtloc, stmt))
  | SContinue(lbl) => Continue(loc, lbl)
  | SBreak(lbl) => Break(loc, lbl)
  | SSwitch((e, case_clause_list, stmt_lst)) =>
    let e = from_expression(e);
    let (last1, last2) =
      switch (stmt_lst) {
      | [] => (true, false)
      | _ => (false, true)
      };
    let case_clause_lst =
      from_case_clause_list(loc, case_clause_list, ~is_last=last1);
    let default_clause_lst =
      switch (stmt_lst) {
      | [] => []
      | stmts =>
        let tail_stmts = last2 ? [] : [Fallthrough(loc)];
        [ExprCaseClause(Default, from_statement_list(stmts) @ tail_stmts)];
      };
    Switch(
      loc,
      SwitchStmt(
        Some(SwitchCond(None, Some(e))),
        case_clause_lst @ default_clause_lst,
      ),
    );
  }
and from_case_clause_list = (loc, lst, ~is_last) => {
  let len = List.length(lst);
  List.mapi(
    ~f=
      (i, {case_expr: e, case_stmts: stmts}) => {
        let tail_stmts =
          switch (List.rev(stmts)) {
          | [(IR.SBreak(_), _), ..._]
          | [(IR.SReturn(_), _), ..._] => []
          | _ when !(i == len - 1 && is_last) => [Fallthrough(loc)]
          | _ => []
          };
        ExprCaseClause(
          Case([from_expression(e)]),
          from_statement_list(stmts) @ tail_stmts,
        );
      },
    lst,
  );
}
and from_function_body_element = loc =>
  fun
  | IR.SEStatement(stmt) => from_statement(loc, stmt)
  | IR.SEFunDecl({name, func: {params, locals, body, loc}}) =>
    gen_func(name, params, locals, body, loc)
and gen_func_closure = (formal_parameter_list, locals, function_body) => {
  let body_stmts =
    List.map(
      ~f=((src, loc)) => from_function_body_element(loc, src),
      function_body,
    );
  let body_stmts = [
    mark_vars_as_used(formal_parameter_list),
    declare_locals(locals),
    mark_vars_as_used(locals),
    ...body_stmts,
  ];
  Unary(
    Primary(
      Cast(
        TAny,
        Unary(
          Primary(
            Operand(
              Parens(
                Unary(
                  Primary(
                    Closure(
                      Some(
                        List.map(formal_parameter_list, ~f=name =>
                          ([name], tval)
                        ),
                      ),
                      Some(tval),
                      body_stmts,
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  );
}
and gen_func = (ident, formal_parameter_list, locals, function_body, loc) => {
  Simple(
    loc,
    AssignVarEquals(
      [ident],
      [gen_func_closure(formal_parameter_list, locals, function_body)],
    ),
  );
}
and from_ir = (rehp: IR.program) => {
  let (IR.{prg_locals: locals, prg_elements: lst}, imports) =
    (new hoist_requires)#program(rehp, []);
  let body_stmts =
    List.map(~f=((src, loc)) => from_function_body_element(loc, src), lst);
  let body_stmts = [
    declare_locals(locals),
    mark_vars_as_used(locals),
    RawStmt(N, "defer Unhandled_panic__()"),
    ...body_stmts,
  ];
  let main_fun_decl = FuncDecl(N, "main", None, None, body_stmts);
  let toplevel_decls =
    [
      [Import(N, "caml_primitives", Some("."))],
      imports |> List.map(~f=((what, as_)) => Import(N, what, Some(as_))),
      [main_fun_decl],
    ]
    |> List.concat;
  Program(N, Package(N, "main"), toplevel_decls);
};
