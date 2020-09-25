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
open Js_of_ocaml_compiler;

let string_of_id =
  fun
  | Id.S(s) => s.name
  | V(var) => Code.Var.to_string(var);

let from_loc =
  fun
  | Loc.Pi({name, line, col, _}) => IR.Pi({filename: name, line, col})
  | N => IR.N
  | U => IR.U;

let rec var_hoist_statement_list = lst =>
  List.map(~f=((stmt, _loc)) => var_hoist_statement(stmt), lst)
and var_hoist_statement =
  fun
  | Rehp.Block(stms) => var_hoist_statement_list(stms) |> List.concat
  | Rehp.Variable_statement(lst) =>
    List.map(
      ~f=
        ((id: Id.t, _initopt: option(Rehp.initialiser))) =>
          [id |> string_of_id],
      lst,
    )
    |> List.concat
  | Rehp.Empty_statement => []
  | Rehp.Expression_statement(_expr) => []
  | Rehp.Return_statement(_eo) => []
  | Rehp.If_statement(_expr, (ifstmt, _ifloc), elsopt) =>
    switch (elsopt) {
    | None => var_hoist_statement(ifstmt)
    | Some((elstmt, _elloc)) =>
      var_hoist_statement(ifstmt) @ var_hoist_statement(elstmt)
    }
  | Rehp.Loop_statement(stmt, _loc) => var_hoist_statement(stmt)
  | Rehp.Throw_statement(_e) => []
  | Rehp.Try_statement(b1, (_, b2)) => {
      let block_closure_hoist = var_hoist_statement_list(b1);
      let ident_block_hoist = var_hoist_statement_list(b2);
      [block_closure_hoist, ident_block_hoist] |> List.concat |> List.concat;
    }
  | Rehp.Labelled_statement(_lbl, (stmt, _stmtloc)) =>
    var_hoist_statement(stmt)
  | Rehp.Continue_statement(_lbl, _)
  | Rehp.Break_statement(_lbl) => []
  | Rehp.Switch_statement(_e, case_clause_list, stmt_lst) => {
      let case_clause_lst = var_hoist_case_clause_list(case_clause_list);
      let default_clause_lst = var_hoist_statement_list(stmt_lst);
      [case_clause_lst, default_clause_lst] |> List.concat |> List.concat;
    }
  | _ => {
      Stdio.print_endline("hoisting: unsupported statement kind");
      [];
    }
and var_hoist_case_clause_list = lst =>
  List.map(~f=((_e, stmts)) => var_hoist_statement_list(stmts), lst)
  |> List.concat
and var_hoist_source_element =
  fun
  | Rehp.Statement(stmt) => var_hoist_statement(stmt)
  | Rehp.Function_declaration((
      ident,
      _formal_parameter_list,
      _function_body,
      _location,
    )) => [
      ident |> string_of_id,
    ]
and var_hoist_source_elements = lst =>
  List.map(~f=((src, _loc)) => var_hoist_source_element(src), lst)
  |> List.concat
  |> List.dedup_and_sort(~compare=String.compare);

let rec from_unop = unop =>
  switch (unop) {
  | Rehp.Not => IR.UNot
  | IsInt => UIsInt
  | ToInt => UToInt
  | ToBool => UToBool
  | IntToString => UIntToString
  | FloatToInt => UFloatToInt
  | IntToFloat => UIntToFloat
  | Neg => UNeg
  | FloatNeg => UFloatNeg
  | Bnot => UBnot
  | Typeof => failwith("unsupported unop: Typeof")
  | Void => failwith("unsupported unop: Void")
  | Delete => failwith("unsupported unop: Delete")
  | IncrA => failwith("unsupported unop: IncrA")
  | DecrA => failwith("unsupported unop: DecrA")
  | IncrB => failwith("unsupported unop: IncrB")
  | DecrB => failwith("unsupported unop: DecrB")
  }
and from_binop =
  fun
  | Rehp.EqEqEq => IR.BEqEqEq
  | EqEq => BEqEq
  | FloatEqEq => IR.BFloatEqEq
  | FloatNotEq => BFloatNotEq
  | Or => BOr
  | And => BAnd
  | Bor => BBor
  | Bxor => BBxor
  | Band => BBand
  | NotEqEq => BNotEqEq
  | NotEq => BNotEq
  | Lt => BLt
  | Le => BLe
  | Gt => BGt
  | Ge => BGe
  | FloatLt => BFloatLt
  | FloatLe => BFloatLe
  | FloatGt => BFloatGt
  | FloatGe => BFloatGe
  | Lsl => BLsl
  | Lsr => BLsr
  | Asr => BAsr
  | Plus => BPlus
  | FloatPlus => BFloatPlus
  | IntPlus => BIntPlus
  | Minus => BMinus
  | Mul => BMul
  | Div => BDiv
  | Mod => BMod
  | FloatMinus => BFloatMinus
  | FloatMul => BFloatMul
  | FloatDiv => BFloatDiv
  | FloatMod => BFloatMod
  | Eq => failwith("unsupported binop: Eq")
  | InstanceOf => failwith("unsupported binop: InstanceOf")
  | StarEq
  | SlashEq
  | ModEq
  | PlusEq
  | MinusEq
  | BandEq
  | BxorEq
  | BorEq => failwith("`*Eq' binops should be rewritten earlier")
and from_segment_list = lst =>
  List.map(
    lst,
    ~f=
      fun
      | Rehp.RawText(s) => IR.RawText(s)
      | RawSubstitution(e) => RawSubstitution(e |> from_expression),
  )
and from_arguments = args => List.map(~f=from_expression, args)
and from_element_list = lst =>
  List.map(
    lst,
    ~f=
      fun
      | Some(e) => from_expression(e)
      | None => failwith("None is unsupported in element list"),
  )
and from_expression = e => {
  let expr_desc =
    switch (e) {
    | Rehp.ECall(e, args, _loc) =>
      IR.ECall({
        ecall_expr: from_expression(e),
        ecall_args: from_arguments(args),
      })
    | EVar(ident) => EVar(ident |> string_of_id)
    | EBool(b) => EBool(b)
    | EFloat(flt) => EFloat(flt)
    | EInt(i) => EInt(i)
    | EStr(s, `Bytes) => EStr({estr_lit: s, estr_kind: Bytes})
    | EStr(s, `Utf8) => EStr({estr_lit: s, estr_kind: Utf8})
    | EStruct(items) => EStruct(items |> List.map(~f=from_expression))
    | EArr(arr_literal) => EArr(arr_literal |> from_element_list)
    | ETag(tag, itms) =>
      ETag({etag_tag: tag, etag_items: itms |> from_arguments})
    | EAccess(e1, e2) =>
      EAccess({
        eacc_expr: e1 |> from_expression,
        eacc_index: e2 |> from_expression,
      })
    | EStructAccess(e1, i) =>
      EStructAccess({estruct_expr: e1 |> from_expression, estruct_index: i})
    | EArrAccess(e1, e2) =>
      EArrAccess({
        earr_expr: e1 |> from_expression,
        earr_index: e2 |> from_expression,
      })
    | EBin(binop, e1, e2) =>
      EBin({
        ebin_op: binop |> from_binop,
        ebin_lhs: e1 |> from_expression,
        ebin_rhs: e2 |> from_expression,
      })
    | ECond(e1, e2, e3) =>
      ECond({
        econd_test: e1 |> from_expression,
        econd_success: e2 |> from_expression,
        econd_failure: e3 |> from_expression,
      })
    | EArityTest(e) => EArityTest(e |> from_expression)
    | EVectlength(e) => EVectlength(e |> from_expression)
    | EArrLen(e) => EArrLen(e |> from_expression)
    | EDot(e, ident) =>
      EDot({edot_expr: e |> from_expression, edot_id: ident})
    | EUn(unop, e) =>
      EUn({eun_op: unop |> from_unop, eun_expr: e |> from_expression})
    | EFun((None, ident_lst, body, loc)) =>
      EFun({
        params:
          ident_lst
          |> List.map(~f=string_of_id)
          |> List.map(~f=x => (x, IR.gen_new_type())),
        locals:
          var_hoist_source_elements(body)
          |> List.map(~f=x => (x, IR.gen_new_type())),
        body: body |> from_function_body,
        loc: from_loc(loc),
      })
    | ESeq(e1, e2) => ESeq((e1 |> from_expression, e2 |> from_expression))
    | ERaw(segments) => ERaw(segments |> from_segment_list)
    | ERequire(str) => ERequire(str)
    | ECustomRequire(str) => ECustomRequire(str)
    | ECustomRegister(e) => ECustomRegister(from_expression(e))
    | ERuntime => ERuntime
    | ECopy(e, _loc) => ECopy(from_expression(e))
    | _ => failwith("unsupported expression")
    };
  /*
   switch (e) {
   | EBool(_) => {expr_desc, expr_typ: TBool}
   | EFloat(_) => {expr_desc, expr_typ: TFloat}
   | EInt(_) => {expr_desc, expr_typ: TInt}
   | EStr(_) => {expr_desc, expr_typ: TString}
   | _ => {expr_desc, expr_typ: IR.gen_new_type()}
   };
   */
  {expr_desc, expr_typ: IR.gen_new_type()};
}
and from_lvalue = e => {
  let lv_desc =
    switch (e) {
    | Rehp.EVar(ident) => IR.LVar(ident |> string_of_id)
    | EStructAccess(e1, i) =>
      LStructAccess({estruct_expr: e1 |> from_expression, estruct_index: i})
    | EArrAccess(e1, e2) =>
      LArrAccess({
        earr_expr: e1 |> from_expression,
        earr_index: e2 |> from_expression,
      })
    | _ => failwith("unsupported lvalue")
    };
  IR.{lv_desc, lv_typ: IR.gen_new_type()};
}
and from_statement_loc_list = lst =>
  List.map(
    ~f=((stmt, loc)) => (from_statement(stmt), from_loc(loc)),
    lst,
  )
and from_statement = e =>
  switch (e) {
  | Rehp.Block(stms) => IR.SBlock(stms |> from_statement_loc_list)
  | Variable_statement([]) => SEmpty
  | Variable_statement([(ident, Some((e, _loc)))]) =>
    SAssignment({
      sassign_lvalue: {
        lv_desc: LVar(ident |> string_of_id),
        lv_typ: IR.gen_new_type(),
      },
      sassign_expr: from_expression(e),
    })
  | Empty_statement => SEmpty
  | Expression_statement(EBin(StarEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Mul, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(SlashEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Div, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(ModEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Mod, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(PlusEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Plus, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(MinusEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Minus, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(BandEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Band, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(BxorEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Bxor, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(BorEq, e1, e2)) =>
    Rehp.(Expression_statement(EBin(Eq, e1, EBin(Bor, e1, e2))))
    |> from_statement
  | Expression_statement(EBin(Eq, e1, e2)) =>
    SAssignment({
      sassign_lvalue: from_lvalue(e1),
      sassign_expr: from_expression(e2),
    })
  | Expression_statement(EBin(_) as expr) =>
    SExpression(expr |> from_expression)
  | Rehp.Expression_statement(expr) => SExpression(expr |> from_expression)
  | Rehp.Return_statement(eo) => SReturn(Option.map(eo, ~f=from_expression))
  | Rehp.If_statement(e1, (s1, loc), s2o) =>
    SIf((
      e1 |> from_expression,
      (s1 |> from_statement, from_loc(loc)),
      s2o
      |> Option.map(~f=((s, loc)) => (s |> from_statement, from_loc(loc))),
    ))
  | Rehp.Loop_statement(s, loc) =>
    SLoop((s |> from_statement, from_loc(loc)))
  | Rehp.Throw_statement(e) => SThrow(e |> from_expression)
  | Rehp.Try_statement(b1, (id, b2)) =>
    STry((
      b1 |> from_statement_loc_list,
      (id |> string_of_id, b2 |> from_statement_loc_list),
    ))
  | Rehp.Labelled_statement(lbl, (s, loc)) =>
    SLabelled((
      lbl |> Javascript.Label.to_string,
      (from_statement(s), from_loc(loc)),
    ))
  | Rehp.Continue_statement(lbl, _depth) =>
    SContinue(lbl |> Option.map(~f=Javascript.Label.to_string))
  | Rehp.Break_statement(lbl) =>
    SBreak(lbl |> Option.map(~f=Javascript.Label.to_string))
  | Rehp.Switch_statement(e, case_clause_list, stmt_lst) =>
    SSwitch((
      e |> from_expression,
      case_clause_list |> from_case_clause_list,
      stmt_lst |> from_statement_loc_list,
    ))
  | _ => failwith("unsupported statement")
  }
and from_case_clause_list = lst => {
  List.map(
    ~f=
      ((e, stmts)) =>
        IR.{
          case_expr: e |> from_expression,
          case_stmts: stmts |> from_statement_loc_list,
        },
    lst,
  );
}
and from_function_body_element =
  fun
  | Rehp.Statement(stmt) => IR.SEStatement(stmt |> from_statement)
  | Function_declaration((
      ident,
      formal_parameter_list,
      function_body,
      location,
    )) =>
    SEFunDecl({
      name: ident |> string_of_id,
      func: {
        params:
          formal_parameter_list
          |> List.map(~f=string_of_id)
          |> List.map(~f=x => (x, IR.gen_new_type())),
        locals:
          var_hoist_source_elements(function_body)
          |> List.map(~f=x => (x, IR.gen_new_type())),
        body: from_function_body(function_body),
        loc: from_loc(location),
      },
    })
and from_function_body = lst =>
  List.map(lst, ~f=((el, loc)) =>
    (from_function_body_element(el), from_loc(loc))
  )
and ir_from_rehp = lst => {
  IR.{
    prg_locals:
      var_hoist_source_elements(lst)
      |> List.map(~f=x => (x, IR.gen_new_type())),
    prg_elements: from_function_body(lst),
  };
};
