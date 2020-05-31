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
open IR;
module Fmt = {
  include Fmt;
  let (++) = append;
};

let pp_binop = f =>
  fun
  | BOr => Fmt.pf(f, "||")
  | BAnd => Fmt.pf(f, "&&")
  | BBor => Fmt.pf(f, "|")
  | BBxor => Fmt.pf(f, "^")
  | BBand => Fmt.pf(f, "&")
  | BEqEq => Fmt.pf(f, "==")
  | BNotEq => Fmt.pf(f, "!=")
  | BFloatEqEq => Fmt.pf(f, "==.")
  | BFloatNotEq => Fmt.pf(f, "!=.")
  | BEqEqEq => Fmt.pf(f, "===")
  | BNotEqEq => Fmt.pf(f, "!==")
  | BLt => Fmt.pf(f, "<")
  | BLe => Fmt.pf(f, "<=")
  | BGt => Fmt.pf(f, ">")
  | BGe => Fmt.pf(f, ">=")
  | BFloatLt => Fmt.pf(f, "<.")
  | BFloatLe => Fmt.pf(f, "<=.")
  | BFloatGt => Fmt.pf(f, ">.")
  | BFloatGe => Fmt.pf(f, ">=.")
  | BLsl => Fmt.pf(f, "<<")
  | BLsr => Fmt.pf(f, ">>")
  | BAsr => Fmt.pf(f, ">>>")
  | BFloatPlus => Fmt.pf(f, "+.")
  | BIntPlus => Fmt.pf(f, "+")
  | BPlus => Fmt.pf(f, "+?")
  | BMinus => Fmt.pf(f, "-?")
  | BMul => Fmt.pf(f, "*?")
  | BDiv => Fmt.pf(f, "/?")
  | BMod => Fmt.pf(f, "(mod?)")
  | BFloatMinus => Fmt.pf(f, "-.")
  | BFloatMul => Fmt.pf(f, "*.")
  | BFloatDiv => Fmt.pf(f, "/.")
  | BFloatMod => Fmt.pf(f, "(mod.)");

let pp_formal_parameter_list = (f, l) =>
  Fmt.pf(f, "@[<h>%a@]", Fmt.(list(~sep=comma, string)), l);

let pp_locals = f =>
  fun
  | [] => ()
  | l => Fmt.pf(f, "vars @[%a@]@,", Fmt.(list(~sep=comma, string)), l);

let escape_string = s =>
  s
  |> String.substr_replace_all(~pattern="\\", ~with_="\\\\")
  |> String.substr_replace_all(~pattern="\n", ~with_="\\n")
  |> String.substr_replace_all(~pattern="\"", ~with_="\\\"");

let pp_estr = (f, estr) =>
  Fmt.pf(f, "\"%s\"", escape_string(estr.estr_lit));

let pp_unop = f =>
  fun
  | UNot => Fmt.pf(f, "!")
  | UNeg => Fmt.pf(f, "(-)")
  | UFloatNeg => Fmt.pf(f, "(-.)")
  | UIsInt => Fmt.pf(f, "(int?)")
  | UToInt => Fmt.pf(f, "(int)")
  | UToBool => Fmt.pf(f, "(bool)")
  | UIntToString => Fmt.pf(f, "(int_to_string)")
  | UFloatToInt => Fmt.pf(f, "(float_to_int)")
  | UIntToFloat => Fmt.pf(f, "(int_to_float)")
  | UBnot => Fmt.pf(f, "~");

let rec pp_arguments = (f, l) =>
  Fmt.pf(f, "@[%a@]", Fmt.(list(~sep=comma, pp_expression)), l)

and pp_eaccess = (f, {eacc_expr, eacc_index}) =>
  Fmt.pf(f, "%a[%a]", pp_expression, eacc_expr, pp_expression, eacc_index)

and pp_earr_access = (f, {earr_expr, earr_index}) =>
  Fmt.pf(f, "%a.[%a]", pp_expression, earr_expr, pp_expression, earr_index)

and pp_ebin = (f, {ebin_op, ebin_lhs, ebin_rhs}) =>
  Fmt.pf(
    f,
    "%a %a %a",
    pp_expression,
    ebin_lhs,
    pp_binop,
    ebin_op,
    pp_expression,
    ebin_rhs,
  )

and pp_ecall = (f, {ecall_expr, ecall_args}) =>
  Fmt.pf(
    f,
    "%a(@;<0 2>@[%a@])",
    pp_expression,
    ecall_expr,
    pp_arguments,
    ecall_args,
  )

and pp_econd = (f, {econd_test, econd_success, econd_failure}) =>
  Fmt.pf(
    f,
    "%a ? %a : %a",
    pp_expression,
    econd_test,
    pp_expression,
    econd_success,
    pp_expression,
    econd_failure,
  )

and pp_edot = (f, {edot_expr, edot_id}) =>
  Fmt.pf(f, "%a.%s", pp_expression, edot_expr, edot_id)

and pp_estruct_access = (f, {estruct_expr, estruct_index}) =>
  Fmt.pf(f, "%a{%d}", pp_expression, estruct_expr, estruct_index)

and pp_etag = (f, {etag_tag, etag_items}) =>
  Fmt.pf(f, "{%d;%a}", etag_tag, pp_arguments, etag_items)

and pp_eun = (f, {eun_op, eun_expr}) =>
  Fmt.pf(f, "%a(%a)", pp_unop, eun_op, pp_expression, eun_expr)

and pp_expression = f =>
  fun
  | ERaw(_) => Fmt.pf(f, "%%ERAW%%")
  | ESeq((e1, e2)) =>
    Fmt.pf(f, "(%a, %a)", pp_expression, e1, pp_expression, e2)
  | ECond(econd) => pp_econd(f, econd)
  | EBin(ebin) => pp_ebin(f, ebin)
  | EUn(eun) => pp_eun(f, eun)
  | ECall(ecall) => pp_ecall(f, ecall)
  | EVar(id) => Fmt.pf(f, "%s", id)
  | EFun(fexp) => pp_function_expression(f, fexp)
  | EArityTest(e) => Fmt.pf(f, "$arity(%a)", pp_expression, e)
  | EStr(estr) => pp_estr(f, estr)
  | EVectlength(e) => Fmt.pf(f, "$vec_len(%a)", pp_expression, e)
  | EArrAccess(earr_access) => pp_earr_access(f, earr_access)
  | EArrLen(e) => Fmt.pf(f, "$arr_len(%a)", pp_expression, e)
  | EArr(array_literal) => Fmt.pf(f, "[%a]", pp_arguments, array_literal)
  | EStructAccess(estruct_access) => pp_estruct_access(f, estruct_access)
  | EStruct(arguments) => Fmt.pf(f, "{%a}", pp_arguments, arguments)
  | ETag(etag) => pp_etag(f, etag)
  | EDot(edot) => pp_edot(f, edot)
  | ECopy(e) => Fmt.pf(f, "$copy(%a)", pp_expression, e)
  | EAccess(eaccess) => pp_eaccess(f, eaccess)
  | EBool(b) => Fmt.pf(f, "%b", b)
  | EFloat(fl) => Fmt.pf(f, "%s", Float.to_string(fl))
  | EInt(i) => Fmt.pf(f, "%d", i)
  | ECustomRequire(r) => Fmt.pf(f, "$custom_require(%s)", r)
  | ECustomRegister(e) => Fmt.pf(f, "$custom_register(%a)", pp_expression, e)
  | ERequire(r) => Fmt.pf(f, "$require(%s)", r)
  | ERuntime => Fmt.pf(f, "runtime")

and pp_function_declaration =
    (f, {name, func: {params, locals, loc: _, body}}) =>
  Fmt.pf(
    f,
    "function %s(%a) {@;<0 2>@[%a%a@]}",
    name,
    pp_formal_parameter_list,
    params,
    pp_locals,
    locals,
    pp_source_elements,
    body,
  )

and pp_function_expression = (f, {params, locals, loc, body}) =>
  Fmt.pf(
    f,
    "function(%a) {%a%a}",
    pp_formal_parameter_list,
    params,
    pp_locals,
    locals,
    pp_source_elements,
    body,
  )

and pp_lvalue = f =>
  fun
  | LVar(s) => Fmt.pf(f, "%s", s)
  | LArrAccess(earr_access) => pp_earr_access(f, earr_access)
  | LStructAccess(estruct_access) => pp_estruct_access(f, estruct_access)

and pp_sassign = (f, {sassign_lvalue, sassign_expr}) =>
  Fmt.pf(f, "%a = %a", pp_lvalue, sassign_lvalue, pp_expression, sassign_expr)

and pp_source_element = f =>
  fun
  | SEStatement(statement) => pp_statement(f, statement)
  | SEFunDecl(function_declaration) =>
    pp_function_declaration(f, function_declaration)

and pp_vlist:
  'a.
  (Formatter.t, (Formatter.t, 'a) => unit, list(('a, loc))) => unit
 =
  (f, pp, el) => {
    let pp_e = (f, e) => Fmt.pf(f, "@[%a@]", pp, e);
    Fmt.pf(f, "@[<v>%a@,@]", Fmt.(list(pp_e)), List.map(el, ~f=fst));
  }

and pp_source_elements = (f, el) => pp_vlist(f, pp_source_element, el)

and pp_case_clause = (f, {case_expr, case_stmts}) =>
  Fmt.pf(
    f,
    "case %a:%a",
    pp_expression,
    case_expr,
    pp_statement_list,
    case_stmts,
  )

and pp_statement = f =>
  fun
  | SBlock(block) => Fmt.pf(f, "{@;<0 2>%a}", pp_statement_list, block)
  | SRaw(_) => Fmt.pf(f, "%%SRAW%%")
  | SAssignment(sassign) => pp_sassign(f, sassign)
  | SEmpty => ()
  | SExpression(expression) => pp_expression(f, expression)
  | SIf((e, (s1, _), None)) =>
    Fmt.pf(f, "if (%a)@;<1 2>%a", pp_expression, e, pp_statement, s1)
  | SIf((e, (s1, _), Some((s2, _)))) =>
    Fmt.pf(
      f,
      "if (%a)@;<1 2>%a@;<1>else@;<1 2>%a",
      pp_expression,
      e,
      pp_statement,
      s1,
      pp_statement,
      s2,
    )
  | SLoop((s, _)) => Fmt.pf(f, "loop %a", pp_statement, s)
  | SContinue(lblo) =>
    Fmt.pf(f, "continue%a", Fmt.(option(any(" ") ++ string)), lblo)
  | SBreak(lblo) =>
    Fmt.pf(f, "break%a", Fmt.(option(any(" ") ++ string)), lblo)
  | SReturn(eo) =>
    Fmt.pf(f, "return%a", Fmt.(option(any(" ") ++ pp_expression)), eo)
  | SLabelled((lbl, (s, _))) => Fmt.pf(f, "%s:@;%a", lbl, pp_statement, s)
  | SSwitch((e, cases, default)) => {
      let pp_default = f => (
        fun
        | [] => ()
        | sl => Fmt.pf(f, "default:@;<0 2>%a@,", pp_statement_list, sl)
      );
      Fmt.pf(
        f,
        "switch (%a) {@[<v 2>@,%a@,%a@]}",
        pp_expression,
        e,
        Fmt.(list(pp_case_clause)),
        cases,
        pp_default,
        default,
      );
    }
  | SThrow(e) => Fmt.pf(f, "throw %a", pp_expression, e)
  | STry((b1, (id, b2))) =>
    Fmt.pf(
      f,
      "try %a@;<1>catch (%s) %a",
      pp_statement,
      SBlock(b1),
      id,
      pp_statement,
      SBlock(b2),
    )

and pp_statement_list = (f, sl) => pp_vlist(f, pp_statement, sl)

and pp_program = (f, {prg_locals, prg_elements}) =>
  Fmt.pf(
    f,
    "%a@;%a",
    pp_locals,
    prg_locals,
    pp_source_elements,
    prg_elements,
  );

let print = ir => {
  let buf = Buffer.create(16);
  let f = Fmt.with_buffer(buf);
  Fmt.pf(f, "@[<v>%a@]@.", pp_program, ir);
  Buffer.contents(buf);
};

let print_statement = s => {
  let buf = Buffer.create(16);
  let f = Fmt.with_buffer(buf);
  Fmt.pf(f, "@[<v>%a@]@.", pp_statement, s);
  Buffer.contents(buf);
};
