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
[@deriving (sexp, compare, equal, hash, traverse)]
type typ =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TString
  | TVar(string)
  | TFun(list(typ), typ)
  | TStruct
  | TArray
  | TAny(string)

and binop =
  | BOr
  | BAnd
  | BBor
  | BBxor
  | BBand
  | BEqEq
  | BNotEq
  | BFloatEqEq
  | BFloatNotEq
  | BEqEqEq
  | BNotEqEq
  | BLt
  | BLe
  | BGt
  | BGe
  | BFloatLt
  | BFloatLe
  | BFloatGt
  | BFloatGe
  | BLsl
  | BLsr
  | BAsr
  | BFloatPlus
  | BIntPlus
  | BPlus
  | BMinus
  | BMul
  | BDiv
  | BMod
  | BFloatMinus
  | BFloatMul
  | BFloatDiv
  | BFloatMod

and formal_parameter_list = list((string, typ))

and locals = list((string, typ))

and parse_info = {
  filename: option(string),
  col: int,
  line: int,
}

and loc =
  | Pi(parse_info)
  | N
  | U

and string_kind =
  | Bytes
  | Utf8

and estr = {
  estr_lit: string,
  estr_kind: string_kind,
}

and unop =
  | UNot
  | UNeg
  | UFloatNeg
  | UIsInt
  | UToInt
  | UToBool
  | UIntToString
  | UFloatToInt
  | UIntToFloat
  | UBnot

and arguments = list(expression)

and array_literal = element_list

and block = statement_list

and case_clause = {
  case_expr: expression,
  case_stmts: statement_list,
}

and eaccess = {
  eacc_expr: expression,
  eacc_index: expression,
}

and earr_access = {
  earr_expr: expression,
  earr_index: expression,
}

and ebin = {
  ebin_op: binop,
  ebin_lhs: expression,
  ebin_rhs: expression,
}

and ecall = {
  ecall_expr: expression,
  ecall_args: arguments,
}

and econd = {
  econd_test: expression,
  econd_success: expression,
  econd_failure: expression,
}

and edot = {
  edot_expr: expression,
  edot_id: string,
}

and element_list = list(expression)

and estruct_access = {
  estruct_expr: expression,
  estruct_index: int,
}

and etag = {
  etag_tag: int,
  etag_items: arguments,
}

and eun = {
  eun_op: unop,
  eun_expr: expression,
}

and expression_desc =
  | ERaw(list(raw_segment))
  | ESeq((expression, expression))
  | ECond(econd)
  | EBin(ebin)
  | EUn(eun)
  | ECall(ecall)
  | EVar(string)
  | EFun(function_expression)
  | EArityTest(expression)
  | EStr(estr)
  | EVectlength(expression)
  | EArrAccess(earr_access)
  | EArrLen(expression)
  | EArr(array_literal)
  | EStructAccess(estruct_access)
  | EStruct(arguments)
  | ETag(etag)
  | EDot(edot)
  | EAccess(eaccess)
  | ECopy(expression)
  | EBool(bool)
  | EFloat(float)
  | EInt(int)
  | ECustomRequire(string)
  | ECustomRegister(expression)
  | ERequire(string)
  | ERuntime

and expression = {
  expr_desc: expression_desc,
  expr_typ: typ,
}

and function_body = source_elements

and function_declaration = {
  name: string,
  func: function_expression,
}

and function_expression = {
  params: formal_parameter_list,
  locals,
  loc,
  body: function_body,
}

and lvalue_desc =
  | LVar(string)
  | LArrAccess(earr_access)
  | LStructAccess(estruct_access)

and lvalue = {
  lv_desc: lvalue_desc,
  lv_typ: typ,
}

and raw_segment =
  | RawText(string)
  | RawSubstitution(expression)

and sassign = {
  sassign_lvalue: lvalue,
  sassign_expr: expression,
}

and source_element =
  | SEStatement(statement)
  | SEFunDecl(function_declaration)

and source_elements = list((source_element, loc))

and statement =
  | SBlock(block)
  | SRaw((list(string), list(string), string))
  | SAssignment(sassign)
  | SEmpty
  | SExpression(expression)
  | SIf((expression, (statement, loc), option((statement, loc))))
  | SLoop((statement, loc))
  | SContinue(option(string))
  | SBreak(option(string))
  | SReturn(option(expression))
  | SLabelled((string, (statement, loc)))
  | SSwitch((expression, list(case_clause), statement_list))
  | SThrow(expression)
  | STry((block, (string, block)))

and statement_list = list((statement, loc))

and program = {
  prg_locals: locals,
  prg_elements: source_elements,
};

let typ_compare = (a, b) => Sexp.compare(a |> sexp_of_typ, b |> sexp_of_typ);
let typ_equal = (a, b) => typ_compare(a, b) == 0;

let gen_new_type = {
  let last_type_id = ref(0);
  () => {
    Int.incr(last_type_id);
    TVar(Printf.sprintf("t%d", last_type_id^));
  };
};
