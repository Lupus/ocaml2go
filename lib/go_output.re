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
module Fmt = {
  include Fmt;
  let (++) = append;
};

let pp_debug_info = (f, loc) =>
  switch (loc) {
  | IR.Pi(IR.{filename: Some(file), line, col}) =>
    Fmt.pf(f, "@,/*<<%s:%d %d>>*/@,", file, line + 1, col)
  | N => ()
  | U
  | Pi(_) => Fmt.pf(f, "@,/*<<?>>*/@,")
  };

let escape_string = s =>
  List.init(String.length(s), ~f=i => s.[i])
  |> List.map(~f=c =>
       if (Char.is_print(c)) {
         switch (c) {
         | '\n' => "\\n"
         | '\\' => "\\\\"
         | '"' => "\\\""
         | _ => Char.to_string(c)
         };
       } else {
         Printf.sprintf("\\x%02x", Char.to_int(c));
       }
     )
  |> String.concat;

let pp_idlist = f =>
  fun
  | [] => failwith("Varlists should be non-empty")
  | l => Fmt.pf(f, "@[<h>%a@]", Fmt.(list(~sep=comma, string)), l);

let rec pp_typ = f =>
  fun
  | Void => Fmt.pf(f, "void")
  | Int => Fmt.pf(f, "int")
  | Float64 => Fmt.pf(f, "float64")
  | Bool => Fmt.pf(f, "bool")
  | Rune => Fmt.pf(f, "rune")
  | String => Fmt.pf(f, "string")
  | FuncTyp(tl, rt) =>
    Fmt.pf(
      f,
      "func(@[<hov>@,%a@]) %a",
      Fmt.(list(~sep=comma, pp_typ)),
      tl,
      pp_typ,
      rt,
    )
  | TypeVar(id) => Fmt.pf(f, "%s", id)
  | TSlice(t) => Fmt.pf(f, "[]%a", pp_typ, t)
  | TArray(size, t) => Fmt.pf(f, "[%d]%a", size, pp_typ, t)
  | TMap(kt, vt) => Fmt.pf(f, "map[%a]%a", pp_typ, kt, pp_typ, vt)
  | TStruct(vssl) =>
    Fmt.pf(f, "struct {@[<hov>@,%a@]}", Fmt.list(pp_varspecsimp), vssl)
  | TRef(typ) => Fmt.pf(f, "%s", typ)
  | TAny => Fmt.pf(f, "interface{}")

and pp_varspecsimp = f =>
  fun
  | (idl, typ) => Fmt.pf(f, "%a %a", pp_idlist, idl, pp_typ, typ);

let pp_varspecsimp_list = (f, vssl) =>
  Fmt.pf(f, "@[<hov>@,%a@]", Fmt.(list(~sep=comma, pp_varspecsimp)), vssl);

let pp_binop = f =>
  fun
  | LOr => Fmt.pf(f, "||")
  | LAnd => Fmt.pf(f, "&&")
  | CmpEq => Fmt.pf(f, "==")
  | NotEq => Fmt.pf(f, "!=")
  | LT => Fmt.pf(f, "<")
  | GT => Fmt.pf(f, ">")
  | LTE => Fmt.pf(f, "<=")
  | GTE => Fmt.pf(f, ">=")
  | Plus => Fmt.pf(f, "+")
  | Minus => Fmt.pf(f, "-")
  | BitOr => Fmt.pf(f, "|")
  | BitXOr => Fmt.pf(f, "^")
  | Times => Fmt.pf(f, "*")
  | Div => Fmt.pf(f, "/")
  | Mod => Fmt.pf(f, "%%")
  | BitAnd => Fmt.pf(f, "&")
  | BitClr => Fmt.pf(f, "&^")
  | LShift => Fmt.pf(f, "<<")
  | RShift => Fmt.pf(f, ">>");

let pp_uop = f =>
  fun
  | UPlus => Fmt.pf(f, "+")
  | UMinus => Fmt.pf(f, "-")
  | LNot => Fmt.pf(f, "!")
  | UBitXor => Fmt.pf(f, "^")
  | AddrOf => Fmt.pf(f, "&");

let rec pp_expr = f =>
  fun
  | Unary(u) => pp_unary(f, u)
  | Binary(op, e1, e2) => {
      Fmt.pf(f, "%a %a %a", pp_expr, e1, pp_binop, op, pp_expr, e2);
    }
  | Raw(segments) => {
      List.iter(
        segments,
        ~f=
          fun
          | RawText(text) => Fmt.pf(f, "%s", text)
          | RawSubstitution(expr) => pp_expr(f, expr),
      );
    }

and pp_exprlist = (f, el) =>
  Fmt.pf(f, "%a", Fmt.(list(~sep=comma, pp_expr)), el)

and pp_unary = f =>
  fun
  | Primary(pe) => pp_primaryexpr(f, pe)
  | UnaryOp(op, u) => {
      pp_uop(f, op);
      pp_unary(f, u);
    }

and pp_primaryexpr = f =>
  fun
  | Operand(oper) => pp_operand(f, oper)
  | Sel(prim, id) => {
      Fmt.pf(f, "%a.%s", pp_primaryexpr, prim, id);
    }
  | ArrAccess(prim, e) => {
      Fmt.pf(f, "%a[%a]", pp_primaryexpr, prim, pp_expr, e);
    }
  | Slice(prim, e1op, e2op) => {
      Fmt.pf(
        f,
        "%a[%a:%a]",
        pp_primaryexpr,
        prim,
        Fmt.option(pp_expr),
        e1op,
        Fmt.option(pp_expr),
        e2op,
      );
    }
  | SliceCap(prim, eop, e1, e2) => {
      Fmt.pf(
        f,
        "%a[%a:%a:%a]",
        pp_primaryexpr,
        prim,
        Fmt.option(pp_expr),
        eop,
        pp_expr,
        e1,
        pp_expr,
        e2,
      );
    }
  | FunApp(prim, el) => {
      Fmt.pf(f, "%a(@;<0 2>@[%a@])", pp_primaryexpr, prim, pp_exprlist, el);
    }
  | Cast(tp, e) => {
      Fmt.pf(f, "%a(%a)", pp_typ, tp, pp_expr, e);
    }
  | TAssert(tp, e) => {
      Fmt.pf(f, "%a.(%a)", pp_primaryexpr, e, pp_typ, tp);
    }
  | Closure(vsslo, tpo, sl) => {
      Fmt.pf(
        f,
        "func (%a) %a %a",
        Fmt.option(pp_varspecsimp_list),
        vsslo,
        Fmt.option(pp_typ),
        tpo,
        pp_block,
        sl,
      );
    }
  | Nil => Fmt.pf(f, "nil")

and pp_operand = f =>
  fun
  | Parens(e) => {
      Fmt.pf(f, "(%a)", pp_expr, e);
    }
  | Var(id) => Fmt.pf(f, "%s", id)
  | IntLit(i) => Fmt.pf(f, "%d", i)
  | FloatLit(fl) =>
    Fmt.pf(f, "%s", Float.to_string(fl) /* %f loses precision */)
  | RuneLit(r) => Fmt.pf(f, "%s", r)
  | StrLit(s) => Fmt.pf(f, "\"%s\"", escape_string(s))
  | BoolLit(b) => Fmt.pf(f, "%b", b)
  | ArrayLit(typ, vals) => {
      Fmt.pf(
        f,
        "[]%a{@;<0 2>%a}",
        pp_typ,
        typ,
        Fmt.(list(~sep=comma, pp_expr)),
        vals,
      );
    }

and pp_lvalue = f =>
  fun
  | LSel(prim, id) => {
      Fmt.pf(f, "%a.%s", pp_primaryexpr, prim, id);
    }
  | LArrAccess(prim, e) => {
      Fmt.pf(f, "%a[%a]", pp_primaryexpr, prim, pp_expr, e);
    }
  | LSlice(prim, eop1, eop2) => {
      Fmt.pf(
        f,
        "%a[%a:%a]",
        pp_primaryexpr,
        prim,
        Fmt.option(pp_expr),
        eop1,
        Fmt.option(pp_expr),
        eop2,
      );
    }
  | LSliceCap(prim, eop, e1, e2) => {
      Fmt.pf(
        f,
        "%a[%a:%a:%a]",
        pp_primaryexpr,
        prim,
        Fmt.option(pp_expr),
        eop,
        pp_expr,
        e1,
        pp_expr,
        e2,
      );
    }

and pp_lvaluelist = f =>
  fun
  | [] => failwith("Lvaluelist should not be empty")
  | l => Fmt.pf(f, "@[<hov>@,%a@]", Fmt.(list(~sep=comma, pp_lvalue)), l)

and pp_assignop = f =>
  fun
  | PlusEq => Fmt.pf(f, "+=")
  | MinusEq => Fmt.pf(f, "-=")
  | TimesEq => Fmt.pf(f, "*=")
  | DivEq => Fmt.pf(f, "/=")
  | ModEq => Fmt.pf(f, "%%=")
  | AndEq => Fmt.pf(f, "&=")
  | OrEq => Fmt.pf(f, "|=")
  | XOrEq => Fmt.pf(f, "^=")
  | LShiftEq => Fmt.pf(f, "<<=")
  | RShiftEq => Fmt.pf(f, ">>=")
  | ClrEq => Fmt.pf(f, "&^=")

and pp_simplestmt = f =>
  fun
  | Expr(e) => pp_expr(f, e)
  | Inc(e) => {
      Fmt.pf(f, "%a++", pp_expr, e);
    }
  | Dec(e) => {
      Fmt.pf(f, "%a--", pp_expr, e);
    }
  | AssignEquals(lvl, el) => {
      Fmt.pf(f, "%a = %a", pp_lvaluelist, lvl, pp_exprlist, el);
    }
  | Assign(assop, lv, e) => {
      pp_lvalue(f, lv);
      Fmt.pf(f, " %a ", pp_assignop, assop);
      pp_expr(f, e);
    }
  | AssignVarEquals(idl, el) => {
      Fmt.pf(f, "%a = %a", pp_idlist, idl, pp_exprlist, el);
    }
  | AssignVar(assop, v, e) => {
      Fmt.pf(f, "%s %a %a", v, pp_assignop, assop, pp_expr, e);
    }
  | ShortVarDecl(idl, el) => {
      Fmt.pf(f, "%a := %a", pp_idlist, idl, pp_exprlist, el);
    }

and pp_varspec = f =>
  fun
  | VarSpecTyp(vss, elo) => {
      pp_varspecsimp(f, vss);
      switch (elo) {
      | Some(el) => Fmt.pf(f, " = %a", pp_exprlist, el)
      | None => ()
      };
    }
  | VarSpecNoTyp(idl, el) => {
      Fmt.pf(f, "%a = %a", pp_idlist, idl, pp_exprlist, el);
    }

and pp_typespec = f =>
  fun
  | TypSpec(t, tp) => {
      Fmt.pf(f, "%s %a", t, pp_typ, tp);
    }

and pp_varspecsemi_list = (f, vsl) =>
  Fmt.pf(f, "@[@,%a@]", Fmt.(list(~sep=semi, pp_varspec)), vsl)

and pp_typespecsemi_list = (f, tsl) =>
  List.iter(tsl, ~f=ts => pp_typespec(f, ts))

and pp_declstmt = f =>
  fun
  | VarDecls([]) => Fmt.pf(f, "@,")
  | VarDecls([varspec]) => {
      Fmt.pf(f, "var %a", pp_varspec, varspec);
    }
  | VarDecls(vsl) => {
      Fmt.pf(f, "var (%a)", pp_varspecsemi_list, vsl);
    }
  | TypeDecls([typespec]) => {
      Fmt.pf(f, "type %a", pp_typespec, typespec);
    }
  | TypeDecls(tsl) => {
      Fmt.pf(f, "type (%a)", pp_typespecsemi_list, tsl);
    }

and pp_ifcond = f =>
  fun
  | IfCond(Some(ss), e) => {
      Fmt.pf(f, "%a; %a", pp_simplestmt, ss, pp_expr, e);
    }
  | IfCond(None, e) => pp_expr(f, e)

and pp_switch_cond = f =>
  fun
  | SwitchCond(None, None) =>
    failwith("Switch condition lacks both statement and expression")
  | SwitchCond(None, eo) => Fmt.pf(f, "%a", Fmt.option(pp_expr), eo)
  | SwitchCond(Some(stmt), eo) => {
      Fmt.pf(f, "%a; %a", pp_simplestmt, stmt, Fmt.option(pp_expr), eo);
    }

and pp_exprswitchcase = f =>
  fun
  | Default => Fmt.pf(f, "default")
  | Case(el) => {
      Fmt.pf(f, "case %a", pp_exprlist, el);
    }

and pp_printstmt = f =>
  fun
  | PrintStmt(Some(el)) => {
      Fmt.pf(f, "print(%a)", pp_exprlist, el);
    }
  | PrintStmt(None) => {
      Fmt.pf(f, "print()");
    }
  | PrintlnStmt(Some(el)) => {
      Fmt.pf(f, "println(%a)", pp_exprlist, el);
    }
  | PrintlnStmt(None) => {
      Fmt.pf(f, "println()");
    }

and pp_stmt = f =>
  fun
  | Decl(loc, declstmt) => {
      pp_debug_info(f, loc);
      pp_declstmt(f, declstmt);
    }
  | Simple(loc, simplestmt) => {
      pp_debug_info(f, loc);
      pp_simplestmt(f, simplestmt);
    }
  | Return(loc, eo) => {
      pp_debug_info(f, loc);
      Fmt.pf(f, "return%a", Fmt.(option(any(" ") ++ pp_expr)), eo);
    }
  | Break(loc, lblo) => {
      pp_debug_info(f, loc);
      Fmt.pf(f, "break%a", Fmt.(option(any(" ") ++ string)), lblo);
    }
  | Continue(loc, lblo) => {
      pp_debug_info(f, loc);
      Fmt.pf(f, "continue%a", Fmt.(option(any(" ") ++ string)), lblo);
    }
  | Fallthrough(loc) => {
      pp_debug_info(f, loc);
      Fmt.pf(f, "fallthrough");
    }
  | Block(loc, sl) => {
      pp_debug_info(f, loc);
      pp_block(f, sl);
    }
  | InlineBlock(loc, sl) => {
      pp_debug_info(f, loc);
      pp_stmtlist(f, sl);
    }
  | If(loc, ifstmt) => {
      pp_debug_info(f, loc);
      pp_ifstmt(f, ifstmt);
    }
  | Switch(loc, switchstmt) => {
      pp_debug_info(f, loc);
      pp_switchstmt(f, switchstmt);
    }
  | For(loc, forstmt) => {
      pp_debug_info(f, loc);
      pp_forstmt(f, forstmt);
    }
  | Print(loc, printstmt) => {
      pp_debug_info(f, loc);
      pp_printstmt(f, printstmt);
    }
  | RawStmt(loc, code) => {
      pp_debug_info(f, loc);
      Fmt.pf(f, "%s", code);
    }
  | EmptyStmt(loc) => ()
  | LabelledStmt(loc, lbl, stmt) => {
      pp_debug_info(f, loc);
      Fmt.pf(f, "%s:%a", lbl, pp_stmt, stmt);
    }

and pp_stmtlist = (f, sl) =>
  Fmt.pf(f, "@[<v>%a@,@]", Fmt.(list(pp_stmt)), sl)

and simplify_block =
  fun
  | [Block(_, sl)] => sl
  | sl => sl

and pp_block = (f, sl) => {
  let sl = simplify_block(sl);
  Fmt.pf(f, "{@;<0 2>%a}", pp_stmtlist, sl);
}

and pp_ifstmt = (f, is) => {
  switch (is) {
  | IfOnly(ifcond, sl) =>
    Fmt.pf(f, "if %a %a", pp_ifcond, ifcond, pp_block, sl)
  | IfElse(ifcond, b1, b2) =>
    let b1 = simplify_block(b1);
    let b2 = simplify_block(b2);
    Fmt.pf(
      f,
      "if %a %a else %a",
      pp_ifcond,
      ifcond,
      pp_block,
      b1,
      pp_block,
      b2,
    );
  | IfElseIf(ifcond, block, ifstmt) =>
    Fmt.pf(
      f,
      "if %a %a else %a",
      pp_ifcond,
      ifcond,
      pp_block,
      block,
      pp_ifstmt,
      ifstmt,
    )
  };
}

and pp_switchstmt = f =>
  fun
  | SwitchStmt(sco, eccl) => {
      Fmt.pf(
        f,
        "switch%a {@[<v 2>@,%a@,@]}",
        Fmt.(option(sp ++ pp_switch_cond)),
        sco,
        Fmt.(list(pp_exprcaseclause)),
        eccl,
      );
    }

and pp_exprcaseclause = f =>
  fun
  | ExprCaseClause(esc, stmtlist) => {
      Fmt.pf(
        f,
        "%a:@[<v 2>@,%a@,@]",
        pp_exprswitchcase,
        esc,
        pp_stmtlist,
        stmtlist,
      );
    }

and pp_forstmt = (f, fs) => {
  switch (fs) {
  | ForLoop(None, None, None, stmtlist)
  | InfLoop(stmtlist) => Fmt.pf(f, "for %a", pp_block, stmtlist)
  | WhileLoop(e, stmtlist) =>
    Fmt.pf(f, "for %a %a", pp_expr, e, pp_block, stmtlist)
  | ForLoop(s1, eo, s2, stmtlist) =>
    Fmt.pf(
      f,
      "for %a; %a; %a %a",
      Fmt.option(pp_simplestmt),
      s1,
      Fmt.option(pp_expr),
      eo,
      Fmt.option(pp_simplestmt),
      s2,
      pp_block,
      stmtlist,
    )
  };
};

let pp_package = f =>
  fun
  | Package(_, name) => {
      Fmt.pf(f, "@[<h>package %s@]@,", name);
    };

let pp_topleveldecl = f =>
  fun
  | Import(_p, name, aso) => {
      let pp_as = (f, as_) => Fmt.pf(f, " %s", as_);
      Fmt.pf(f, "@[<h>import%a \"%s\"@]@,", Fmt.option(pp_as), aso, name);
    }
  | FuncDecl(_p, i, vsslo, tpo, sl) => {
      Fmt.pf(
        f,
        "func %s(%a) %a %a",
        i,
        Fmt.option(pp_varspecsimp_list),
        vsslo,
        Fmt.option(pp_typ),
        tpo,
        pp_block,
        sl,
      );
    }
  | Stmt(_, stmt) => {
      pp_stmt(f, stmt);
    };

let pp_program = f =>
  fun
  | Program(_, pkg, tldl) => {
      Fmt.pf(
        f,
        "%a@,%a@,",
        pp_package,
        pkg,
        Fmt.list(pp_topleveldecl),
        tldl,
      );
    };

let program = p => {
  let buf = Buffer.create(16);
  let f = Fmt.with_buffer(buf);
  Fmt.pf(f, "@[<v>%a@]@.", pp_program, p);
  Buffer.contents(buf);
};
