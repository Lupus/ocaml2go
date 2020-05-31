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
[@deriving sexp]
type id = string;

[@deriving sexp]
type typ =
  | Int
  | Float64
  | Bool
  | Rune
  | String
  | Void
  | FuncTyp(list(typ), typ)
  | TypeVar(id)
  | TArray(int, typ)
  | TMap(typ, typ)
  | TSlice(typ)
  | TStruct(list(varspecsimp))
  | TRef(string)
  | TAny

and varspecsimp = (list(id), typ);

[@deriving sexp]
type binop =
  | LOr
  | LAnd
  | CmpEq
  | NotEq
  | LT
  | GT
  | LTE
  | GTE
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | BitOr
  | BitAnd
  | BitXOr
  | BitClr
  | LShift
  | RShift;

[@deriving sexp]
type uop =
  | UPlus
  | UMinus
  | LNot
  | UBitXor
  | AddrOf;

[@deriving sexp]
type raw_segment =
  | RawText(string)
  | RawSubstitution(expr)

and expr =
  | Unary(unaryexpr)
  | Binary(binop, expr, expr)
  | Raw(list(raw_segment))

and unaryexpr =
  | Primary(primaryexpr)
  | UnaryOp(uop, unaryexpr)

and primaryexpr =
  | Operand(operand)
  | Sel(primaryexpr, id)
  | ArrAccess(primaryexpr, expr)
  | Slice(primaryexpr, option(expr), option(expr))
  | SliceCap(primaryexpr, option(expr), expr, expr)
  | FunApp(primaryexpr, list(expr))
  | Cast(typ, expr)
  | TAssert(typ, primaryexpr)
  | Closure(option(list(varspecsimp)), option(typ), list(stmt))
  | Nil

and operand =
  | Parens(expr)
  | Var(id)
  | IntLit(int)
  | FloatLit(float)
  | RuneLit(string)
  | StrLit(string)
  | BoolLit(bool)
  | ArrayLit(typ, list(expr))

and lvalue =
  | LSel(primaryexpr, id)
  | LArrAccess(primaryexpr, expr)
  | LSlice(primaryexpr, option(expr), option(expr))
  | LSliceCap(primaryexpr, option(expr), expr, expr)

and assignop =
  /* doesn't contain Equals because we handle it separately */
  | PlusEq
  | MinusEq
  | TimesEq
  | DivEq
  | ModEq
  | AndEq
  | OrEq
  | XOrEq
  | LShiftEq
  | RShiftEq
  | ClrEq

and simplestmt =
  | Expr(expr)
  | Inc(expr)
  | Dec(expr)
  | Assign(assignop, lvalue, expr)
  | AssignEquals(list(lvalue), list(expr))
  | AssignVar(assignop, id, expr)
  | AssignVarEquals(list(id), list(expr))
  | ShortVarDecl(list(id), list(expr))

and varspec =
  | VarSpecTyp(varspecsimp, option(list(expr)))
  | VarSpecNoTyp(list(id), list(expr))

and typespec =
  | TypSpec(id, typ)

and declstmt =
  | VarDecls(list(varspec))
  | TypeDecls(list(typespec))

and ifcond =
  | IfCond(option(simplestmt), expr)

and switchcond =
  | SwitchCond(option(simplestmt), option(expr))

and exprswitchcase =
  | Case(list(expr))
  | Default

and printstmt =
  | PrintStmt(option(list(expr)))
  | PrintlnStmt(option(list(expr)))

and stmt =
  | Decl(IR.loc, declstmt)
  | Simple(IR.loc, simplestmt)
  | Return(IR.loc, option(expr))
  | Break(IR.loc, option(string))
  | Continue(IR.loc, option(string))
  | Fallthrough(IR.loc)
  | Block(IR.loc, list(stmt))
  | InlineBlock(IR.loc, list(stmt))
  | If(IR.loc, ifstmt)
  | Switch(IR.loc, switchstmt)
  | For(IR.loc, forstmt)
  | Print(IR.loc, printstmt)
  | RawStmt(IR.loc, string)
  | EmptyStmt(IR.loc)
  | LabelledStmt(IR.loc, string, stmt)

and ifstmt =
  | IfOnly(ifcond, list(stmt))
  | IfElse(ifcond, list(stmt), list(stmt))
  | IfElseIf(ifcond, list(stmt), ifstmt)

and switchstmt =
  | SwitchStmt(option(switchcond), list(exprcaseclause))

and exprcaseclause =
  | ExprCaseClause(exprswitchcase, list(stmt))

and forstmt =
  | InfLoop(list(stmt))
  | WhileLoop(expr, list(stmt))
  | ForLoop(
      option(simplestmt),
      option(expr),
      option(simplestmt),
      list(stmt),
    );

[@deriving sexp]
type package =
  | Package(IR.loc, id);

[@deriving sexp]
type topleveldecl =
  | Import(IR.loc, string, option(string))
  | FuncDecl(
      IR.loc,
      id,
      option(list(varspecsimp)),
      option(typ),
      list(stmt),
    )
  | Stmt(IR.loc, stmt);

[@deriving sexp]
type program =
  | Program(IR.loc, package, list(topleveldecl));
