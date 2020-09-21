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

module Acc = {
  type var_rewrite = {
    from: string,
    to_: string,
  };
  type t = {
    local_labels: Set.t(string, String.comparator_witness),
    is_in_for_loop: bool,
    is_in_switch: bool,
    last_cont_id: int,
    continuations: List.Assoc.t(int, IR.statement),
    new_locals: list((string, IR.typ)),
  };

  let empty = {
    local_labels: Set.empty((module String)),
    is_in_for_loop: false,
    is_in_switch: false,
    last_cont_id: 0,
    continuations: [],
    new_locals: [],
  };

  let with_loop = (t, ~f) => {
    let t' = {...t, is_in_for_loop: true};
    let (x, t'') = f(t');
    (x, {...t'', is_in_for_loop: t.is_in_for_loop});
  };

  let with_switch = (t, ~f) => {
    let t' = {...t, is_in_switch: true};
    let (x, t'') = f(t');
    (x, {...t'', is_in_switch: t.is_in_switch});
  };

  let with_label = (t, ~f, ~label) => {
    let t' = {...t, local_labels: Set.add(t.local_labels, label)};
    let (x, t'') = f(t');
    (x, {...t'', local_labels: t.local_labels});
  };

  let add_continuation = (t, s) => {
    let id = t.last_cont_id + 1;
    (
      id,
      {
        ...t,
        last_cont_id: id,
        continuations: [(id, s), ...t.continuations],
      },
    );
  };

  let add_local = (t, v) => {
    {...t, new_locals: [(v, TValue), ...t.new_locals]};
  };
};

let is_try =
  fun
  | STry(_) => true
  | _ => false;

let fresh = {
  let last_id = ref(0);
  () => {
    Int.incr(last_id);
    Printf.sprintf("__exn_rw_%d__", last_id^);
  };
};

let dispatch_continuations = (cont_expr, conts, has_default_case) => {
  let cases =
    List.map(conts, ~f=((id, stmt)) => {
      {
        case_expr: {
          expr_desc: EInt(id),
          expr_typ: IR.TInt,
        },
        case_stmts: [(stmt, N)],
      }
    });
  let cases =
    if (has_default_case) {
      [
        {
          case_expr: {
            expr_desc: EInt(0),
            expr_typ: IR.TInt,
          },
          case_stmts: [(SBreak(None), N)],
        },
        ...cases,
      ];
    } else {
      cases;
    };
  SSwitch((
    cont_expr,
    cases,
    [
      (
        SExpression({
          expr_desc:
            ERaw([
              RawText(
                {|panic("bug: unreachable exn continuation dispatch case")|},
              ),
            ]),
          expr_typ: IR.TUnit,
        }),
        N,
      ),
    ],
  ));
};

class id_renamer = {
  as self;
  inherit class Traverse_builtins.map_with_context(Acc.var_rewrite);
  inherit class map_with_context(Acc.var_rewrite) as super;
  pub function_expression = (Acc.{from, _} as ctx, fe) => {
    if (List.mem(fe.locals, (from, IR.TValue), ~equal=((id1, _), (id2, _)) =>
          String.equal(id1, id2)
        )) {
      failwith(
        Printf.sprintf(
          "var renaming is going to rename local variable %s in the following function: %{sexp:IR.function_expression}"
            ^,
          from,
          fe,
        ),
      );
    };
    if (List.mem(fe.params, (from, IR.TValue), ~equal=((id1, _), (id2, _)) =>
          String.equal(id1, id2)
        )) {
      failwith(
        Printf.sprintf(
          "var renaming is going to rename param %s in the following function: %{sexp:IR.function_expression}"
            ^,
          from,
          fe,
        ),
      );
    };
    super#function_expression(ctx, fe);
  };
  pub expression = (Acc.{from, to_} as ctx, e) => {
    switch (e) {
    | {expr_desc: EVar(x), expr_typ} when String.(x == from) => {
        expr_desc: EVar(to_),
        expr_typ,
      }
    | _ => super#expression(ctx, e)
    };
  };
  pub lvalue = (Acc.{from, to_} as ctx, lv) => {
    switch (lv) {
    | {lv_desc: LVar(x), lv_typ} when String.(x == from) => {
        lv_desc: LVar(to_),
        lv_typ,
      }
    | _ => super#lvalue(ctx, lv)
    };
  };
};

class rewriter = {
  as self;
  inherit class Traverse_builtins.fold_map(Acc.t);
  inherit class fold_map(Acc.t) as super;
  pub function_expression = (fe, acc) => {
    (fe, acc);
  };
  pub statement = (s, acc) => {
    let rewrite_cont = s => {
      let (cont_id, acc) = Acc.add_continuation(acc, s);
      let cont_success = {expr_desc: EInt(cont_id), expr_typ: IR.TInt};
      (SReturn(Some(cont_success)), acc);
    };
    switch (s) {
    | SContinue(Some(lbl)) when !Set.mem(acc.local_labels, lbl) =>
      rewrite_cont(s)
    | SContinue(None) when !acc.is_in_for_loop => rewrite_cont(s)
    | SBreak(Some(lbl)) when !Set.mem(acc.local_labels, lbl) =>
      rewrite_cont(s)
    | SBreak(None) when !(acc.is_in_for_loop || acc.is_in_switch) =>
      rewrite_cont(s)
    | SReturn(_) => rewrite_cont(s)
    | SLoop(_) => Acc.with_loop(acc, ~f=acc => super#statement(s, acc))
    | SSwitch(_) => Acc.with_switch(acc, ~f=acc => super#statement(s, acc))
    | SLabelled((label, _)) =>
      Acc.with_label(acc, ~f=acc => super#statement(s, acc), ~label)
    | STry(_) => failwith("unexpected nested try for rewriter")
    | _ => super#statement(s, acc)
    };
  };
  pub rewrite_try = (s, l, acc) => {
    let acc =
      Acc.{
        ...acc,
        local_labels: Set.empty((module String)),
        is_in_for_loop: false,
        is_in_switch: false,
      };
    let (b1, (id, b2)) =
      switch (s) {
      | STry((b1, (id, b2))) => (b1, (id, b2))
      | _ => failwith("not an STry statement")
      };
    let var = fresh();
    let new_id = fresh();
    let acc = Acc.add_local(acc, new_id);
    let b2 =
      (new id_renamer)#statement_list(Acc.{from: id, to_: new_id}, b2);
    let id = new_id;
    let acc = Acc.add_local(acc, var);
    let caml_try = {
      expr_desc:
        EAccess({
          eacc_expr: {
            expr_desc: EVar("runtime"),
            expr_typ: IR.TValue,
          },
          eacc_index: {
            expr_desc: EStr({estr_lit: "caml_try", estr_kind: Bytes}),
            expr_typ: IR.TString,
          },
        }),
      expr_typ: TValue,
    };
    let (body_sl, acc) = self#statement_list(b1, acc);
    let cont_success = {expr_desc: EInt(0), expr_typ: IR.TInt};
    let (body_sl, has_default_case) =
      switch (List.rev(body_sl)) {
      | [(SReturn(_), _), ..._] => (body_sl, false)
      | _ => (body_sl @ [(SReturn(Some(cont_success)), N)], true)
      };
    let try_closure = {
      expr_desc:
        EFun({
          locals: [],
          params: [],
          body: body_sl |> List.map(~f=((s, l)) => (SEStatement(s), l)),
          loc: N,
        }),
      expr_typ: TValue,
    };
    let try_apply = {
      expr_desc: ECall({ecall_expr: caml_try, ecall_args: [try_closure]}),
      expr_typ: TValue,
    };
    let try_assign =
      SAssignment({
        sassign_lvalue: {
          lv_desc: LVar(var),
          lv_typ: TValue,
        },
        sassign_expr: try_apply,
      });
    let try_is_exn = {
      expr_desc:
        EStructAccess({
          estruct_expr: {
            expr_desc: EVar(var),
            expr_typ: TValue,
          },
          estruct_index: 0,
        }),
      expr_typ: TValue,
    };
    let try_value = {
      expr_desc:
        EStructAccess({
          estruct_expr: {
            expr_desc: EVar(var),
            expr_typ: TValue,
          },
          estruct_index: 1,
        }),
      expr_typ: TValue,
    };
    let catch_var_assign =
      SAssignment({
        sassign_lvalue: {
          lv_desc: LVar(id),
          lv_typ: TValue,
        },
        sassign_expr: try_value,
      });
    let catch_block = SBlock([(catch_var_assign, l), ...b2]);
    let check_cont =
      Some((
        dispatch_continuations(
          try_value,
          acc.continuations,
          has_default_case,
        ),
        N,
      ));
    let check_exn =
      SIf((
        {
          expr_desc:
            EBin({
              ebin_op: BNotEqEq,
              ebin_lhs: try_is_exn,
              ebin_rhs: {
                expr_desc: EInt(0),
                expr_typ: TInt,
              },
            }),
          expr_typ: TBool,
        },
        (catch_block, l),
        check_cont,
      ));
    let extra_sl = [(check_exn, l)];
    ([(try_assign, l), ...extra_sl], acc);
  };
};

class scanner = {
  as self;
  inherit class Traverse_builtins.fold_map(Acc.t);
  inherit class fold_map(Acc.t) as super;
  pub program = (prg, acc) => {
    let (prg, acc') = super#program(prg, Acc.empty);
    let locals = prg.prg_locals @ acc'.new_locals;
    ({...prg, prg_locals: locals}, acc);
  };
  pub function_expression = (fe, acc) => {
    let (body, acc') = self#function_body(fe.body, Acc.empty);
    let locals = fe.locals @ acc'.new_locals;
    ({...fe, body, locals}, acc);
  };
  pub statement = (s, acc) => {
    switch (s) {
    | STry(_) =>
      let (s, acc) = super#statement(s, acc);
      let (sl, acc') = (new rewriter)#rewrite_try(s, U, Acc.empty);
      let acc = Acc.{...acc, new_locals: acc.new_locals @ acc'.new_locals};
      (SBlock(sl), acc);
    | _ => super#statement(s, acc)
    };
  };
};

let run = ir => (new scanner)#program(ir, Acc.empty) |> fst;
