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

module G = {
  module Vertex = {
    [@deriving (sexp, compare, equal, hash)]
    type kind =
      | Unit
      | Int
      | Float
      | Bool
      | String
      | Struct
      | Array
      | Var(string)
      | Fun
      | Any;
    [@deriving sexp]
    type t = {
      id: int,
      kind,
      orig_typ: option(IR.typ),
      comment: option(string),
    };
    let compare = (a, b) => [%compare: int](a.id, b.id);
    let hash = a => [%hash: int](a.id);
    let equal = (a, b) => [%equal: int](a.id, b.id);
    let default = {id: 0, kind: Unit, orig_typ: None, comment: None};
    let last_id = ref(1);
    let var_mapping = Hashtbl.create((module String));
    let fresh = (~orig_typ=?, ~comment=?, kind) => {
      let aux = () => {
        let id = last_id^;
        Int.incr(last_id);
        {id, kind, orig_typ, comment};
      };
      switch (kind) {
      | Var(x) =>
        switch (Hashtbl.find(var_mapping, x)) {
        | Some(v) => v
        | None =>
          let v = aux();
          Hashtbl.add_exn(var_mapping, ~key=x, ~data=v);
          v;
        }
      | _ => aux()
      };
    };
    let of_typ = (~comment=?, typ) => {
      let kind =
        switch (typ) {
        | IR.TUnit => Unit
        | TInt => Int
        | TFloat => Float
        | TBool => Bool
        | TString => String
        | TVar(x) => Var(x)
        | TFun(_) =>
          failwith(
            "TFun should not appear in expressions during constraint collection",
          )
        | TStruct =>
          failwith(
            "TStruct should not appear in expressions during constraint collection",
          )
        | TArray => Array
        | TAny(_) =>
          failwith(
            "TAny should not appear in expressions during constraint collection",
          )
        };
      fresh(~orig_typ=typ, ~comment?, kind);
    };
    let to_typ =
      fun
      | {kind: Unit, _} => IR.TUnit
      | {kind: Int, _} => IR.TInt
      | {kind: Float, _} => IR.TFloat
      | {kind: Bool, _} => IR.TBool
      | {kind: String, _} => IR.TString
      | {kind: Struct, _} => IR.TStruct
      | {kind: Array, _} => IR.TArray
      | {kind: Var(x), _} => IR.TAny(x)
      | {kind: Fun, id} => IR.TAny(Printf.sprintf("v%d", id))
      | {kind: Any, id} => IR.TAny(Printf.sprintf("v%d", id));
  };
  module Edge = {
    [@deriving (sexp, compare, equal, hash)]
    type t =
      | Arg(int)
      | Returns
      | Field(int)
      | Constraint;
    let default = Constraint;
  };
  module X = Graph.Imperative.Digraph.ConcreteLabeled(Vertex, Edge);
  include Graph.Merge.I(X);
  include X;
};

module Reachability =
  Graph.Fixpoint.Make(
    G,
    {
      type vertex = G.E.vertex;
      type edge = G.E.t;
      type g = G.t;
      type data = bool;
      let direction = Graph.Fixpoint.Forward;
      let equal = Bool.equal;
      let join = (||);
      let analyze = (_, x) => x;
    },
  );

module Display = {
  include G;
  let vertex_name =
    fun
    | {G.Vertex.kind: G.Vertex.Unit, _} => "unit"
    | {kind: Int, _} => "int"
    | {kind: Float, _} => "float"
    | {kind: Bool, _} => "bool"
    | {kind: String, _} => "string"
    | {kind: Struct, id} => Printf.sprintf("\"struct (%d)\"", id)
    | {kind: Array, _} => "array"
    | {kind: Var(x), _} => x
    | {kind: Fun, id} => Printf.sprintf("\"-> (%d)\"", id)
    | {kind: Any, id} => Printf.sprintf("\"??? (%d)\"", id);

  let graph_attributes = _ => [`Overlap(false), `Spline(true)];
  let default_vertex_attributes = _ => [];
  let vertex_attributes = _ => [];
  let default_edge_attributes = _ => [];
  let edge_attributes = e => {
    switch (e) {
    | (_, G.Edge.Arg(x), _) => [
        `HtmlLabel(Printf.sprintf({|<font color="blue">Arg(%d)</font>|}, x)),
      ]
    | (_, Returns, _) => [`HtmlLabel({|<font color="red">Returns</font>|})]
    | (_, Field(x), _) => [
        `HtmlLabel(
          Printf.sprintf({|<font color="green">Field(%d)</font>|}, x),
        ),
      ]
    | (_, Constraint, _) => [`Style(`Dashed)]
    };
  };
  let get_subgraph = _ => None;
};

module Gv = Graph.Graphviz.Neato(Display);

module Env = {
  type t = Map.t(string, G.vertex, String.comparator_witness);
  let empty = Map.empty((module String));
  let get = (t, x) =>
    Map.find(t, x)
    |> Option.value_exn(
         ~message=Printf.sprintf("variable %s is not defined", x),
         _,
       );
  let add_vars = (t, vars) =>
    List.fold_left(vars, ~init=t, ~f=(m, (name, typ)) =>
      Map.remove(m, name) |> Map.add_exn(_, name, G.Vertex.of_typ(typ))
    );
};

module Acc = {
  type t = {
    env: Env.t,
    g: G.t,
    fn_return_v: option(G.vertex),
  };
  let create = () => {
    let env = Env.empty;
    let g = G.create();
    {env, g, fn_return_v: None};
  };
};

class collect_constraints = {
  as self;
  inherit class Traverse_builtins.fold(Acc.t);
  inherit class fold(Acc.t) as super;
  val mutable first_statement = true;
  pub expression = (e, acc) => {
    let expr_v = G.Vertex.of_typ(e.expr_typ);
    G.add_vertex(acc.g, expr_v);
    switch (e.expr_desc) {
    | IR.ECall({ecall_expr: e, ecall_args: args}) =>
      let acc = self#expression(e, acc);
      let acc = self#arguments(args, acc);
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let fn_v = G.Vertex.(fresh(Fun));
      List.iteri(
        args,
        ~f=(i, arg) => {
          let arg_v = G.Vertex.of_typ(arg.expr_typ);
          let edge = G.E.create(fn_v, G.Edge.Arg(i), arg_v);
          G.add_edge_e(acc.g, edge);
        },
      );
      let edge = G.E.create(fn_v, G.Edge.Returns, expr_v);
      G.add_edge_e(acc.g, edge);
      G.add_edge(acc.g, e_v, fn_v);
      acc;
    | EVar(id) =>
      let var_v = Env.get(acc.env, id);
      G.add_edge(acc.g, expr_v, var_v);
      acc;
    | EBool(_) =>
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TBool));
      acc;
    | EFloat(_) =>
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TFloat));
      acc;
    | EInt(_) =>
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
      acc;
    | EStr(_) =>
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TString));
      acc;
    | ETag({etag_tag: _, etag_items: args}) =>
      let acc = self#arguments(args, acc);
      let struct_v = G.Vertex.(fresh(Struct));
      let edge =
        G.E.create(struct_v, G.Edge.Field(0), G.Vertex.of_typ(TInt));
      G.add_edge_e(acc.g, edge);
      List.iteri(
        args,
        ~f=(i, arg) => {
          let arg_v = G.Vertex.of_typ(arg.expr_typ);
          let edge = G.E.create(struct_v, G.Edge.Field(i + 1), arg_v);
          G.add_edge_e(acc.g, edge);
        },
      );
      G.add_edge(acc.g, expr_v, struct_v);
      acc;
    | EStruct(args) =>
      let acc = self#arguments(args, acc);
      let struct_v = G.Vertex.(fresh(Struct));
      List.iteri(
        args,
        ~f=(i, arg) => {
          let arg_v = G.Vertex.of_typ(arg.expr_typ);
          let edge = G.E.create(struct_v, G.Edge.Field(i), arg_v);
          G.add_edge_e(acc.g, edge);
        },
      );
      G.add_edge(acc.g, expr_v, struct_v);
      acc;
    | EArr(_) =>
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TArray));
      acc;
    | EAccess({eacc_expr: _, eacc_index: _}) =>
      /*let acc = self#expression(eacc_expr, acc);
        let eacc_expr_v = G.Vertex.of_typ(eacc_expr.expr_typ);
        G.add_edge(acc.g, eacc_expr_v, acc.struct_v);*/
      acc
    | EStructAccess({estruct_expr, estruct_index}) =>
      let acc = self#expression(estruct_expr, acc);
      let estruct_expr_v = G.Vertex.of_typ(estruct_expr.expr_typ);
      let struct_v = G.Vertex.(fresh(Struct));
      let field_v = G.Vertex.of_typ(IR.gen_new_type());
      let edge = G.E.create(struct_v, G.Edge.Field(estruct_index), field_v);
      G.add_edge_e(acc.g, edge);
      G.add_edge(acc.g, estruct_expr_v, struct_v);
      G.add_edge(acc.g, expr_v, field_v);
      acc;
    | EArrAccess({earr_expr, _}) =>
      let acc = self#expression(earr_expr, acc);
      let earr_expr_v = G.Vertex.of_typ(earr_expr.expr_typ);
      G.add_edge(acc.g, earr_expr_v, G.Vertex.of_typ(TArray));
      acc;
    | EBin({ebin_op: binop, ebin_lhs: e1, ebin_rhs: e2}) =>
      let acc = self#expression(e1, acc);
      let acc = self#expression(e2, acc);
      let e1_v = G.Vertex.of_typ(e1.expr_typ);
      let e2_v = G.Vertex.of_typ(e2.expr_typ);
      switch (binop) {
      | BOr
      | BAnd =>
        G.add_edge(acc.g, e1_v, G.Vertex.of_typ(TBool));
        G.add_edge(acc.g, e2_v, G.Vertex.of_typ(TBool));
        acc;
      | BIntPlus
      | BPlus
      | BMinus
      | BMul
      | BDiv
      | BMod
      | BLt
      | BLe
      | BGt
      | BGe
      | BBor
      | BBxor
      | BBand
      | BLsl
      | BLsr
      | BAsr =>
        G.add_edge(acc.g, e1_v, G.Vertex.of_typ(TInt));
        G.add_edge(acc.g, e2_v, G.Vertex.of_typ(TInt));
        acc;
      | BEqEq
      | BNotEq => acc
      | BEqEqEq
      | BNotEqEq =>
        G.add_edge(acc.g, e1_v, e2_v);
        acc;
      | BFloatEqEq
      | BFloatNotEq
      | BFloatLt
      | BFloatLe
      | BFloatGt
      | BFloatGe
      | BFloatPlus
      | BFloatMinus
      | BFloatMul
      | BFloatDiv
      | BFloatMod =>
        G.add_edge(acc.g, e1_v, G.Vertex.of_typ(TFloat));
        G.add_edge(acc.g, e2_v, G.Vertex.of_typ(TFloat));
        acc;
      };
    | ECond(_) =>
      failwith("ECond unsupported by inference, please use Econd_rewriter")
    | EArityTest(e) =>
      let acc = self#expression(e, acc);
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
      acc;
    | EVectlength(e) =>
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e, acc);
      let struct_v = G.Vertex.(fresh(Struct));
      G.add_edge(acc.g, e_v, struct_v);
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
      acc;
    | EArrLen(e) =>
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e, acc);
      G.add_edge(acc.g, e_v, G.Vertex.of_typ(TArray));
      G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
      acc;
    | EDot(_) => super#expression(e, acc)
    | ECopy(e) =>
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e, acc);
      G.add_edge(acc.g, expr_v, e_v);
      acc;
    | EUn({eun_op: unop, eun_expr: e}) =>
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e, acc);
      switch (unop) {
      | UNot =>
        G.add_edge(acc.g, e_v, G.Vertex.of_typ(TBool));
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TBool));
        acc;
      | UNeg =>
        G.add_edge(acc.g, e_v, G.Vertex.of_typ(TInt));
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
        acc;
      | UFloatNeg =>
        G.add_edge(acc.g, e_v, G.Vertex.of_typ(TFloat));
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TFloat));
        acc;
      | UIsInt =>
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TBool));
        acc;
      | UToInt =>
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
        acc;
      | UToBool =>
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TBool));
        acc;
      | UIntToString =>
        G.add_edge(acc.g, e_v, G.Vertex.of_typ(TInt));
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TString));
        acc;
      | UFloatToInt =>
        G.add_edge(acc.g, e_v, G.Vertex.of_typ(TFloat));
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TString));
        acc;

      | UBnot =>
        G.add_edge(acc.g, e_v, G.Vertex.of_typ(TInt));
        G.add_edge(acc.g, expr_v, G.Vertex.of_typ(TInt));
        acc;
      };
    | EFun({params, locals, body, loc}) =>
      let ret_v =
        G.Vertex.of_typ(
          IR.gen_new_type(),
          ~comment=Printf.sprintf("return of EFun"),
        );
      let acc = {
        let env' = Env.add_vars(acc.env, params);
        let env' = Env.add_vars(env', locals);
        let acc' = {...acc, fn_return_v: Some(ret_v), env: env'};
        let _: Acc.t = self#function_body(body, acc');
        acc;
      };
      let fn_v = G.Vertex.(fresh(Fun));
      List.iteri(
        params,
        ~f=(i, (_, arg_typ)) => {
          let arg_v = G.Vertex.of_typ(arg_typ);
          let edge = G.E.create(fn_v, G.Edge.Arg(i), arg_v);
          G.add_edge_e(acc.g, edge);
        },
      );
      let edge = G.E.create(fn_v, G.Edge.Returns, ret_v);
      G.add_edge_e(acc.g, edge);
      G.add_edge(acc.g, expr_v, fn_v);
      acc;
    | ESeq((e1, e2)) =>
      let e1_v = G.Vertex.of_typ(e.expr_typ);
      let e2_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e1, acc);
      let acc = self#expression(e2, acc);
      G.add_edge(acc.g, e1_v, G.Vertex.of_typ(TUnit));
      G.add_edge(acc.g, expr_v, e2_v);
      acc;
    | ERaw(_) => acc
    | ERuntime => acc
    | ECustomRequire(_) => failwith("unsupported ECustomRequire")
    | ECustomRegister(_) => failwith("unsupported ECustomRegister")
    | ERequire(_) => failwith("unexpected ERequire")
    };
  };
  pub statement = (s, acc) => {
    switch (s) {
    | SAssignment({sassign_lvalue: {lv_desc, _}, sassign_expr: e}) =>
      let acc = super#statement(s, acc);
      switch (lv_desc) {
      | LVar(id) =>
        let var_v = Env.get(acc.env, id);
        let e_v = G.Vertex.of_typ(e.expr_typ);
        G.add_edge(acc.g, e_v, var_v);
        acc;
      | LStructAccess({estruct_expr, estruct_index}) =>
        let e_v = G.Vertex.of_typ(e.expr_typ);
        let estruct_expr_v = G.Vertex.of_typ(estruct_expr.expr_typ);
        let struct_v = G.Vertex.(fresh(Struct));
        let field_v = G.Vertex.of_typ(IR.gen_new_type());
        let edge =
          G.E.create(struct_v, G.Edge.Field(estruct_index), field_v);
        G.add_edge_e(acc.g, edge);
        G.add_edge(acc.g, estruct_expr_v, struct_v);
        G.add_edge(acc.g, e_v, field_v);
        acc;
      | LArrAccess({earr_expr: e1, earr_index: e2}) =>
        let e1_v = G.Vertex.of_typ(e1.expr_typ);
        let e2_v = G.Vertex.of_typ(e2.expr_typ);
        G.add_edge(acc.g, e1_v, G.Vertex.of_typ(TArray));
        G.add_edge(acc.g, e2_v, G.Vertex.of_typ(TInt));
        acc;
      };
    | SReturn(eo) =>
      let acc = super#statement(s, acc);
      switch (eo, acc.fn_return_v) {
      | (Some(e), Some(fn_return_v)) =>
        let e_v = G.Vertex.of_typ(e.expr_typ);
        G.add_edge(acc.g, e_v, fn_return_v);
        acc;
      | (Some(e), None) =>
        failwith("unexpected return outside of function body")
      | (None, _) => failwith("unexpected return without value")
      };
    | SIf((e, (ifstmt, ifloc), elsopt)) =>
      let acc = super#statement(s, acc);
      let e_v = G.Vertex.of_typ(e.expr_typ);
      G.add_edge(acc.g, e_v, G.Vertex.of_typ(TBool));
      acc;
    | SSwitch((e, case_clause_list, stmt_lst)) =>
      let acc = super#statement(s, acc);
      let e_v = G.Vertex.of_typ(e.expr_typ);
      List.iter(
        case_clause_list,
        ~f=({case_expr, _}) => {
          let case_expr_v = G.Vertex.of_typ(case_expr.expr_typ);
          G.add_edge(acc.g, e_v, case_expr_v);
        },
      );
      acc;
    | _ => super#statement(s, acc)
    };
  };
  pub source_element = (se, acc) => {
    switch (se) {
    | SEStatement(_) => super#source_element(se, acc)
    | SEFunDecl(fdecl) =>
      let fn_v = G.Vertex.(fresh(Fun));
      let fn_var_v = Env.get(acc.env, fdecl.name);
      let ret_v =
        G.Vertex.of_typ(
          IR.gen_new_type(),
          ~comment=Printf.sprintf("return of %s", fdecl.name),
        );
      let acc = {
        let env' = Env.add_vars(acc.env, fdecl.func.params);
        let env' = Env.add_vars(env', fdecl.func.locals);
        let acc' = {...acc, fn_return_v: Some(ret_v), env: env'};
        let _: Acc.t = self#function_body(fdecl.func.body, acc');
        acc;
      };
      List.iteri(
        fdecl.func.params,
        ~f=(i, (_, arg_typ)) => {
          let arg_v =
            G.Vertex.of_typ(
              arg_typ,
              ~comment=Printf.sprintf("arg #%d of %s", i, fdecl.name),
            );
          let edge = G.E.create(fn_v, G.Edge.Arg(i), arg_v);
          G.add_edge_e(acc.g, edge);
        },
      );
      let edge = G.E.create(fn_v, G.Edge.Returns, ret_v);
      G.add_edge_e(acc.g, edge);
      G.add_edge(acc.g, fn_var_v, fn_v);
      acc;
    };
  };
  pub program = (prg, acc) => {
    let env = Env.add_vars(acc.env, prg.prg_locals);
    let acc = {...acc, env};
    super#program(prg, acc);
  };
};

class substitute_types = {
  as self;
  inherit class Traverse_builtins.map_with_context(Hashtbl.t(string, IR.typ));
  inherit class map_with_context(Hashtbl.t(string, IR.typ)) as super;
  pub typ = (tbl, typ) => {
    switch (typ) {
    | IR.TVar(x) =>
      switch (Hashtbl.find(tbl, x)) {
      | Some(new_typ) => new_typ
      | None => typ
      }
    | _ => typ
    };
  };
};

let last_fn_constraing_id = ref(0);
let output_fn_constraint = (g, v1, v2) => {
  let is_root_vertex = v =>
    [%equal: G.Vertex.t](v, v1) || [%equal: G.Vertex.t](v, v2);
  let reachable_from_roots = Reachability.analyze(is_root_vertex, g);
  let g' = G.create();
  g
  |> G.iter_edges_e(((v1, elb, v2) as e) => {
       switch (elb) {
       | Constraint => ()
       | _ =>
         if (reachable_from_roots(v1) && reachable_from_roots(v2)) {
           G.add_edge_e(g', e);
         }
       }
     });
  let id = last_fn_constraing_id^;
  Int.incr(last_fn_constraing_id);
  let out =
    Stdio.Out_channel.create(Printf.sprintf("fn_constraint_%d.dot", id));
  Gv.output_graph(out, g');
  Stdio.Out_channel.close(out);
};

let get_args_and_ret = (g, v) => {
  let (args, ret) =
    G.fold_succ_e(
      (e, (args, ret)) => {
        switch (e) {
        | (_, Arg(i), v) =>
          let args = Map.add_exn(args, ~key=i, ~data=v);
          (args, ret);
        | (_, Returns, v) =>
          switch (ret) {
          | Some(_) => failwith("conflicting return for function!")
          | None => (args, Some(v))
          }
        | _ => (args, ret)
        }
      },
      g,
      v,
      (Map.empty((module Int)), None),
    );
  let args = List.init(Map.length(args), ~f=i => Map.find_exn(args, i));
  let ret = Option.value_exn(ret, ~message="no return in function node");
  (args, ret);
};

let group_constraints = g => {
  module G' = {
    type t = G.t;
    module V = G.V;
    let iter_vertex = G.iter_vertex;
    let iter_edges = (f, g) => {
      G.iter_edges_e(
        ((v1, lbl, v2)) => {
          switch (v1, lbl, v2) {
          | (_, G.Edge.Constraint, _) => f(v1, v2)
          | _ => ()
          }
        },
        g,
      );
    };
  };
  module C = Graph.Components.Undirected(G');
  let groups = C.components_list(g);
  let trivial_groups = ref(0);
  List.sort(groups, ~compare=(a, b) =>
    Int.ascending(List.length(a), List.length(b))
  )
  |> List.iter(~f=group => {
       let dups =
         List.sort(group, ~compare=[%compare: G.Vertex.t])
         |> List.remove_consecutive_duplicates(~equal=(v1, v2) => {
              switch (v1.G.Vertex.kind, v2.kind) {
              | (_, Var(_))
              | (Var(_), _) => true
              | (Int, Bool)
              | (Bool, Int) => true
              | (Struct, Bool)
              | (Bool, Struct) => true
              | (a, b) => [%equal: G.Vertex.kind](a, b)
              }
            });
       switch (dups) {
       | []
       | [_] => Int.incr(trivial_groups)
       | _ =>
         Stdio.eprintf("Constraint group:\n");
         let vars_skipped = ref(0);
         List.iter(group, ~f=v => {
           switch (v) {
           //| {G.Vertex.kind: Var(_), _} => Int.incr(vars_skipped)
           | _ => Stdio.eprintf("  - %{sexp:G.Vertex.t}\n"^, v)
           }
         });
         if (vars_skipped^ > 0) {
           Stdio.eprintf(
             "  - (%d type variables not shown)\n",
             vars_skipped^,
           );
         };
         Stdio.eprintf("\n");
       };
     });
  Stdio.eprintf("TRIVIAL GROUPS FOUND: %d\n", trivial_groups^);
};

let add_fn_constraints = g => {
  module G' = {
    type t = G.t;
    module V = G.V;
    let iter_vertex = (f, g) => {
      G.iter_vertex(
        v => {
          switch (v.G.Vertex.kind) {
          | Fun => f(v)
          | Var(_) => f(v)
          | _ => ()
          }
        },
        g,
      );
    };
    let iter_edges = (f, g) => {
      G.iter_edges_e(
        ((v1, lbl, v2)) => {
          switch (v1, lbl, v2) {
          | (
              {kind: Fun | Var(_), _},
              G.Edge.Constraint,
              {kind: Fun | Var(_), _},
            ) =>
            f(v1, v2)
          | _ => ()
          }
        },
        g,
      );
    };
  };
  module C = Graph.Components.Undirected(G');
  let groups = C.components_list(g);
  List.iter(
    groups,
    ~f=group => {
      let maybe_transposed =
        List.map(group, ~f=v => {
          switch (v) {
          | {kind: G.Vertex.Fun, _} =>
            let (args, ret) = get_args_and_ret(g, v);
            Some([ret, ...args]);
          | _ => None
          }
        })
        |> List.filter_opt
        |> List.transpose;
      switch (maybe_transposed) {
      | Some(l) =>
        List.iter(
          l,
          ~f=vs => {
            let prev = ref(None);
            List.iter(
              vs,
              ~f=v => {
                switch (prev^) {
                | Some(pv) => G.add_edge(g, pv, v)
                | None => ()
                };
                prev := Some(v);
              },
            );
          },
        )
      | None =>
        Stdio.eprintf("Got constraint group with different arities!\n")
      };
    },
  );
};

let group_fn_constraints = g => {
  module G' = {
    type t = G.t;
    module V = G.V;
    let iter_vertex = (f, g) => {
      G.iter_vertex(
        v => {
          switch (v.G.Vertex.kind) {
          | Fun => f(v)
          | Var(_) => f(v)
          | _ => ()
          }
        },
        g,
      );
    };
    let iter_edges = (f, g) => {
      G.iter_edges_e(
        ((v1, lbl, v2)) => {
          switch (v1, lbl, v2) {
          | (
              {kind: Fun | Var(_), _},
              G.Edge.Constraint,
              {kind: Fun | Var(_), _},
            ) =>
            f(v1, v2)
          | _ => ()
          }
        },
        g,
      );
    };
  };
  module C = Graph.Components.Undirected(G');
  let groups = C.components_list(g);
  List.sort(groups, ~compare=(a, b) =>
    Int.ascending(List.length(a), List.length(b))
  )
  |> List.iter(~f=group => {
       Stdio.eprintf("Function constraint group:\n");
       List.iter(
         group,
         ~f=v => {
           Stdio.eprintf("  - %{sexp:G.Vertex.t}\n"^, v);
           switch (v) {
           | {kind: G.Vertex.Fun, _} =>
             let (args, ret) = get_args_and_ret(g, v);
             List.iteri(args, ~f=(i, v) => {
               Stdio.eprintf("    %d. %{sexp:G.Vertex.t}\n"^, i, v)
             });
             Stdio.eprintf("    -> %{sexp:G.Vertex.t}\n"^, ret);
           | _ => ()
           };
         },
       );
       Stdio.eprintf("\n");
     });
};

let run = prg => {
  let acc = Acc.create();
  let acc = {
    ...acc,
    env: Env.add_vars(acc.env, [("Math", IR.gen_new_type())]),
  };
  let acc = (new collect_constraints)#program(prg, acc);
  add_fn_constraints(acc.g);
  group_constraints(acc.g);
  prg;
  /*
   let subst = Hashtbl.create((module String));
   let add_subst = (name, typ) => {
     switch (Hashtbl.add(subst, ~key=name, ~data=typ)) {
     | `Duplicate =>
       let existing_typ = Hashtbl.find_exn(subst, name);
       Stdio.eprintf(
         "conflicling substitution for %s, new one: %{sexp:IR.typ}, existing one: %{sexp:IR.typ}\n"
           ^,
         name,
         typ,
         existing_typ,
       );
     | `Ok => ()
     };
   };
   let constraints =
     G.fold_edges_e(
       (edge, cc) => {
         switch (edge) {
         | (v1, G.Edge.Constraint, v2) => [(v1, v2), ...cc]
         | _ => cc
         }
       },
       acc.g,
       [],
     );
   List.iter(constraints, ~f=((v1, v2)) => {
     switch (v1.kind, v2.kind) {
     | (G.Vertex.Unit, G.Vertex.Var(x))
     | (G.Vertex.Int, G.Vertex.Var(x))
     | (G.Vertex.Float, G.Vertex.Var(x))
     | (G.Vertex.Bool, G.Vertex.Var(x))
     | (G.Vertex.String, G.Vertex.Var(x))
     | (G.Vertex.Struct, G.Vertex.Var(x))
     | (G.Vertex.Array, G.Vertex.Var(x))
     | (G.Vertex.Fun, G.Vertex.Var(x))
     | (G.Vertex.Any, G.Vertex.Var(x)) =>
       add_subst(x, G.Vertex.to_typ(v1));
       G.merge_vertex(acc.g, [v1, v2]);
     | (G.Vertex.Var(_), G.Vertex.Var(x)) =>
       add_subst(x, G.Vertex.to_typ(v1));
       G.merge_vertex(acc.g, [v1, v2]);
     | (G.Vertex.Var(x), G.Vertex.Unit)
     | (G.Vertex.Var(x), G.Vertex.Int)
     | (G.Vertex.Var(x), G.Vertex.Float)
     | (G.Vertex.Var(x), G.Vertex.Bool)
     | (G.Vertex.Var(x), G.Vertex.String)
     | (G.Vertex.Var(x), G.Vertex.Struct)
     | (G.Vertex.Var(x), G.Vertex.Array)
     | (G.Vertex.Var(x), G.Vertex.Fun)
     | (G.Vertex.Var(x), G.Vertex.Any) =>
       add_subst(x, G.Vertex.to_typ(v2));
       G.merge_vertex(acc.g, [v2, v1]);
     | (G.Vertex.Fun, G.Vertex.Fun) => output_fn_constraint(acc.g, v1, v2)
     | (v1, v2) =>
       Stdio.eprintf(
         "Ignoring constraint %{sexp:G.Vertex.kind} == %{sexp:G.Vertex.kind}\n"
           ^,
         v1,
         v2,
       )
     }
   });
   let out = Stdio.Out_channel.create("out.dot");
   Gv.output_graph(out, acc.g);
   Stdio.Out_channel.close(out);
   (new substitute_types)#program(subst, prg);
   */
};
