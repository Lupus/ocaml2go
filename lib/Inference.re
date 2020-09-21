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
    type t =
      | Unit
      | Int
      | Float
      | Bool
      | String
      | Struct
      | Array
      | Var(string)
      | Fun(string)
      | Any(string);
    let default = Unit;
    let gen_new_func = {
      let last_func_id = ref(0);
      () => {
        Int.incr(last_func_id);
        Fun(Printf.sprintf("f%d", last_func_id^));
      };
    };
    let of_typ =
      fun
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
      | TStruct => Struct
      | TArray => Array
      | TAny(_) =>
        failwith(
          "TAny should not appear in expressions during constraint collection",
        );
  };
  module Edge = {
    [@deriving (sexp, compare, equal, hash)]
    type t =
      | Arg(int)
      | Returns
      | Constraint;
    let default = Constraint;
  };
  module X = Graph.Imperative.Digraph.ConcreteLabeled(Vertex, Edge);
  include Graph.Merge.I(X);
  include X;
};

module Display = {
  include G;
  let vertex_name =
    fun
    | G.Vertex.Unit => "unit"
    | Int => "int"
    | Float => "float"
    | Bool => "bool"
    | String => "string"
    | Struct => "struct"
    | Array => "array"
    | Var(x) => x
    | Fun(x) => Printf.sprintf("\"-> (%s)\"", x)
    | Any(x) => x;

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
    unit_v: G.vertex,
    bool_v: G.vertex,
    float_v: G.vertex,
    int_v: G.vertex,
    str_v: G.vertex,
    struct_v: G.vertex,
    arr_v: G.vertex,
    fn_return_v: option(G.vertex),
  };
  let create = () => {
    let env = Env.empty;
    let g = G.create();
    let unit_v = G.Vertex.of_typ(IR.TUnit);
    G.add_vertex(g, unit_v);
    let bool_v = G.Vertex.of_typ(IR.TBool);
    G.add_vertex(g, bool_v);
    let float_v = G.Vertex.of_typ(IR.TFloat);
    G.add_vertex(g, float_v);
    let int_v = G.Vertex.of_typ(IR.TInt);
    G.add_vertex(g, int_v);
    let str_v = G.Vertex.of_typ(IR.TString);
    G.add_vertex(g, str_v);
    let struct_v = G.Vertex.of_typ(IR.TStruct);
    G.add_vertex(g, struct_v);
    let arr_v = G.Vertex.of_typ(IR.TArray);
    G.add_vertex(g, arr_v);
    {
      env,
      g,
      unit_v,
      bool_v,
      float_v,
      int_v,
      str_v,
      struct_v,
      arr_v,
      fn_return_v: None,
    };
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
      let fn_v = G.Vertex.gen_new_func();
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
    | EVar(_) => acc
    | EBool(_) =>
      G.add_edge(acc.g, expr_v, acc.bool_v);
      acc;
    | EFloat(_) =>
      G.add_edge(acc.g, expr_v, acc.float_v);
      acc;
    | EInt(_) =>
      G.add_edge(acc.g, expr_v, acc.int_v);
      acc;
    | EStr(_) =>
      G.add_edge(acc.g, expr_v, acc.str_v);
      acc;
    | ETag(_)
    | EStruct(_) =>
      G.add_edge(acc.g, expr_v, acc.struct_v);
      acc;
    | EArr(_) =>
      G.add_edge(acc.g, expr_v, acc.arr_v);
      acc;
    | EAccess({eacc_expr, eacc_index}) =>
      let acc = self#expression(eacc_expr, acc);
      let eacc_expr_v = G.Vertex.of_typ(eacc_expr.expr_typ);
      G.add_edge(acc.g, eacc_expr_v, acc.struct_v);
      acc;
    | EStructAccess({estruct_expr, _}) =>
      let acc = self#expression(estruct_expr, acc);
      let estruct_expr_v = G.Vertex.of_typ(estruct_expr.expr_typ);
      G.add_edge(acc.g, estruct_expr_v, acc.struct_v);
      acc;
    | EArrAccess({earr_expr, _}) =>
      let acc = self#expression(earr_expr, acc);
      let earr_expr_v = G.Vertex.of_typ(earr_expr.expr_typ);
      G.add_edge(acc.g, earr_expr_v, acc.arr_v);
      acc;
    | EBin({ebin_op: binop, ebin_lhs: e1, ebin_rhs: e2}) =>
      let acc = self#expression(e1, acc);
      let acc = self#expression(e2, acc);
      let e1_v = G.Vertex.of_typ(e1.expr_typ);
      let e2_v = G.Vertex.of_typ(e2.expr_typ);
      switch (binop) {
      | BOr
      | BAnd =>
        G.add_edge(acc.g, e1_v, acc.bool_v);
        G.add_edge(acc.g, e2_v, acc.bool_v);
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
        G.add_edge(acc.g, e1_v, acc.int_v);
        G.add_edge(acc.g, e2_v, acc.int_v);
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
        G.add_edge(acc.g, e1_v, acc.float_v);
        G.add_edge(acc.g, e2_v, acc.float_v);
        acc;
      };
    | ECond(_) =>
      failwith("ECond unsupported by inference, please use Econd_rewriter")
    | EArityTest(e) =>
      let acc = self#expression(e, acc);
      G.add_edge(acc.g, expr_v, acc.int_v);
      acc;
    | EVectlength(e) =>
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e, acc);
      G.add_edge(acc.g, e_v, acc.struct_v);
      G.add_edge(acc.g, expr_v, acc.int_v);
      acc;
    | EArrLen(e) =>
      let e_v = G.Vertex.of_typ(e.expr_typ);
      let acc = self#expression(e, acc);
      G.add_edge(acc.g, e_v, acc.arr_v);
      G.add_edge(acc.g, expr_v, acc.int_v);
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
        G.add_edge(acc.g, e_v, acc.bool_v);
        G.add_edge(acc.g, expr_v, acc.bool_v);
        acc;
      | UNeg =>
        G.add_edge(acc.g, e_v, acc.int_v);
        G.add_edge(acc.g, expr_v, acc.int_v);
        acc;
      | UFloatNeg =>
        G.add_edge(acc.g, e_v, acc.float_v);
        G.add_edge(acc.g, expr_v, acc.float_v);
        acc;
      | UIsInt =>
        G.add_edge(acc.g, expr_v, acc.bool_v);
        acc;
      | UToInt =>
        G.add_edge(acc.g, expr_v, acc.int_v);
        acc;
      | UToBool =>
        G.add_edge(acc.g, expr_v, acc.bool_v);
        acc;
      | UIntToString =>
        G.add_edge(acc.g, e_v, acc.int_v);
        G.add_edge(acc.g, expr_v, acc.str_v);
        acc;
      | UFloatToInt =>
        G.add_edge(acc.g, e_v, acc.float_v);
        G.add_edge(acc.g, expr_v, acc.str_v);
        acc;

      | UBnot =>
        G.add_edge(acc.g, e_v, acc.int_v);
        G.add_edge(acc.g, expr_v, acc.int_v);
        acc;
      };
    | EFun({params, locals, body, loc}) =>
      let ret_v = G.Vertex.of_typ(IR.gen_new_type());
      let acc = {
        let env' = Env.add_vars(acc.env, params);
        let env' = Env.add_vars(env', locals);
        let acc' = {...acc, fn_return_v: Some(ret_v), env: env'};
        let _: Acc.t = self#function_body(body, acc');
        acc;
      };
      let fn_v = G.Vertex.gen_new_func();
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
      G.add_edge(acc.g, e1_v, acc.unit_v);
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
      | LStructAccess({estruct_expr: e, _}) =>
        let e_v = G.Vertex.of_typ(e.expr_typ);
        G.add_edge(acc.g, e_v, acc.struct_v);
        acc;
      | LArrAccess({earr_expr: e1, earr_index: e2}) =>
        let e1_v = G.Vertex.of_typ(e1.expr_typ);
        let e2_v = G.Vertex.of_typ(e2.expr_typ);
        G.add_edge(acc.g, e1_v, acc.arr_v);
        G.add_edge(acc.g, e2_v, acc.int_v);
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
      G.add_edge(acc.g, e_v, acc.bool_v);
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
      let fn_v = G.Vertex.gen_new_func();
      let fn_var_v = Env.get(acc.env, fdecl.name);
      let ret_v = G.Vertex.of_typ(IR.gen_new_type());
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
          let arg_v = G.Vertex.of_typ(arg_typ);
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

let run = prg => {
  let acc = (new collect_constraints)#program(prg, Acc.create());
  let out = Stdio.Out_channel.create("out.dot");
  Gv.output_graph(out, acc.g);
  Stdio.Out_channel.close(out);
};
