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

class find = {
  as _self;
  inherit class Traverse_builtins.fold(option(expression));
  inherit class fold(option(expression)) as super;
  val mutable first_statement = true;
  pub expression = (e, acc) => {
    switch (acc) {
    | None =>
      switch (e.expr_desc) {
      | ECond({econd_test: e, _}) => Some(e)
      | EFun(_) => acc
      | _ => super#expression(e, acc)
      }
    | Some(_) => acc
    };
  };
  pub statement = (s, acc) =>
    if (first_statement) {
      first_statement = false;
      super#statement(s, acc);
    } else {
      acc;
    };
};

type subst_ctx = {
  econd: expression,
  follow: [ | `True | `False],
};

class econd_subst = {
  as _self;
  inherit class Traverse_builtins.map_with_context(subst_ctx);
  inherit class map_with_context(subst_ctx) as super;
  pub expression = ({econd, follow} as ctx, e) => {
    switch (e.expr_desc) {
    | ECond({econd_test, econd_success, econd_failure})
        when phys_equal(econd_test, econd) =>
      switch (follow) {
      | `True => econd_success
      | `False => econd_failure
      }
    | _ => super#expression(ctx, e)
    };
  };
};

class rewriter = {
  as self;
  inherit class Traverse_builtins.map;
  inherit class map as super;
  pub statement = s => {
    let econd = (new find)#statement(s, None);
    switch (econd) {
    | None => super#statement(s)
    | Some(econd) =>
      let subst_mapper = new econd_subst;
      let s_true =
        subst_mapper#statement({econd, follow: `True}, s) |> self#statement;
      let s_false =
        subst_mapper#statement({econd, follow: `False}, s) |> self#statement;
      SIf((econd, (s_true, N), Some((s_false, N))));
    };
  };
};

let run: program => program =
  rehps => {
    (new rewriter)#program(rehps);
  };
