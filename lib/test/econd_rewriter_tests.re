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
open Ocaml2go;

let%expect_test _ = {
  let ir_sexp = {|
((prg_locals ())
 (prg_elements
  (((SEFunDecl
     ((name show)
      (func
       ((params ()) (locals ()) (loc N)
       (body
                (
                 ((SEStatement

                   (SIf
                     ((EUn
                       ((eun_op UBnot)
                        (eun_expr
                         (ECond
                          ((econd_test
                            (EBin
                             ((ebin_op BEqEqEq)
                              (ebin_lhs (EInt 0))
                              (ebin_rhs
                               (EVar top__0)))))
                           (econd_success
                            (EInt 1))
                           (econd_failure
                            (EInt 0)))))))
                      ((SAssignment
                        ((sassign_lvalue
                          (LStructAccess
                           ((estruct_expr
                             (EVar attrs))
                            (estruct_index 1))))
                         (sassign_expr
                          (ETag
                           ((etag_tag 0)
                            (etag_items
                             ((EVar e)
                              (ECall
                               ((ecall_expr
                                 (EVar
                                  rev_append))
                                (ecall_args
                                 ((EVar top__0)
                                  (EVar es))))))))))))
                       N)
                      ()))

                    )
                  N)
                 ))
          ))))
    N))))
    |};
  let ir = ir_sexp |> Parsexp.Single.parse_string_exn |> IR.program_of_sexp;
  Stdio.printf("--- Original IR ---\n%s\n", IR_printer.print(ir));
  let ir' = Econd_rewriter.run(ir);
  Stdio.printf("--- IR after rewriting ---\n%s\n", IR_printer.print(ir'));
  %expect
  {|
    --- Original IR ---

    function show() {
      if (~(0 === top__0 ? 1 : 0)) attrs{1} = {0;e, rev_append(top__0, es)}
      }


    --- IR after rewriting ---

    function show() {
      if (0 === top__0) if (~(1)) attrs{1} = {0;e, rev_append(top__0, es)} else
        if (~(0)) attrs{1} = {0;e, rev_append(top__0, es)}
      } |};
};
