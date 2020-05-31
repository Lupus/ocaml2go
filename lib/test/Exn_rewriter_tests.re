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
       ((params (t x)) (locals (try_all)) (loc N)
        (body
         (((SEFunDecl
            ((name try_all)
             (func
              ((params (param)) (locals (ai_ f fs match param__0)) (loc N)
               (body
                (((SEStatement
                   (SAssignment
                    ((sassign_lvalue (LVar param__0))
                     (sassign_expr (EVar param)))))
                  N)
                 ((SEStatement
                   (SLoop
                    ((SBlock
                      (((SIf
                         ((EVar param__0)
                          ((SBlock
                            (((SAssignment
                               ((sassign_lvalue (LVar fs))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar param__0))
                                   (estruct_index 2))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar match))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar param__0))
                                   (estruct_index 1))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar f))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar match))
                                   (estruct_index 1))))))
                              N)
                             ((STry
                               ((((SAssignment
                                   ((sassign_lvalue (LVar ai_))
                                    (sassign_expr
                                     (ECall
                                      ((ecall_expr (EVar call2))
                                       (ecall_args
                                        ((EVar f) (EVar t) (EVar x))))))))
                                  N)
                                 ((SReturn ((EVar ai_))) N))
                                (aj_
                                 (((SAssignment
                                    ((sassign_lvalue (LVar aj_))
                                     (sassign_expr
                                      (ECall
                                       ((ecall_expr
                                         (EAccess
                                          ((eacc_expr (EVar runtime))
                                           (eacc_index
                                            (EStr
                                             ((estr_lit caml_wrap_exception)
                                              (estr_kind Utf8)))))))
                                        (ecall_args ((EVar aj_))))))))
                                   N)
                                  ((SIf
                                    ((EBin
                                      ((ebin_op BEqEqEq)
                                       (ebin_lhs (EVar aj_))
                                       (ebin_rhs (EVar Not_found))))
                                     ((SBlock
                                       (((SAssignment
                                          ((sassign_lvalue (LVar param__0))
                                           (sassign_expr (EVar fs))))
                                         N)
                                        ((SContinue ()) N)))
                                      N)
                                     ()))
                                   N)
                                  ((SThrow
                                    (ECall
                                     ((ecall_expr
                                       (EVar
                                        caml_wrap_thrown_exception_reraise))
                                      (ecall_args ((EVar aj_))))))
                                   N)))))
                              N)))
                           N)
                          ()))
                        N)
                       ((SThrow
                         (ECall
                          ((ecall_expr (EVar caml_wrap_thrown_exception))
                           (ecall_args ((EVar Not_found))))))
                        N)))
                     N)))
                  N)))))))
           N)
          ((SEStatement
            (SReturn
             ((ECall
               ((ecall_expr (EVar try_all))
                (ecall_args
                 ((EStructAccess
                   ((estruct_expr (EVar funs)) (estruct_index 1))))))))))
           N)))))))
    N))))
    |};
  let ir = ir_sexp |> Parsexp.Single.parse_string_exn |> IR.program_of_sexp;
  Stdio.printf("--- Original IR ---\n%s\n", IR_printer.print(ir));
  let ir' = Exn_rewriter.run(ir);
  Stdio.printf("--- IR after rewriting ---\n%s\n", IR_printer.print(ir'));
  %expect
  {|
    --- Original IR ---

    function show(t, x) {
      vars try_all
      function try_all(param) {
        vars ai_, f, fs, match, param__0
        param__0 = param
        loop {
          if (param__0) {
            fs = param__0{2}
            match = param__0{1}
            f = match{1}
            try {ai_ = call2(f, t, x)
                 return ai_
                 }
            catch (aj_) {
              aj_ = runtime["caml_wrap_exception"](aj_)
              if (aj_ === Not_found) {param__0 = fs
                                      continue
                                      }
              throw caml_wrap_thrown_exception_reraise(aj_)
              }
            }
          throw caml_wrap_thrown_exception(Not_found)
          }
        }
      return try_all(funs{1})
      }


    --- IR after rewriting ---

    function show(t, x) {
      vars try_all
      function try_all(param) {
        vars ai_, f, fs, match, param__0, __exn_rw_1__, __exn_rw_2__
        param__0 = param
        loop {
          if (param__0) {
            fs = param__0{2}
            match = param__0{1}
            f = match{1}
            {
              __exn_rw_1__ = runtime["caml_try"](
                function() {ai_ = call2(f, t, x)
                            return 1
                            })
              if (__exn_rw_1__{0} !== 0) {
                __exn_rw_2__ = __exn_rw_1__{1}
                __exn_rw_2__ = runtime["caml_wrap_exception"](__exn_rw_2__)
                if (__exn_rw_2__ === Not_found) {param__0 = fs
                                                 continue
                                                 }
                throw caml_wrap_thrown_exception_reraise(__exn_rw_2__)
                }
              else switch (__exn_rw_1__{1}) {
                                               case 1:return ai_

                                               default:
                                                 %ERAW%

                                               }
              }
            }
          throw caml_wrap_thrown_exception(Not_found)
          }
        }
      return try_all(funs{1})
      } |};
};

let%expect_test _ = {
  let ir_sexp = {|
((prg_locals ())
 (prg_elements
  (((SEFunDecl
     ((name show)
      (func
       ((params (t x)) (locals (try_all)) (loc N)
        (body
         (((SEFunDecl
            ((name try_all)
             (func
              ((params (param)) (locals (ai_ aj_ ak_ f fs match param__0))
               (loc N)
               (body
                (((SEStatement
                   (SAssignment
                    ((sassign_lvalue (LVar param__0))
                     (sassign_expr (EVar param)))))
                  N)
                 ((SEStatement
                   (SLoop
                    ((SBlock
                      (((SIf
                         ((EVar param__0)
                          ((SBlock
                            (((SAssignment
                               ((sassign_lvalue (LVar fs))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar param__0))
                                   (estruct_index 2))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar match))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar param__0))
                                   (estruct_index 1))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar f))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar match))
                                   (estruct_index 1))))))
                              N)
                             ((STry
                               ((((STry
                                   ((((SAssignment
                                       ((sassign_lvalue (LVar ak_))
                                        (sassign_expr
                                         (ECall
                                          ((ecall_expr (EVar call2))
                                           (ecall_args
                                            ((EVar f) (EVar t) (EVar x))))))))
                                      N)
                                     ((SAssignment
                                       ((sassign_lvalue (LVar aj_))
                                        (sassign_expr (EVar ak_))))
                                      N)
                                     ((SContinue ()) N))
                                    (am_
                                     (((SAssignment
                                        ((sassign_lvalue (LVar am_))
                                         (sassign_expr
                                          (ECall
                                           ((ecall_expr
                                             (EAccess
                                              ((eacc_expr (EVar runtime))
                                               (eacc_index
                                                (EStr
                                                 ((estr_lit
                                                   caml_wrap_exception)
                                                  (estr_kind Utf8)))))))
                                            (ecall_args ((EVar am_))))))))
                                       N)
                                      ((SIf
                                        ((EBin
                                          ((ebin_op BNotEqEq)
                                           (ebin_lhs (EVar am_))
                                           (ebin_rhs (EVar Not_found))))
                                         ((SThrow
                                           (ECall
                                            ((ecall_expr
                                              (EVar
                                               caml_wrap_thrown_exception_reraise))
                                             (ecall_args ((EVar am_))))))
                                          N)
                                         ()))
                                       N)
                                      ((SAssignment
                                        ((sassign_lvalue (LVar ai_))
                                         (sassign_expr
                                          (ECall
                                           ((ecall_expr (EVar try_all))
                                            (ecall_args ((EVar fs))))))))
                                       N)
                                      ((SAssignment
                                        ((sassign_lvalue (LVar aj_))
                                         (sassign_expr (EVar ai_))))
                                       N)
                                       ((SReturn ((EInt 42))) N)
                                       ))))
                                  N)
                                 ((SReturn ((EVar aj_))) N))
                                (al_
                                 (((SAssignment
                                    ((sassign_lvalue (LVar al_))
                                     (sassign_expr
                                      (ECall
                                       ((ecall_expr
                                         (EAccess
                                          ((eacc_expr (EVar runtime))
                                           (eacc_index
                                            (EStr
                                             ((estr_lit caml_wrap_exception)
                                              (estr_kind Utf8)))))))
                                        (ecall_args ((EVar al_))))))))
                                   N)
                                  ((SIf
                                    ((EBin
                                      ((ebin_op BEqEqEq)
                                       (ebin_lhs (EVar al_))
                                       (ebin_rhs (EVar End_of_file))))
                                     ((SBlock
                                       (((SAssignment
                                          ((sassign_lvalue (LVar param__0))
                                           (sassign_expr (EVar fs))))
                                         N)
                                        ((SContinue ()) N)))
                                      N)
                                     ()))
                                   N)
                                  ((SThrow
                                    (ECall
                                     ((ecall_expr
                                       (EVar
                                        caml_wrap_thrown_exception_reraise))
                                      (ecall_args ((EVar al_))))))
                                   N)))))
                              N)))
                           N)
                          ()))
                        N)
                       ((SThrow
                         (ECall
                          ((ecall_expr (EVar caml_wrap_thrown_exception))
                           (ecall_args ((EVar Not_found))))))
                        N)))
                     N)))
                  N)))))))
           N)
          ((SEStatement
            (SReturn
             ((ECall
               ((ecall_expr (EVar try_all))
                (ecall_args
                 ((EStructAccess
                   ((estruct_expr (EVar funs)) (estruct_index 1))))))))))
           N)))))))
    N))))
    |};
  let ir = ir_sexp |> Parsexp.Single.parse_string_exn |> IR.program_of_sexp;
  Stdio.printf("--- Original IR ---\n%s\n", IR_printer.print(ir));
  let ir' = Exn_rewriter.run(ir);
  Stdio.printf("--- IR after rewriting ---\n%s\n", IR_printer.print(ir'));
  %expect
  {|
    --- Original IR ---

    function show(t, x) {
      vars try_all
      function try_all(param) {
        vars ai_, aj_, ak_, f, fs, match, param__0
        param__0 = param
        loop {
          if (param__0) {
            fs = param__0{2}
            match = param__0{1}
            f = match{1}
            try {
              try {ak_ = call2(f, t, x)
                   aj_ = ak_
                   continue
                   }
              catch (am_) {
                am_ = runtime["caml_wrap_exception"](am_)
                if (am_ !== Not_found) throw caml_wrap_thrown_exception_reraise(
                  am_)
                ai_ = try_all(fs)
                aj_ = ai_
                return 42
                }
              return aj_
              }
            catch (al_) {
              al_ = runtime["caml_wrap_exception"](al_)
              if (al_ === End_of_file) {param__0 = fs
                                        continue
                                        }
              throw caml_wrap_thrown_exception_reraise(al_)
              }
            }
          throw caml_wrap_thrown_exception(Not_found)
          }
        }
      return try_all(funs{1})
      }


    --- IR after rewriting ---

    function show(t, x) {
      vars try_all
      function try_all(param) {
        vars ai_, aj_, ak_, f, fs, match, param__0, __exn_rw_3__, __exn_rw_4__,
             __exn_rw_5__, __exn_rw_6__
        param__0 = param
        loop {
          if (param__0) {
            fs = param__0{2}
            match = param__0{1}
            f = match{1}
            {
              __exn_rw_5__ = runtime["caml_try"](
                function() {{
                              __exn_rw_3__ = runtime["caml_try"](
                                function() {ak_ = call2(f, t, x)
                                            aj_ = ak_
                                            return 1
                                            })
                              if (__exn_rw_3__{0} !== 0) {
                                __exn_rw_4__ = __exn_rw_3__{1}
                                __exn_rw_4__ = runtime["caml_wrap_exception"](
                                  __exn_rw_4__)
                                if (__exn_rw_4__ !== Not_found)
                                  throw caml_wrap_thrown_exception_reraise(
                                  __exn_rw_4__)
                                ai_ = try_all(fs)
                                aj_ = ai_
                                return 1
                                }
                              else
                                switch (__exn_rw_3__{1}) {
                                                            case 1:return 2

                                                            default:
                                                              %ERAW%

                                                            }
                              }
                            return 3
                            })
              if (__exn_rw_5__{0} !== 0) {
                __exn_rw_6__ = __exn_rw_5__{1}
                __exn_rw_6__ = runtime["caml_wrap_exception"](__exn_rw_6__)
                if (__exn_rw_6__ === End_of_file) {param__0 = fs
                                                   continue
                                                   }
                throw caml_wrap_thrown_exception_reraise(__exn_rw_6__)
                }
              else
                switch (__exn_rw_5__{1}) {
                                            case 3:return aj_

                                            case 2:continue

                                            case 1:return 42

                                            default:
                                              %ERAW%

                                            }
              }
            }
          throw caml_wrap_thrown_exception(Not_found)
          }
        }
      return try_all(funs{1})
      } |};
};

let%expect_test _ = {
  let ir_sexp = {|
((prg_locals ())
 (prg_elements
   (((SEFunDecl
     ((name waitpid_non_intr)
      (func
       ((params (pid)) (locals (DF_ DG_)) (loc N)
        (body
         (((SEStatement
            (SLoop
             ((STry
               ((((SAssignment
                   ((sassign_lvalue (LVar DG_))
                    (sassign_expr
                     (ECall
                      ((ecall_expr
                        (EAccess
                         ((eacc_expr (EVar runtime))
                          (eacc_index
                           (EStr ((estr_lit unix_waitpid) (estr_kind Utf8)))))))
                       (ecall_args ((EInt 0) (EVar pid))))))))
                  N)
                 ((SReturn ((EVar DG_))) N))
                (DH_
                 (((SAssignment
                    ((sassign_lvalue (LVar DH_))
                     (sassign_expr
                      (ECall
                       ((ecall_expr
                         (EAccess
                          ((eacc_expr (EVar runtime))
                           (eacc_index
                            (EStr
                             ((estr_lit caml_wrap_exception)
                              (estr_kind Utf8)))))))
                        (ecall_args ((EVar DH_))))))))
                   N)
                  ((SIf
                    ((EBin
                      ((ebin_op BEqEqEq)
                       (ebin_lhs
                        (EStructAccess
                         ((estruct_expr (EVar DH_)) (estruct_index 1))))
                       (ebin_rhs (EVar Unix_error))))
                     ((SBlock
                       (((SAssignment
                          ((sassign_lvalue (LVar DF_))
                           (sassign_expr
                            (EStructAccess
                             ((estruct_expr (EVar DH_)) (estruct_index 2))))))
                         N)
                        ((SIf
                          ((ECall
                            ((ecall_expr (EVar is_int))
                             (ecall_args ((EVar DF_)))))
                           ((SIf
                             ((EBin
                               ((ebin_op BEqEqEq) (ebin_lhs (EInt 11))
                                (ebin_rhs (EVar DF_))))
                              ((SContinue ()) N) ()))
                            N)
                           ()))
                         N)))
                      N)
                     ()))
                   N)
                  ((SThrow
                    (ECall
                     ((ecall_expr (EVar caml_wrap_thrown_exception_reraise))
                      (ecall_args ((EVar DH_))))))
                   N)))))
              N)))
           N)))))))
    N))))
    |};
  let ir = ir_sexp |> Parsexp.Single.parse_string_exn |> IR.program_of_sexp;
  Stdio.printf("--- Original IR ---\n%s\n", IR_printer.print(ir));
  let ir' = Exn_rewriter.run(ir);
  Stdio.printf("--- IR after rewriting ---\n%s\n", IR_printer.print(ir'));
  %expect
  {|
    --- Original IR ---

    function waitpid_non_intr(pid) {
      vars DF_, DG_
      loop try {DG_ = runtime["unix_waitpid"](0, pid)
                return DG_
                }
      catch (DH_) {
        DH_ = runtime["caml_wrap_exception"](DH_)
        if (DH_{1} === Unix_error) {
          DF_ = DH_{2}
          if (is_int(DF_)) if (11 === DF_) continue
          }
        throw caml_wrap_thrown_exception_reraise(DH_)
        }
      }


    --- IR after rewriting ---

    function waitpid_non_intr(pid) {
      vars DF_, DG_, __exn_rw_7__, __exn_rw_8__
      loop {
        __exn_rw_7__ = runtime["caml_try"](
          function() {DG_ = runtime["unix_waitpid"](0, pid)
                      return 1
                      })
        if (__exn_rw_7__{0} !== 0) {
          __exn_rw_8__ = __exn_rw_7__{1}
          __exn_rw_8__ = runtime["caml_wrap_exception"](__exn_rw_8__)
          if (__exn_rw_8__{1} === Unix_error) {
            DF_ = __exn_rw_8__{2}
            if (is_int(DF_)) if (11 === DF_) continue
            }
          throw caml_wrap_thrown_exception_reraise(__exn_rw_8__)
          }
        else switch (__exn_rw_7__{1}) {
                                         case 1:return DG_

                                         default:
                                           %ERAW%

                                         }
        }
      } |};
};

let%expect_test _ = {
  let ir_sexp = {|
((prg_locals ())
 (prg_elements
   (
          ((SEFunDecl
            ((name accept_s)
             (func
              ((params (s__0)) (locals (S6_ S7_ S8_ S9_ S__ Ta_ j len__0))
               (loc N)
               (body
                (((SEStatement
                   (SAssignment
                    ((sassign_lvalue (LVar len__0))
                     (sassign_expr
                      (ECall
                       ((ecall_expr (EVar caml_ml_string_length))
                        (ecall_args ((EVar s__0)))))))))
                  N)
                 ((SEStatement
                   (STry
                    ((((SAssignment
                        ((sassign_lvalue (LVar S7_))
                         (sassign_expr
                          (EUn
                           ((eun_op UToInt)
                            (eun_expr
                             (EBin
                              ((ebin_op BIntPlus) (ebin_lhs (EVar len__0))
                               (ebin_rhs (EInt -1))))))))))
                       N)
                      ((SAssignment
                        ((sassign_lvalue (LVar S6_)) (sassign_expr (EInt 0))))
                       N)
                      ((SIf
                        ((EUn
                          ((eun_op UNot)
                           (eun_expr
                            (EBin
                             ((ebin_op BLt) (ebin_lhs (EVar S7_))
                              (ebin_rhs (EInt 0)))))))
                         ((SBlock
                           (((SAssignment
                              ((sassign_lvalue (LVar j))
                               (sassign_expr (EVar S6_))))
                             N)
                            ((SLoop
                              ((SBlock
                                (((STry
                                   ((((SAssignment
                                       ((sassign_lvalue (LVar S9_))
                                        (sassign_expr
                                         (EUn
                                          ((eun_op UToInt)
                                           (eun_expr
                                            (EBin
                                             ((ebin_op BIntPlus)
                                              (ebin_lhs
                                               (EStructAccess
                                                ((estruct_expr (EVar i))
                                                 (estruct_index 1))))
                                              (ebin_rhs (EVar j))))))))))
                                      N)
                                     ((SAssignment
                                       ((sassign_lvalue (LVar S__))
                                        (sassign_expr
                                         (ECall
                                          ((ecall_expr
                                            (EVar caml_string_get))
                                           (ecall_args ((EVar s) (EVar S9_))))))))
                                      N)
                                     ((SIf
                                       ((EBin
                                         ((ebin_op BNotEqEq)
                                          (ebin_lhs
                                           (ECall
                                            ((ecall_expr
                                              (EVar caml_string_get))
                                             (ecall_args
                                              ((EVar s__0) (EVar j))))))
                                          (ebin_rhs (EVar S__))))
                                        ((SThrow
                                          (ECall
                                           ((ecall_expr
                                             (EVar
                                              caml_wrap_thrown_exception))
                                            (ecall_args ((EVar Exit))))))
                                         N)
                                        ()))
                                      N))
                                    (Tc_
                                     (((SThrow
                                        (ECall
                                         ((ecall_expr
                                           (EVar caml_wrap_thrown_exception))
                                          (ecall_args ((EVar Exit))))))
                                       N)))))
                                  N)
                                 ((SAssignment
                                   ((sassign_lvalue (LVar Ta_))
                                    (sassign_expr
                                     (EUn
                                      ((eun_op UToInt)
                                       (eun_expr
                                        (EBin
                                         ((ebin_op BIntPlus)
                                          (ebin_lhs (EVar j))
                                          (ebin_rhs (EInt 1))))))))))
                                  N)
                                 ((SIf
                                   ((EBin
                                     ((ebin_op BNotEqEq)
                                      (ebin_lhs (EVar S7_))
                                      (ebin_rhs (EVar j))))
                                    ((SBlock
                                      (((SAssignment
                                         ((sassign_lvalue (LVar j))
                                          (sassign_expr (EVar Ta_))))
                                        N)
                                       ((SContinue ()) N)))
                                     N)
                                    ()))
                                  N)
                                 ((SBreak ()) N)))
                               N))
                             N)))
                          N)
                         ()))
                       N)
                      ((SAssignment
                        ((sassign_lvalue
                          (LStructAccess
                           ((estruct_expr (EVar i)) (estruct_index 1))))
                         (sassign_expr
                          (EUn
                           ((eun_op UToInt)
                            (eun_expr
                             (EBin
                              ((ebin_op BIntPlus)
                               (ebin_lhs
                                (EStructAccess
                                 ((estruct_expr (EVar i)) (estruct_index 1))))
                               (ebin_rhs (EVar len__0))))))))))
                       N)
                      ((SAssignment
                        ((sassign_lvalue (LVar S8_)) (sassign_expr (EInt 1))))
                       N)
                      ((SReturn ((EVar S8_))) N))
                     (Tb_
                      (((SAssignment
                         ((sassign_lvalue (LVar Tb_))
                          (sassign_expr
                           (ECall
                            ((ecall_expr
                              (EAccess
                               ((eacc_expr (EVar runtime))
                                (eacc_index
                                 (EStr
                                  ((estr_lit caml_wrap_exception)
                                   (estr_kind Utf8)))))))
                             (ecall_args ((EVar Tb_))))))))
                        N)
                       ((SIf
                         ((EBin
                           ((ebin_op BEqEqEq) (ebin_lhs (EVar Tb_))
                            (ebin_rhs (EVar Exit))))
                          ((SReturn ((EInt 0))) N) ()))
                        N)
                       ((SThrow
                         (ECall
                          ((ecall_expr
                            (EVar caml_wrap_thrown_exception_reraise))
                           (ecall_args ((EVar Tb_))))))
                        N))))))
                  N)))))))
           N)

   )))
    |};
  let ir = ir_sexp |> Parsexp.Single.parse_string_exn |> IR.program_of_sexp;
  Stdio.printf("--- Original IR ---\n%s\n", IR_printer.print(ir));
  let ir' = Exn_rewriter.run(ir);
  Stdio.printf("--- IR after rewriting ---\n%s\n", IR_printer.print(ir'));
  %expect
  {|
    --- Original IR ---

    function accept_s(s__0) {
      vars S6_, S7_, S8_, S9_, S__, Ta_, j, len__0
      len__0 = caml_ml_string_length(s__0)
      try {
        S7_ = (int)(len__0 + -1)
        S6_ = 0
        if (!(S7_ < 0)) {
          j = S6_
          loop {
            try {
              S9_ = (int)(i{1} + j)
              S__ = caml_string_get(s, S9_)
              if (caml_string_get(s__0, j) !== S__)
                throw caml_wrap_thrown_exception(Exit)
              }
            catch (Tc_) {throw caml_wrap_thrown_exception(Exit)
                         }
            Ta_ = (int)(j + 1)
            if (S7_ !== j) {j = Ta_
                            continue
                            }
            break
            }
          }
        i{1} = (int)(i{1} + len__0)
        S8_ = 1
        return S8_
        }
      catch (Tb_) {
        Tb_ = runtime["caml_wrap_exception"](Tb_)
        if (Tb_ === Exit) return 0
        throw caml_wrap_thrown_exception_reraise(Tb_)
        }
      }


    --- IR after rewriting ---

    function accept_s(s__0) {
      vars S6_, S7_, S8_, S9_, S__, Ta_, j, len__0, __exn_rw_9__, __exn_rw_10__,
           __exn_rw_11__, __exn_rw_12__
      len__0 = caml_ml_string_length(s__0)
      {
        __exn_rw_11__ = runtime["caml_try"](
          function() {S7_ = (int)(len__0 + -1)
                      S6_ = 0
                      if (!(S7_ < 0)) {
                        j = S6_
                        loop {
                          {
                            __exn_rw_9__ = runtime["caml_try"](
                              function() {S9_ = (int)(i{1} + j)
                                          S__ = caml_string_get(s, S9_)
                                          if (caml_string_get(s__0, j) !== S__)
                                            throw caml_wrap_thrown_exception(
                                            Exit)
                                          return 0
                                          })
                            if (__exn_rw_9__{0} !== 0) {
                              __exn_rw_10__ = __exn_rw_9__{1}
                              throw caml_wrap_thrown_exception(Exit)
                              }
                            else
                              switch (__exn_rw_9__{1}) {
                                                          case 0:break

                                                          default:
                                                            %ERAW%

                                                          }
                            }
                          Ta_ = (int)(j + 1)
                          if (S7_ !== j) {j = Ta_
                                          continue
                                          }
                          break
                          }
                        }
                      i{1} = (int)(i{1} + len__0)
                      S8_ = 1
                      return 1
                      })
        if (__exn_rw_11__{0} !== 0) {
          __exn_rw_12__ = __exn_rw_11__{1}
          __exn_rw_12__ = runtime["caml_wrap_exception"](__exn_rw_12__)
          if (__exn_rw_12__ === Exit) return 0
          throw caml_wrap_thrown_exception_reraise(__exn_rw_12__)
          }
        else switch (__exn_rw_11__{1}) {
                                          case 1:return S8_

                                          default:
                                            %ERAW%

                                          }
        }
      } |};
};

let%expect_test _ = {
  let ir_sexp = {|
((prg_locals ())
 (prg_elements
   (
          ((SEFunDecl
            ((name read_tail)
             (func
              ((params (max_lines ic))
               (locals
                (Nh_ Ni_ Nj_ Nk_ Nl_ display_lines n omitted_count rev_lines
                 selected_lines))
               (loc N)
               (body
                (((SEStatement
                   (SAssignment
                    ((sassign_lvalue (LVar rev_lines))
                     (sassign_expr
                      (ETag ((etag_tag 0) (etag_items ((EInt 0)))))))))
                  N)
                 ((SEStatement
                   (STry
                    ((((SLoop
                        ((SBlock
                          (((SAssignment
                             ((sassign_lvalue (LVar Nl_))
                              (sassign_expr
                               (EStructAccess
                                ((estruct_expr (EVar rev_lines))
                                 (estruct_index 1))))))
                            N)
                           ((SAssignment
                             ((sassign_lvalue
                               (LStructAccess
                                ((estruct_expr (EVar rev_lines))
                                 (estruct_index 1))))
                              (sassign_expr
                               (ETag
                                ((etag_tag 0)
                                 (etag_items
                                  ((ECall
                                    ((ecall_expr (EVar input_line))
                                     (ecall_args ((EVar ic)))))
                                   (EVar Nl_))))))))
                            N)
                           ((SContinue ()) N)))
                         N))
                       N))
                     (Nm_
                      (((SAssignment
                         ((sassign_lvalue (LVar Nm_))
                          (sassign_expr
                           (ECall
                            ((ecall_expr
                              (EAccess
                               ((eacc_expr (EVar runtime))
                                (eacc_index
                                 (EStr
                                  ((estr_lit caml_wrap_exception)
                                   (estr_kind Utf8)))))))
                             (ecall_args ((EVar Nm_))))))))
                        N)
                       ((SIf
                         ((EBin
                           ((ebin_op BEqEqEq) (ebin_lhs (EVar Nm_))
                            (ebin_rhs (EVar End_of_file))))
                          ((SBlock
                            (((SIf
                               ((ECall
                                 ((ecall_expr (EVar is_int))
                                  (ecall_args ((EVar max_lines)))))
                                ((SBlock
                                  (((SAssignment
                                     ((sassign_lvalue (LVar Nh_))
                                      (sassign_expr
                                       (EStructAccess
                                        ((estruct_expr (EVar rev_lines))
                                         (estruct_index 1))))))
                                    N)
                                   ((SAssignment
                                     ((sassign_lvalue (LVar selected_lines))
                                      (sassign_expr
                                       (ECall
                                        ((ecall_expr (EVar rev))
                                         (ecall_args ((EVar Nh_))))))))
                                    N)))
                                 N)
                                (((SBlock
                                   (((SAssignment
                                      ((sassign_lvalue (LVar n))
                                       (sassign_expr
                                        (EStructAccess
                                         ((estruct_expr (EVar max_lines))
                                          (estruct_index 2))))))
                                     N)
                                    ((SAssignment
                                      ((sassign_lvalue (LVar Nk_))
                                       (sassign_expr
                                        (EStructAccess
                                         ((estruct_expr (EVar rev_lines))
                                          (estruct_index 1))))))
                                     N)
                                    ((SAssignment
                                      ((sassign_lvalue (LVar selected_lines))
                                       (sassign_expr
                                        (ECall
                                         ((ecall_expr (EVar rev_head))
                                          (ecall_args ((EVar n) (EVar Nk_))))))))
                                     N)))
                                  N))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar Ni_))
                                (sassign_expr
                                 (ECall
                                  ((ecall_expr (EVar length))
                                   (ecall_args ((EVar selected_lines))))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar Nj_))
                                (sassign_expr
                                 (EStructAccess
                                  ((estruct_expr (EVar rev_lines))
                                   (estruct_index 1))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar omitted_count))
                                (sassign_expr
                                 (EUn
                                  ((eun_op UToInt)
                                   (eun_expr
                                    (EBin
                                     ((ebin_op BMinus)
                                      (ebin_lhs
                                       (ECall
                                        ((ecall_expr (EVar length))
                                         (ecall_args ((EVar Nj_))))))
                                      (ebin_rhs (EVar Ni_))))))))))
                              N)
                             ((SAssignment
                               ((sassign_lvalue (LVar display_lines))
                                (sassign_expr
                                 (ECond
                                  ((econd_test
                                    (EBin
                                     ((ebin_op BEqEqEq) (ebin_lhs (EInt 0))
                                      (ebin_rhs (EVar omitted_count)))))
                                   (econd_success (EVar selected_lines))
                                   (econd_failure
                                    (ETag
                                     ((etag_tag 0)
                                      (etag_items
                                       ((ECall
                                         ((ecall_expr (EVar call3))
                                          (ecall_args
                                           ((ECall
                                             ((ecall_expr (EVar asprintf))
                                              (ecall_args ((EVar mi_)))))
                                            (EVar omitted_count)
                                            (EVar pp_plural)
                                            (EVar omitted_count)))))
                                        (EVar selected_lines)))))))))))
                              N)
                             ((SReturn
                               ((ECall
                                 ((ecall_expr (EVar symbol))
                                  (ecall_args
                                   ((ECall
                                     ((ecall_expr (EVar arg__0))
                                      (ecall_args
                                       ((EVar mh_) (EVar display_lines)))))
                                    (EVar mg_)))))))
                              N)))
                           N)
                          ()))
                        N)
                       ((SThrow
                         (ECall
                          ((ecall_expr
                            (EVar caml_wrap_thrown_exception_reraise))
                           (ecall_args ((EVar Nm_))))))
                        N))))))
                  N)))))))
           N)


   )))
    |};
  let ir = ir_sexp |> Parsexp.Single.parse_string_exn |> IR.program_of_sexp;
  Stdio.printf("--- Original IR ---\n%s\n", IR_printer.print(ir));
  let ir' = Exn_rewriter.run(ir) |> Missing_return_rewriter.run;
  Stdio.printf("--- IR after rewriting ---\n%s\n", IR_printer.print(ir'));
  %expect
  {|
    --- Original IR ---

    function read_tail(max_lines, ic) {
      vars Nh_, Ni_, Nj_, Nk_, Nl_, display_lines, n, omitted_count, rev_lines,
           selected_lines
      rev_lines = {0;0}
      try {
        loop {Nl_ = rev_lines{1}
              rev_lines{1} = {0;input_line(ic), Nl_}
              continue
              }
        }
      catch (Nm_) {
        Nm_ = runtime["caml_wrap_exception"](Nm_)
        if (Nm_ === End_of_file) {
          if (is_int(max_lines)) {Nh_ = rev_lines{1}
                                  selected_lines = rev(Nh_)
                                  }
          else {
            n = max_lines{2}
            Nk_ = rev_lines{1}
            selected_lines = rev_head(n, Nk_)
            }
          Ni_ = length(selected_lines)
          Nj_ = rev_lines{1}
          omitted_count = (int)(length(Nj_) -? Ni_)
          display_lines = 0 === omitted_count ? selected_lines : {0;call3(
                                                                      asprintf(
                                                                        mi_),
                                                                      omitted_count,
                                                                      pp_plural,
                                                                      omitted_count),
                                                                    selected_lines}
          return symbol(arg__0(mh_, display_lines), mg_)
          }
        throw caml_wrap_thrown_exception_reraise(Nm_)
        }
      }


    --- IR after rewriting ---

    function read_tail(max_lines, ic) {
      vars Nh_, Ni_, Nj_, Nk_, Nl_, display_lines, n, omitted_count, rev_lines,
           selected_lines, __exn_rw_13__, __exn_rw_14__
      rev_lines = {0;0}
      {
        __exn_rw_13__ = runtime["caml_try"](
          function() {loop {
                        Nl_ = rev_lines{1}
                        rev_lines{1} = {0;input_line(ic), Nl_}
                        continue
                        }
                      return 0
                      })
        if (__exn_rw_13__{0} !== 0) {
          __exn_rw_14__ = __exn_rw_13__{1}
          __exn_rw_14__ = runtime["caml_wrap_exception"](__exn_rw_14__)
          if (__exn_rw_14__ === End_of_file) {
            if (is_int(max_lines)) {Nh_ = rev_lines{1}
                                    selected_lines = rev(Nh_)
                                    }
            else {
              n = max_lines{2}
              Nk_ = rev_lines{1}
              selected_lines = rev_head(n, Nk_)
              }
            Ni_ = length(selected_lines)
            Nj_ = rev_lines{1}
            omitted_count = (int)(length(Nj_) -? Ni_)
            display_lines = 0 === omitted_count ? selected_lines : {0;call3(
                                                                        asprintf(
                                                                        mi_),
                                                                        omitted_count,
                                                                        pp_plural,
                                                                        omitted_count),
                                                                      selected_lines}
            return symbol(arg__0(mh_, display_lines), mg_)
            }
          throw caml_wrap_thrown_exception_reraise(__exn_rw_14__)
          }
        else switch (__exn_rw_13__{1}) {
                                          case 0:break

                                          default:
                                            %ERAW%

                                          }
        }
      %ERAW%
      } |};
};
