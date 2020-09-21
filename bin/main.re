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
open Sexplib;
open Ocaml2go;
open Js_of_ocaml_compiler;

let config =
  Backend_exec.{
    keywords: [
      "break",
      "case",
      "chan",
      "const",
      "continue",
      "default",
      "defer",
      "else",
      "fallthrough",
      "for",
      "func",
      "go",
      "goto",
      "if",
      "import",
      "interface",
      "map",
      "package",
      "range",
      "return",
      "select",
      "struct",
      "switch",
      "type",
      "var",
      /* built-in types */
      "string",
      "bool",
      "int8",
      "uint8",
      "byte",
      "int16",
      "uint16",
      "int32",
      "rune",
      "uint32",
      "int64",
      "uint64",
      "int",
      "uint",
      "uintptr",
      "float32",
      "float64",
      "complex64",
      "complex128",
      /* built-in functions */
      "make",
      "copy",
      "len",
      "panic",
    ],
    extension: "go",
    globals: [],
    supplied_primitives: [
      ("is_int", "is_int"),
      (
        "caml_wrap_thrown_exception_traceless",
        "caml_wrap_thrown_exception_traceless",
      ),
      (
        "caml_wrap_thrown_exception_reraise",
        "caml_wrap_thrown_exception_reraise",
      ),
      ("caml_wrap_thrown_exception", "caml_wrap_thrown_exception"),
      ("caml_wrap_exception", "caml_wrap_exception"),
      ("caml_arity_test", "caml_arity_test"),
      ("left_shift_32", "left_shift_32"),
      ("right_shift_32", "right_shift_32"),
      ("unsigned_right_shift_32", "unsigned_right_shift_32"),
    ],
    allow_simplify_ifdecl: true,
  };

class drop_locs = {
  as _self;
  inherit class Traverse_builtins.map;
  inherit class IR.map;
  pub! loc = _ => IR.N;
};

let save_ir_for_tests = ir => {
  let out = open_out("ir.sexp");
  let ir' = (new drop_locs)#program(ir);
  Printf.fprintf(out, "%s\n", IR.sexp_of_program(ir') |> Sexp.to_string_hum);
  flush(out);
  close_out(out);
};

let process_tree = tree => {
  let ir = Rehp_import.ir_from_rehp(tree);
  let ir =
    ir |> Econd_rewriter.run |> Exn_rewriter.run |> Missing_return_rewriter.run;
  let ir = Inference.run(ir);
  save_ir_for_tests(ir);
  let go = Go_from_rehp.from_ir(ir);
  Go_output.program(go);
};

let rec process_rpc = () => {
  let resp =
    switch (Marshal.from_channel(stdin)) {
    | Backend_exec.FetchConfig => Backend_exec.Config(config)
    | Backend_exec.ProcessTree(tree) =>
      Backend_exec.Output(process_tree(tree))
    };
  Marshal.to_channel(stdout, resp, [Marshal.No_sharing]);
  flush(stdout);
  process_rpc();
};

let () =
  try(process_rpc()) {
  | End_of_file => ()
  };
