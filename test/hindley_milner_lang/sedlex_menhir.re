/*
    Boilerplate for using sedlex with Menhir, based on
    https://github.com/Drup/llvm/blob/3c43000f4e86af5b9b368f50721604957d403750/test/Bindings/OCaml/kaleidoscope/src/syntax.ml
 */

/** The state of the parser, a stream and a position. */

type lexbuf = {
  stream: Sedlexing.lexbuf,
  mutable pos: Lexing.position,
};

/** Initialize with the null position. */

let create_lexbuf = (stream) => {
  let pos = {
    Lexing.pos_fname: "(stdin)",
    pos_lnum: 1, /* Start lines at 1, not 0 */
    pos_bol: 0,
    pos_cnum: 0,
  };
  {pos, stream};
};

/** Register a new line in the lexer's position. */

let new_line = (~n=0, lexbuf) => {
  open Lexing;
  let lcp = lexbuf.pos;
  lexbuf.pos = {...lcp, pos_lnum: lcp.pos_lnum + 1, pos_bol: lcp.pos_cnum};
};

/** Update the position with the stream. */

let update = lexbuf => {
  let new_pos = Sedlexing.lexeme_end(lexbuf.stream);
  let p = lexbuf.pos;
  lexbuf.pos = {...p, Lexing.pos_cnum: new_pos};
};

/** The last matched word. */

let lexeme = ({stream}) => Sedlexing.Utf8.lexeme(stream);

/** [ParseError (file, line, col, token)] */

exception ParseError((string, int, int, string));

let string_of_ParseError = ((file, line, cnum, tok)) => {
  let file_to_string = file =>
    if (file == "") {
      "";
    } else {
      " on file " ++ file;
    };

  Printf.sprintf(
    "Parse error%s line %i, column %i, token %s",
    file_to_string(file),
    line,
    cnum,
    tok,
  );
};

let report_parse_error = lexbuf => {
  let {pos} = lexbuf;
  let tok = lexeme(lexbuf);
  open Lexing;
  let line = pos.pos_lnum;
  let col = pos.pos_cnum - pos.pos_bol;
  Printf.fprintf(
    stderr,
    "Parse error: %s\n",
    string_of_ParseError((pos.pos_fname, line, col, tok)),
  );
  exit(1);
};

let sedlex_with_menhir = (lexer', parser', lexbuf) => {
  let lexer = () => {
    let ante_position = lexbuf.pos;
    let token = lexer'(lexbuf);
    let post_position = lexbuf.pos;
    (token, ante_position, post_position);
  };
  let parser = MenhirLib.Convert.Simplified.traditional2revised(parser');

  try(parser(lexer)) {
  | Parser.Error
  | Sedlexing.MalFormed
  | Sedlexing.InvalidCodepoint(_) =>
    report_parse_error(lexbuf)
  | exn =>
    Printf.printf("other exception: %s\n%!", Printexc.to_string(exn))
    exit(1);
  };
};
