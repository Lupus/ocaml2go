open Sedlexing;

open Parser;

exception LexError(Lexing.position, string);

let digit = [%sedlex.regexp? '0'..'9'];

let number = [%sedlex.regexp? Plus(digit)];

let blank = [%sedlex.regexp? ' ' | '\t'];

let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"];

let any_blank = [%sedlex.regexp? blank | newline];

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z'];

let id = [%sedlex.regexp? (letter, Star(letter | digit)) ];

let decimal_ascii = [%sedlex.regexp? Plus('0'..'9')];

let octal_ascii = [%sedlex.regexp? ("0o", Plus('0'..'7'))];

let hex_ascii = [%sedlex.regexp?
  ("0x", Plus('0'..'9' | 'a'..'f' | 'A'..'F'))
];

let rec nom = buf =>
  switch%sedlex (buf) {
  | Plus(any_blank) => nom(buf)
  | _ => ()
  };

let digit_value = c =>
  Stdlib.(
    switch (c) {
    | 'a'..'f' => 10 + Char.code(c) - Char.code('a')
    | 'A'..'F' => 10 + Char.code(c) - Char.code('A')
    | '0'..'9' => Char.code(c) - Char.code('0')
    | _ => assert(false)
    }
  );

let num_value = (buffer, ~base, ~first) => {
  let buf = Utf8.lexeme(buffer);
  let c = ref(0);
  for (i in first to String.length(buf) - 1) {
    let v = digit_value(buf.[i]);
    assert(v < base);
    c := base * c^ + v;
  };
  c^;
};

let token = lexbuf => {
  let buf = lexbuf.Sedlex_menhir.stream;
  nom(buf);
  switch%sedlex (buf) {
  | eof => EOL
  | "" => EOL
  | '+' => PLUS
  | '*' => TIMES
  | "true" => TRUE
  | "false" => FALSE
  | "fun" => FUN
  | "->" => THINARROW
  | ">"  => GT
  | "&&" => AND
  | "||" => OR
  | "<"  => LT
  | '('  => LPAREN
  | ')'  => RPAREN
  | hex_ascii =>
    let number = num_value(~base=16, ~first=2, buf);
    NUMBER(number);
  | octal_ascii =>
    let number = num_value(~base=8, ~first=2, buf);
    NUMBER(number);
  | decimal_ascii =>
    let number = num_value(~base=10, ~first=0, buf);
    NUMBER(number);
  | id => ID(String.escaped(Utf8.lexeme(buf)))
  | _ =>
    let position = fst @@ lexing_positions(buf);
    let tok = Utf8.lexeme(buf);
    raise @@
    LexError(position, Printf.sprintf("unexpected character %S", tok));
  };
};
