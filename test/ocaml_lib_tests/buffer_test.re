/* adapted from https://raw.githubusercontent.com/ocaml/ocaml/trunk/testsuite/tests/lib-buffer/test.ml */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/


open Printf;

let run = () => {
  /* Set up*/
  let n = 10;

  let buf = Buffer.create(n);

  let () =
    for (i in 1 to 10) {
      Buffer.add_char(buf, 'a');
    };

  assert(Buffer.length(buf) == n);

  /* Helpers */

  let output = (result, str) =>
    print_string("Buffer " ++ str ++ " " ++ result ++ "\n");

  let passed = output("passed");

  let failed = output("failed");

  let buffer_truncate = "Buffer.truncate";

  let unexpected = str =>
    Printf.sprintf(
      "The Invalid_argument exception has been raised with an invalid value as argument \"%s\". Expecting \"%s\".",
      str,
      buffer_truncate,
    );

  let validate = (f, str, msg) =>
    if (str == buffer_truncate) {
      f(msg);
    } else {
      failed(unexpected(str));
    };

  /* Tests */
  let () = print_string("Standard Library: Module Buffer\n");

  let truncate_neg: unit = (
    {
      let msg = "truncate: negative";
      try(
        {
          Buffer.truncate(buf, -1);
          failed(msg);
        }
      ) {
      | Invalid_argument(str) => validate(passed, str, msg)
      };
    }: unit
  );

  let truncate_large: unit = (
    {
      let msg = "truncate: large";
      try(
        {
          Buffer.truncate(buf, n + 1);
          failed(msg);
        }
      ) {
      | Invalid_argument(str) => validate(passed, str, msg)
      };
    }: unit
  );

  let truncate_correct: unit = (
    {
      let n' = n - 1
      and msg = "truncate: in-range";
      try(
        {
          Buffer.truncate(buf, n');
          if (Buffer.length(buf) == n') {
            passed(msg);
          } else {
            failed(msg);
          };
        }
      ) {
      | Invalid_argument(str) => validate(failed, str, msg)
      };
    }: unit
  );

  let reset_non_zero: unit = (
    {
      let msg = "reset: non-zero";
      Buffer.reset(buf);
      if (Buffer.length(buf) == 0) {
        passed(msg);
      } else {
        failed(msg);
      };
    }: unit
  );

  let reset_zero: unit = (
    {
      let msg = "reset: zero";
      Buffer.reset(buf);
      if (Buffer.length(buf) == 0) {
        passed(msg);
      } else {
        failed(msg);
      };
    }: unit
  );

  let utf_8_spec =
    /* UTF-8 byte sequences, cf. table 3.7 Unicode 9. */
    [
      ((0x0000, 0x007F), [|(0x00, 0x7F)|]),
      ((0x0080, 0x07FF), [|(0xC2, 0xDF), (0x80, 0xBF)|]),
      ((0x0800, 0x0FFF), [|(0xE0, 0xE0), (0xA0, 0xBF), (0x80, 0xBF)|]),
      ((0x1000, 0xCFFF), [|(0xE1, 0xEC), (0x80, 0xBF), (0x80, 0xBF)|]),
      ((0xD000, 0xD7FF), [|(0xED, 0xED), (0x80, 0x9F), (0x80, 0xBF)|]),
      ((0xE000, 0xFFFF), [|(0xEE, 0xEF), (0x80, 0xBF), (0x80, 0xBF)|]),
      (
        (0x10000, 0x3FFFF),
        [|(0xF0, 0xF0), (0x90, 0xBF), (0x80, 0xBF), (0x80, 0xBF)|],
      ),
      (
        (0x40000, 0xFFFFF),
        [|(0xF1, 0xF3), (0x80, 0xBF), (0x80, 0xBF), (0x80, 0xBF)|],
      ),
      (
        (0x100000, 0x10FFFF),
        [|(0xF4, 0xF4), (0x80, 0x8F), (0x80, 0xBF), (0x80, 0xBF)|],
      ),
    ];

  let utf_16be_spec =
    /* UTF-16BE byte sequences, derived from table 3.5 Unicode 9. */
    [
      ((0x0000, 0xD7FF), [|(0x00, 0xD7), (0x00, 0xFF)|]),
      ((0xE000, 0xFFFF), [|(0xE0, 0xFF), (0x00, 0xFF)|]),
      (
        (0x10000, 0x10FFFF),
        [|(0xD8, 0xDB), (0x00, 0xFF), (0xDC, 0xDF), (0x00, 0xFF)|],
      ),
    ];

  let uchar_map_of_spec = spec => {
    /* array mapping Uchar.t as ints to byte sequences according to [spec]. */
    let map = Array.make(Uchar.to_int(Uchar.max) + 1, "");
    let add_range = (((umin, umax), bytes)) => {
      let len = Array.length(bytes);
      let bmin = i =>
        if (i < len) {
          fst(bytes[i]);
        } else {
          max_int;
        };
      let bmax = i =>
        if (i < len) {
          snd(bytes[i]);
        } else {
          min_int;
        };
      let uchar = ref(umin);
      let buf = Bytes.create(len);
      let add = len' =>
        if (len != len') {
          ();
        } else {
          let bytes = Bytes.to_string(buf);
          map[uchar^] = bytes;
          incr(uchar);
        };

      for (b0 in bmin(0) to bmax(0)) {
        Bytes.unsafe_set(buf, 0, Char.chr(b0));
        for (b1 in bmin(1) to bmax(1)) {
          Bytes.unsafe_set(buf, 1, Char.chr(b1));
          for (b2 in bmin(2) to bmax(2)) {
            Bytes.unsafe_set(buf, 2, Char.chr(b2));
            for (b3 in bmin(3) to bmax(3)) {
              Bytes.unsafe_set(buf, 3, Char.chr(b3));
              add(4);
            };
            add(3);
          };
          add(2);
        };
        add(1);
      };
      assert(uchar^ - 1 == umax);
    };

    List.iter(add_range, spec);
    map;
  };

  let test_spec_map = (msg, utf_x_map, buffer_add_utf_x_uchar) => {
    let b = Buffer.create(4);
    let rec loop = u => {
      Buffer.clear(b);
      buffer_add_utf_x_uchar(b, u);
      switch (Buffer.contents(b) == utf_x_map[Uchar.to_int(u)]) {
      | false => failed(sprintf("%s of U+%04X", msg, Uchar.to_int(u)))
      | true =>
        if (Uchar.equal(u, Uchar.max)) {
          passed(msg);
        } else {
          loop(Uchar.succ(u));
        }
      };
    };

    loop(Uchar.min);
  };

  let add_utf_8_uchar: unit = (
    {
      let map = uchar_map_of_spec(utf_8_spec);
      test_spec_map(
        "add_utf_8_uchar: test against spec",
        map,
        Buffer.add_utf_8_uchar,
      );
    }: unit
  );

  let add_utf_16be_uchar: unit = (
    {
      let map = uchar_map_of_spec(utf_16be_spec);
      test_spec_map(
        "add_utf_16be_uchar: test against spec",
        map,
        Buffer.add_utf_16be_uchar,
      );
    }: unit
  );

  let add_utf_16le_uchar: unit = (
    {
      let map = {
        let swap = bytes => {
          let swap = i =>
            switch (i) {
            | 0 => 1
            | 1 => 0
            | 2 => 3
            | 3 => 2
            | _ => assert(false)
            };

          String.init(String.length(bytes), i => bytes.[swap(i)]);
        };

        Array.map(swap, uchar_map_of_spec(utf_16be_spec));
      };

      test_spec_map(
        "add_utf_16le_uchar: test against spec",
        map,
        Buffer.add_utf_16le_uchar,
      );
    }: unit
    /* The uchar_map_of_spec generation function doesn't work on a LE spec since
       uchars and byte seqs have to increase and map together; simply swap
       the map obtained with utf_16be_spec. */
  );

  let () = {
    let b = Buffer.create(64);
    Buffer.add_int8(b, 0xff);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int8(b, 0x01);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int16_be(b, 0x0123);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int16_le(b, 0x0123);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int32_be(b, 0x01234567l);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int32_le(b, 0x01234567l);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int64_be(b, 0x0123456789abcdefL);
    print_endline(String.escaped(Buffer.contents(b)));
    Buffer.add_int64_le(b, 0x0123456789abcdefL);
    print_endline(String.escaped(Buffer.contents(b)));
    print_endline("Expected:");
    let expected =
      "\xff\x01"
      ++ "\x01\x23\x23\x01"
      ++ "\x01\x23\x45\x67"
      ++ "\x67\x45\x23\x01"
      ++ "\x01\x23\x45\x67\x89\xab\xcd\xef"
      ++ "\xef\xcd\xab\x89\x67\x45\x23\x01";
    print_endline(String.escaped(expected));
    assert(Buffer.contents(b) == expected);
    Buffer.clear(b);
    Buffer.add_int16_ne(b, 0x0123);
    Buffer.add_int32_ne(b, 0x01234567l);
    Buffer.add_int64_ne(b, 0x0123456789abcdefL);
    let s = Buffer.contents(b);
    if (Sys.big_endian) {
      assert(s == "\x01\x23\x01\x23\x45\x67\x01\x23\x45\x67\x89\xab\xcd\xef");
    } else {
      assert(s == "\x23\x01\x67\x45\x23\x01\xef\xcd\xab\x89\x67\x45\x23\x01");
    };

    for (i in 1 to 20) {
      let b = Buffer.create(i);
      for (j in 1 to 100) {
        Buffer.add_int8(b, 1);
      };
      assert(Buffer.length(b) == 100);
    };
    for (i in 1 to 20) {
      let b = Buffer.create(i);
      for (j in 1 to 100) {
        Buffer.add_int16_ne(b, 1);
      };
      assert(Buffer.length(b) == 200);
    };
    for (i in 1 to 20) {
      let b = Buffer.create(i);
      for (j in 1 to 100) {
        Buffer.add_int32_ne(b, 1l);
      };
      assert(Buffer.length(b) == 400);
    };
    for (i in 1 to 20) {
      let b = Buffer.create(i);
      for (j in 1 to 100) {
        Buffer.add_int64_ne(b, 1L);
      };
      assert(Buffer.length(b) == 800);
    };
  };
  ();
};
