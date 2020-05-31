/*
 * Code examples are taken from
 * https://ocaml.org/learn/tutorials/99problems.html
 * 
 * According to https://github.com/ocaml/ocaml.org/blob/master/LICENSE.md, code
 * examples are licensed under the UNLICENSE.
 * 
 * UNLICENSE
 * 
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or distribute
 * this software, either in source code form or as a compiled binary, for any
 * purpose, commercial or non-commercial, and by any means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors of
 * this software dedicate any and all copyright interest in the software to the
 * public domain. We make this dedication for the benefit of the public at
 * large and to the detriment of our heirs and successors. We intend this
 * dedication to be an overt act of relinquishment in perpetuity of all present
 * and future rights to this software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to http://unlicense.org/
 */

type binary_tree('a) =
  | Empty
  | Node('a, binary_tree('a), binary_tree('a));

let rec buffer_add_tree = buf =>
  fun
  | Empty => ()
  | Node((c, x1, x2), l, r) => {
      Buffer.add_string(buf, Printf.sprintf("('%c', %d, %d)", c, x1, x2));
      switch (l, r) {
      | (Empty, Empty) => ()
      | (_, _) =>
        Buffer.add_char(buf, '(');
        buffer_add_tree(buf, l);
        Buffer.add_char(buf, ',');
        buffer_add_tree(buf, r);
        Buffer.add_char(buf, ')');
      };
    };

let string_of_tree = t => {
  let buf = Buffer.create(128);
  buffer_add_tree(buf, t);
  Buffer.contents(buf);
};

let example_layout_tree = {
  let leaf = x => Node(x, Empty, Empty);
  Node(
    'n',
    Node(
      'k',
      Node('c', leaf('a'), Node('h', Node('g', leaf('e'), Empty), Empty)),
      leaf('m'),
    ),
    Node('u', Node('p', Empty, Node('s', leaf('q'), Empty)), Empty),
  );
};

let example3_layout_tree =
  Node(
    'a',
    Node('b', Empty, Node('e', Empty, Node('f', Empty, Empty))),
    Node('c', Empty, Node('d', Node('g', Empty, Empty), Empty)),
  );

let layout_binary_tree_3 = {
  let rec translate_x = d =>
    fun
    | Empty => Empty
    | Node((v, x, y), l, r) =>
      Node((v, x + d, y), translate_x(d, l), translate_x(d, r));
  /* Distance between a left subtree given by its right profile [lr]
     and a right subtree given by its left profile [rl]. */
  let rec dist = (lr, rl) =>
    switch (lr, rl) {
    | ([lrx, ...ltl], [rlx, ...rtl]) => max(lrx - rlx, dist(ltl, rtl))
    | ([], _)
    | (_, []) => 0
    };
  let rec merge_profiles = (p1, p2) =>
    switch (p1, p2) {
    | ([x1, ...tl1], [_, ...tl2]) => [x1, ...merge_profiles(tl1, tl2)]
    | ([], _) => p2
    | (_, []) => p1
    };
  let rec layout = depth =>
    fun
    | Empty => ([], Empty, [])
    | Node(v, l, r) => {
        let (ll, l', lr) = layout(depth + 1, l);
        let (rl, r', rr) = layout(depth + 1, r);
        let d = 1 + dist(lr, rl) / 2;
        let ll = List.map(x => x - d, ll)
        and lr = List.map(x => x - d, lr)
        and rl = List.map((+)(d), rl)
        and rr = List.map((+)(d), rr);
        (
          [0, ...merge_profiles(ll, rl)],
          Node((v, 0, depth), translate_x(- d, l'), translate_x(d, r')),
          [0, ...merge_profiles(rr, lr)],
        );
      };
  t => {
    let (l, t', _) = layout(1, t);
    let x_min = List.fold_left(min, 0, l);
    translate_x(1 - x_min, t');
  };
};

let () = {
  layout_binary_tree_3(example_layout_tree)
  |> string_of_tree
  |> Printf.printf("example_layout_tree after layout: %s\n");
  layout_binary_tree_3(example3_layout_tree)
  |> string_of_tree
  |> Printf.printf("example3_layout_tree after layout: %s\n");
};
