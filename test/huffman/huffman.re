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

/* Simple priority queue where the priorities are integers 0..100.
   The node with the lowest probability comes first. */
module Pq = {
  type t('a) = {
    data: array(list('a)),
    mutable first: int,
  };
  let make = () => {data: Array.make(101, []), first: 101};

  let add = (q, p, x) => {
    q.data[p] = [x, ...q.data[p]];
    q.first = min(p, q.first);
  };

  let get_min = q =>
    if (q.first == 101) {
      None;
    } else {
      switch (q.data[q.first]) {
      | [] => assert(false)
      | [x, ...tl] =>
        let p = q.first;
        q.data[q.first] = tl;
        while (q.first < 101 && q.data[q.first] == []) {
          q.first = q.first + 1;
        };
        Some((p, x));
      };
    };
};

type tree =
  | Leaf(string)
  | Node(tree, tree);

let rec huffman_tree = q =>
  switch (Pq.get_min(q), Pq.get_min(q)) {
  | (Some((p1, t1)), Some((p2, t2))) =>
    Pq.add(q, p1 + p2, [@implicit_arity] Node(t1, t2));
    huffman_tree(q);
  | (Some((_, t)), None)
  | (None, Some((_, t))) => t
  | (None, None) => assert(false)
  };

/* Build the prefix-free binary code from the tree */
let rec prefixes_of_tree = prefix =>
  fun
  | Leaf(s) => [(s, prefix)]
  | [@implicit_arity] Node(t0, t1) =>
    prefixes_of_tree(prefix ++ "0", t0) @ prefixes_of_tree(prefix ++ "1", t1);

let huffman = fs => {
  if (List.fold_left((s, (_, p)) => s + p, 0, fs) != 100) {
    failwith("huffman: sum of weights must be 100");
  };
  let q = Pq.make();
  List.iter(((s, f)) => Pq.add(q, f, Leaf(s)), fs);
  prefixes_of_tree("", huffman_tree(q));
};

let print_result = lst => {
  List.map(((key, value)) => Printf.sprintf("(%s,%s)", key, value), lst)
  |> String.concat(", ")
  |> Printf.printf("[%s]\n");
};

let () = {
  let fs = [
    ("a", 45),
    ("b", 13),
    ("c", 12),
    ("d", 16),
    ("e", 9),
    ("f", 5),
  ];
  huffman(fs) |> print_result;
  huffman([("a", 10), ("b", 15), ("c", 30), ("d", 16), ("e", 29)])
  |> print_result;
};
