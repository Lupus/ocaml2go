let strings = ["a","b","c"];
let ints = [1,2,3,4,5];
type r = {
    foo: int,
    bar: string,
    baz: float,
};

let rs = [{foo: 1, bar: "2", baz: 3.0}];

let string_of_r = ({baz,_}) => string_of_float(baz);

strings
|> List.map(x => "string: " ++ x)
|> List.iter(print_endline);

ints
|> List.map(string_of_int)
|> List.iter(print_endline);

rs
|> List.map(string_of_r)
|> List.iter(print_endline);

print_endline("hello world");
