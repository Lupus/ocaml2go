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
/*
 package main

 import (
 	"fmt"
 	"go/importer"
 	"go/types"
 	"log"
 )

 func main() {
 	conf := types.Config{Importer: importer.Default()}

 	pkg, err := conf.Importer.Import("go/types")
 	if err != nil {
 		log.Fatal(err) // type error
 	}

 	fmt.Printf("Package  %q\n", pkg.Path())
 	fmt.Printf("Name:    %s\n", pkg.Name())
 	fmt.Printf("Imports: %s\n", pkg.Imports())
 	fmt.Printf("Scope:   %s\n", pkg.Scope())
 }
 */

let () = {
  let default_importer = Types.Importer.default();
  let conf = Types.Config.create();
  Types.Config.set_importer(conf, default_importer);
  let (pkg,err) = Types.Importer.import(Types.Config.importer(conf), "go/types");
  if (!Go_error.is_nil(err)) {
      Printf.printf("failed to import: %s\n%!", Go_error.error(err));
      exit(1);
  };
  Printf.printf("Package  %s\n", Types.Package.path(pkg));
  Printf.printf("Name:    %s\n", Types.Package.name(pkg));
};
