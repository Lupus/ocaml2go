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
module Package = {
    type t;
    external name: t => string = {|
    <@.exec>
      <@caml_js_to_string><@><@1/>.(*types.Package).Name()</@></@caml_js_to_string>
    </@.exec>
    |}

    external path: t => string = {|
    <@.exec>
      <@caml_js_to_string><@><@1/>.(*types.Package).Path()</@></@caml_js_to_string>
    </@.exec>
    |}
};

module Importer = {
    type t;

    external default: unit => t = {|
    <@.exec>
      <@require>go/importer</@require>.Default()
    </@.exec>
    |}

    external import: (t, string) => (Package.t, Go_error.t) = {|
    <@.exec>
      (func (path_ V) V {
          path := <@caml_jsbytes_of_string><@>path_<@/></@caml_jsbytes_of_string>.(string)
          x, e := <@1/>.(types.Importer).Import(path)
          return []V{0, x, e}
      })(<@2/>)
    </@.exec>
    |}
};

module Config = {
    type t;

    external set_importer: (t, Importer.t) => unit = {|
    <@.exec>
      /* !type(<@1/> : *types.Config) */
      <@1/>.(*types.Config).Importer = <@2/>.(types.Importer)
    </@.exec>
    |}

    external importer: t => Importer.t = {|
    <@.exec>
      /* !type(<@1/> : *types.Config) */
      <@1/>.(*types.Config).Importer
    </@.exec>
    |}

    external create: unit => t = {|
    <@.exec>
      &<@require>go/types</@require>.Config{}
    </@.exec>
    |}
};
