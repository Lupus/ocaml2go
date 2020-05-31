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
package caml_runtime

import (
	. "caml_primitives"
	crypto_rand "crypto/rand"
	"encoding/binary"
	"os"
)

func Caml_sys_const_ostype_cygwin(_ V) V {
	return 0
}

func Caml_sys_const_ostype_unix(_ V) V {
	return 1
}

func Caml_sys_const_ostype_win32(_ V) V {
	return 0
}

func Caml_sys_file_exists(_ ...V) V {
	panic("Primitive Caml_sys_file_exists is not yet implemented")
}

func Caml_sys_get_config(_ V) V {
	return []V{0, Caml_new_string("Unix"), 32, 0}
}

func Caml_sys_getcwd(_ V) V {
	dir, err := os.Getwd()
	Caml_fail_if_err(err)
	return Caml_js_to_string(dir)
}

func Caml_sys_getenv(n_ V) V {
	n := Caml_jsbytes_of_string(n_.(*MlBytes)).(string)
	if v, ok := os.LookupEnv(n); ok {
		return Caml_js_to_string(v)
	} else {
		return Caml_raise_not_found()
	}
}

func Caml_sys_is_directory(_ ...V) V {
	panic("Primitive Caml_sys_is_directory is not yet implemented")
}

func Caml_sys_open(_ ...V) V {
	panic("Primitive Caml_sys_open is not yet implemented")
}

func Caml_sys_random_seed(_ V) V {
	var b [8]byte
	_, err := crypto_rand.Read(b[:])
	if err != nil {
		panic("cannot seed with cryptographically secure random number generator")
	}
	return []V{0, int(binary.LittleEndian.Uint64(b[:]))}
}

func Caml_sys_remove(_ ...V) V {
	panic("Primitive Caml_sys_remove is not yet implemented")
}

func Caml_sys_system_command(_ ...V) V {
	panic("Primitive Caml_sys_system_command is not yet implemented")
}

func Caml_sys_unsafe_getenv(_ ...V) V {
	panic("Primitive Caml_sys_unsafe_getenv is not yet implemented")
}
