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
package caml_primitives

import (
	"fmt"
	"math"
	"os"
	"reflect"
	"runtime/debug"
	"strconv"
	"unsafe"
)

type V = interface{}

type Vmap map[string]V

type Comparable interface {
	Compare(other V, total bool) int
}

func Mark_as_used(_ ...V) {

}

func Seq__(_ V, x V) V {
	return x
}

func Is_true__(x V) bool {
	switch v := x.(type) {
	case bool:
		return v
	case int:
		return v != 0
	case string:
		panic(fmt.Sprintf("unsupported type for if condition: %T", x))
	default:
		return x != nil
	}
}

func Caml_bool(x bool) V {
	if x {
		return 1
	} else {
		return 0
	}
}

var Caml_true = 1
var Caml_false = 0

func Binop_not_eq_eq__(a V, b V) V {
	return Unop_not__(Binop_eq_eq_eq__(a, b))
}

func Binop_eq_eq_eq__(a V, b V) V {
	at := reflect.TypeOf(a)
	bt := reflect.TypeOf(b)
	if at != bt {
		return Caml_false
	}
	switch v := a.(type) {
	case int:
		return Caml_bool(v == b.(int))
	case string:
		return Caml_bool(v == b.(string))
	case float64:
		if math.IsNaN(v) && math.IsNaN(b.(float64)) {
			return Caml_true
		}
		return v == b.(float64)
	case []V:
		v2 := b.([]V)
		h1, h2 := (*reflect.SliceHeader)(unsafe.Pointer(&v)), (*reflect.SliceHeader)(unsafe.Pointer(&v2))
		return Caml_bool(h1.Data == h2.Data)
	default:
		if reflect.ValueOf(a).Kind() == reflect.Ptr && reflect.ValueOf(b).Kind() == reflect.Ptr {
			return Caml_bool(a == b)
		}
		panic(fmt.Sprintf("unsupported types for Binop_eq_eq_eq__: %s and %s", reflect.TypeOf(a), reflect.TypeOf(b)))
	}
}

func Binop_eq_eq__(a V, b V) V {
	return Caml_bool(reflect.DeepEqual(a, b))
}

func Unop_is_int__(x V) V {
	_, ok := x.(int)
	return Caml_bool(ok)
}

func Unop_to_int__(i V) V {
	return i.(int)
}

func Unop_int_to_string__(i V) V {
	return strconv.Itoa(i.(int))
}

func Unop_not__(x V) V {
	return Caml_bool(!Is_true__(x))
}

func Unop_neg__(x V) V {
	return -(x.(int))
}

func Unop_float_neg__(x V) V {
	return -(x.(float64))
}

func Binop_int_plus__(a V, b V) V {
	return a.(int) + b.(int)
}

func Binop_plus__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v + v2
		case string:
			x, err := strconv.Atoi(v2)
			if err != nil {
				panic(err)
			}
			return v + x
		default:
			panic("mismatched types for Binop_plus__")
		}
	case string:
		switch v2 := b.(type) {
		case int:
			return v + strconv.Itoa(v2)
		case string:
			return v + v2
		default:
			panic("mismatched types for Binop_plus__")
		}
	default:
		panic("mismatched types for Binop_plus__")
	}
}

func Binop_eq__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return Caml_bool(v == v2)
		case string:
			x, err := strconv.Atoi(v2)
			if err != nil {
				panic(err)
			}
			return Caml_bool(v == x)
		default:
			panic("mismatched types for Binop_eq__")
		}
	case float64:
		switch v2 := b.(type) {
		case int:
			return Caml_bool(v == float64(v2))
		case float64:
			return Caml_bool(v == v2)
		default:
			panic("mismatched types for Binop_eq__")
		}
	case string:
		switch v2 := b.(type) {
		case int:
			return Caml_bool(v == strconv.Itoa(v2))
		case string:
			return Caml_bool(v == v2)
		default:
			panic("mismatched types for Binop_eq__")
		}
	default:
		panic("mismatched types for Binop_eq__")
	}
}

func Binop_not_eq__(a V, b V) V {
	return Unop_not__(Binop_eq__(a, b))
}

func Binop_div__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v / v2
		case float64:
			return float64(v) / v2
		default:
			panic("mismatched types for Binop_div__")
		}
	case float64:
		switch v2 := b.(type) {
		case int:
			return v / float64(v2)
		case float64:
			return v / v2
		default:
			panic("mismatched types for Binop_div__")
		}
	default:
		panic("mismatched types for Binop_div__")
	}
}

func Binop_minus__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v - v2
		case float64:
			return float64(v) - v2
		default:
			panic("mismatched types for Binop_minus__")
		}
	case float64:
		switch v2 := b.(type) {
		case int:
			return v - float64(v2)
		case float64:
			return v - v2
		default:
			panic("mismatched types for Binop_minus__")
		}
	default:
		panic("mismatched types for Binop_minus__")
	}
}

func Binop_mod__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v % v2
		default:
			panic("mismatched types for Binop_mod__")
		}
	default:
		panic("mismatched types for Binop_mod__")
	}
}

func Unop_bnot__(x V) V {
	return ^(x.(int))
}

func Binop_bor__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v | v2
		default:
			panic("mismatched types for Binop_bor__")
		}
	default:
		panic("mismatched types for Binop_bor__")
	}
}

func Binop_band__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v & v2
		default:
			panic("mismatched types for Binop_band__")
		}
	default:
		panic("mismatched types for Binop_band__")
	}
}

func Binop_bxor__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v ^ v2
		default:
			panic("mismatched types for Binop_bxor__")
		}
	default:
		panic("mismatched types for Binop_bxor__")
	}
}

func Binop_mul__(a V, b V) V {
	switch v := a.(type) {
	case int:
		switch v2 := b.(type) {
		case int:
			return v * v2
		case float64:
			return float64(v) * v2
		default:
			panic("mismatched types for Binop_mul__")
		}
	case float64:
		switch v2 := b.(type) {
		case int:
			return v * float64(v2)
		case float64:
			return v * v2
		default:
			panic("mismatched types for Binop_mul__")
		}
	default:
		panic("mismatched types for Binop_mul__")
	}
}

func Binop_float_plus__(a V, b V) V {
	return a.(float64) + b.(float64)
}

func Binop_and__(a V, b V) V {
	return Caml_bool(Is_true__(a) && Is_true__(b))
}

func Binop_lt__(a V, b V) V {
	switch v := a.(type) {
	case int:
		return Caml_bool(v < b.(int))
	case float64:
		return Caml_bool(v < b.(float64))
	case string:
		return Caml_bool(v < b.(string))
	default:
		panic("mismatched types for Binop_lt__")
	}
}

func Binop_le__(a V, b V) V {
	switch v := a.(type) {
	case int:
		return Caml_bool(v <= b.(int))
	case float64:
		return Caml_bool(v <= b.(float64))
	case string:
		return Caml_bool(v <= b.(string))
	default:
		panic("mismatched types for Binop_le__")
	}
}

func Binop_gt__(a V, b V) V {
	switch v := a.(type) {
	case int:
		return Caml_bool(v > b.(int))
	case float64:
		return Caml_bool(v > b.(float64))
	case string:
		return Caml_bool(v > b.(string))
	default:
		panic("mismatched types for Binop_gt__")
	}
}

func Binop_ge__(a V, b V) V {
	switch v := a.(type) {
	case int:
		return Caml_bool(v >= b.(int))
	case float64:
		return Caml_bool(v >= b.(float64))
	case string:
		return Caml_bool(v >= b.(string))
	default:
		panic("mismatched types for Binop_ge__")
	}
}

func Binop_float_lt__(a V, b V) V {
	return Caml_bool(a.(float64) < b.(float64))
}

func Binop_float_le__(a V, b V) V {
	return Caml_bool(a.(float64) <= b.(float64))
}

func Binop_float_gt__(a V, b V) V {
	return Caml_bool(a.(float64) > b.(float64))
}

func Binop_float_ge__(a V, b V) V {
	return Caml_bool(a.(float64) >= b.(float64))
}

func Binop_float_minus__(a V, b V) V {
	return a.(float64) - b.(float64)
}

func Binop_float_mul__(a V, b V) V {
	return a.(float64) * b.(float64)
}

func Binop_float_div__(a V, b V) V {
	return a.(float64) / b.(float64)
}

func Binop_float_eq_eq__(a V, b V) V {
	return Caml_bool(a.(float64) == b.(float64))
}

func Binop_float_not_eq__(a V, b V) V {
	return Caml_bool(a.(float64) != b.(float64))
}

func Unhandled_panic__() {
	if r := recover(); r != nil {
		switch v := r.(type) {
		case error:
			fmt.Printf("Unhandled panic: %s\nStacktrace: \n%s\n", v.Error(), string(debug.Stack()))
		default:
			fmt.Printf("Unhandled panic: %#v\nStacktrace: \n%s\n", r, string(debug.Stack()))
		}
		os.Exit(1)
	}
}

func If_else__(expr func() V, if_true func() V, if_false func() V) V {
	e := expr()
	r := false
	if b, is_bool := e.(bool); is_bool {
		r = b
	} else {
		r = e != nil
	}
	if r {
		return if_true()
	} else {
		return if_false()
	}
}

var Math V = Vmap{
	"sqrt": func(x_ V) V {
		x := x_.(float64)
		return math.Sqrt(x)
	},
}

func Copy__(x_ V) V {
	x := x_.([]V)
	y := make([]V, len(x))
	copy(y, x)
	return y
}

func Unop_int_to_float__(x V) V {
	return float64(x.(int))
}
