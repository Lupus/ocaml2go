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
	"bytes"
	. "caml_primitives"
	"crypto/md5"
	"fmt"
	"math"
	"os"
	"reflect"
	"runtime/debug"
	"strconv"
	"strings"
	"unicode"
)

type MlBytes struct {
	t int
	a []byte
	c string
	l int
}

func (s *MlBytes) String() string {
	return Caml_jsbytes_of_string(s).(string)
}

func (s *MlBytes) GoString() string {
	return fmt.Sprintf("%q", s.String())
}

var global_data = make(map[V]V, 0)

var caml_named_values = make(map[string]V, 0)

func Caml_new_string(s_ V) V {
	s := s_.(string)
	return &MlBytes{0, []byte{}, s, len(s)}
}

func Caml_ml_string_length(s_ V) V {
	s := s_.(*MlBytes)
	return s.l
}

func Caml_ml_bytes_length(s_ V) V {
	s := s_.(*MlBytes)
	return s.l
}

func is_ascii(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] > unicode.MaxASCII {
			return false
		}
	}
	return true
}

var last_oo_counter = 0

func Caml_fresh_oo_id(_ V) V {
	last_oo_counter += 1
	return last_oo_counter
}

func Caml_register_global(n V, v V, name_opt V) V {
	global_data[n] = v
	if name, ok := name_opt.(string); ok {
		global_data[name] = v
	}
	return nil
}

type Channel struct {
	file   *os.File
	buffer strings.Builder
}

var channels = make(map[int]*Channel)

func Caml_ml_open_descriptor_in(fd_ V) V {
	fd := fd_.(int)
	channel := &Channel{file: os.NewFile(uintptr(fd), "caml-fd-in")}
	channels[fd] = channel
	return fd
}

func Caml_ml_open_descriptor_out(fd_ V) V {
	fd := fd_.(int)
	channel := &Channel{file: os.NewFile(uintptr(fd), "caml-fd-out")}
	channels[fd] = channel
	return fd
}

func Caml_jsbytes_of_string(s_ V) V {
	s := s_.(*MlBytes)
	if (s.t & 6) != 0 {
		Caml_convert_string_to_bytes(s)
	}
	return s.c
}

func Caml_js_to_string(s_ V) V {
	s := s_.(string)
	tag := 9 /* BYTES | ASCII */
	if !is_ascii(s) {
		tag = 8 /* BYTES | NOT_ASCII */
	}
	return &MlBytes{tag, []byte{}, s, len(s)}
}

func Caml_convert_string_to_bytes(s_ V) V {
	s := s_.(*MlBytes)
	if s.t == 2 {
		remainder := make([]byte, s.l-len(s.c))
		s.c += string(remainder)
	} else {
		// Then it must be array (type 4)
		s.c = Caml_subarray_to_string(s.a, 0, len(s.c)).(string)
	}
	s.t = 9
	return nil
}

func Caml_ml_output_bytes(chanid_ V, buffer_ V, offset_ V, length_ V) V {
	chanid := chanid_.(int)
	buffer := buffer_.(*MlBytes)
	offset := offset_.(int)
	length := length_.(int)
	channel := channels[chanid]
	var s *MlBytes
	if offset == 0 && Caml_ml_bytes_length(buffer).(int) == length {
		s = buffer
	} else {
		s = Caml_create_bytes(length).(*MlBytes)
		Caml_blit_bytes(buffer, offset, s, 0, length)
	}
	channel.buffer.WriteString(Caml_jsbytes_of_string(s).(string))
	return 0
}

var Caml_ml_output = Caml_ml_output_bytes

func Caml_ml_output_char(chanid_ V, char_ V) V {
	chanid := chanid_.(int)
	char := char_.(int)
	channel := channels[chanid]
	channel.buffer.WriteString(string(char))
	return 0
}

func Caml_ml_flush(chanid_ V) V {
	chanid := chanid_.(int)
	channel := channels[chanid]
	channel.file.Write([]byte(channel.buffer.String()))
	channel.buffer.Reset()
	return 0
}

func Caml_ml_out_channels_list(_ V) V {
	var l V = nil
	for fd := range channels {
		l = []V{0, fd, l}
	}
	return l
}

func Is_int(x V) V {
	_, ok := x.(int)
	return ok
}

func Caml_arity_test(f V) V {
	ft := reflect.TypeOf(f)
	return ft.NumIn()
}

func Caml_call1(f V, x1 V) V {
	switch v := f.(type) {
	case func(V) V:
		return v(x1)
	default:
		return Caml_call_gen(f, []V{x1})
	}
}

func Caml_call2(f V, x1 V, x2 V) V {
	switch v := f.(type) {
	case func(V, V) V:
		return v(x1, x2)
	default:
		return Caml_call_gen(f, []V{x1, x2})
	}
}

func Caml_call3(f V, x1 V, x2 V, x3 V) V {
	switch v := f.(type) {
	case func(V, V, V) V:
		return v(x1, x2, x3)
	default:
		return Caml_call_gen(f, []V{x1, x2, x3})
	}
}

func fn_type(n int) reflect.Type {
	fmt.Printf("len: %d\n", n)
	args := make([]reflect.Type, n)
	var a_v V
	var vt = reflect.TypeOf(a_v)
	for i := 0; i < n; i++ {
		args[i] = vt
	}
	results := []reflect.Type{vt}
	return reflect.FuncOf(args, results, false)
}

func invoke(ft reflect.Type, f V, args []V) V {
	switch ft.NumIn() {
	case 2:
		return f.(func(V, V) V)(args[0], args[1])
	case 3:
		return f.(func(V, V, V) V)(args[0], args[1], args[2])
	case 4:
		return f.(func(V, V, V, V) V)(args[0], args[1], args[2], args[3])
	case 5:
		return f.(func(V, V, V, V, V) V)(args[0], args[1], args[2], args[3], args[4])
	case 6:
		return f.(func(V, V, V, V, V, V) V)(args[0], args[1], args[2], args[3], args[4], args[5])
	case 7:
		return f.(func(V, V, V, V, V, V, V) V)(args[0], args[1], args[2], args[3], args[4], args[5], args[6])
	case 8:
		return f.(func(V, V, V, V, V, V, V, V) V)(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7])
	default:
		fun := reflect.ValueOf(f)
		vargs := make([]reflect.Value, len(args))
		for i, a := range args {
			vargs[i] = reflect.ValueOf(a)
		}
		results := fun.Call(vargs)
		return results[0].Interface()
	}
}

func Caml_call_gen(f V, args []V) V {
	ft := reflect.TypeOf(f)
	n := ft.NumIn()
	args_len := len(args)
	d := n - args_len
	if d == 0 {
		return invoke(ft, f, args)
	} else if d < 0 {
		return Caml_call_gen(invoke(ft, f, args[0:n]),
			args[n:args_len])
	} else {
		return func(x V) V {
			return Caml_call_gen(f, append(args, x))
		}
	}
}

type GoPanic struct {
	msg        string
	stacktrace string
}

func Caml_try(block_ V) (ret V) {
	block := block_.(func() V)
	defer func() {
		if r := recover(); r != nil {
			switch v := r.(type) {
			case []V:
				ret = []V{1, r}
			case error:
				ret = []V{1, GoPanic{msg: v.Error(), stacktrace: string(debug.Stack())}}
			default:
				ret = []V{1, GoPanic{msg: fmt.Sprintf("%#v", r), stacktrace: string(debug.Stack())}}
			}
		}
	}()
	return []V{0, block()}
}

var Caml_wrap_thrown_exception_reraise = Caml_wrap_thrown_exception

/*
Caml_wrap_exception receives a value, which was cought by recover(), might be
whatever was passed to panic ()
*/
func Caml_wrap_exception(e_ V) V {
	// Normal exceptions are arrays
	if e, ok := e_.([]V); ok {
		return e
	}

	// Golang panic
	if e, ok := e_.(GoPanic); ok {
		v := []V{0,
			global_data["Failure"],
			Caml_new_string(
				fmt.Sprintf("Native Golang panic: %s\nStacktrace:%s\n", e.msg, e.stacktrace)),
		}
		return v
	}

	//fallback: wrapped in Failure
	return []V{0, global_data["Failure"], Caml_new_string(fmt.Sprintf("%#v", e_))}
}

/*
Caml_wrap_thrown_exception is called right before the value is thrown
*/
func Caml_wrap_thrown_exception(e V) V {
	/* No wrapping is needed to pass e to panic() */
	return e
}

func Caml_fail_if_err(err error) {
	if err != nil {
		Caml_failwith(err.Error())
	}
}

func Caml_failwith(msg string) V {
	return Caml_raise_with_string(global_data["Failure"], msg)
}

func Caml_raise_with_string(tag V, msg string) V {
	return Caml_raise_with_arg(tag, Caml_new_string(msg))
}

func Caml_raise_with_arg(tag V, arg V) V {
	panic(Caml_wrap_thrown_exception([]V{0, tag, arg}))
}

func Caml_raise_constant(tag V) V {
	panic(Caml_wrap_thrown_exception(tag))
}

func Caml_raise_not_found() V {
	return Caml_raise_constant(global_data["Not_found"])
}

func Caml_raise_zero_divide() V {
	return Caml_raise_constant(global_data["Division_by_zero"])
}

func Caml_invalid_argument(msg string) V {
	return Caml_raise_with_string(global_data["Invalid_argument"], msg)
}

func Caml_string_bound_error() V {
	return Caml_invalid_argument("index out of bounds")
}

func Caml_subarray_to_string(a_ V, i_ V, length_ V) V {
	a := a_.([]byte)
	i := i_.(int)
	length := length_.(int)
	return string(a[i : i+length])
}

func Caml_convert_string_to_array(s_ V) V {
	s := s_.(*MlBytes)
	b := s.c
	l := len(b)
	a := make([]byte, s.l)
	for i := 0; i < l; i++ {
		a[i] = byte(b[i])
	}
	s.a = a
	s.t = 4
	return a
}

func int_min(x, y int) int {
	if x > y {
		return y
	}
	return x
}

func Caml_blit_bytes(s1_ V, i1_ V, s2_ V, i2_ V, length_ V) V {
	s1 := s1_.(*MlBytes)
	i1 := i1_.(int)
	s2 := s2_.(*MlBytes)
	i2 := i2_.(int)
	length := length_.(int)
	if length == 0 {
		return 0
	}
	if (i2 == 0) && (length >= s2.l || (s2.t == 2 && length >= len(s2.c))) {
		// Array
		if s1.t == 4 {
			s2.c = Caml_subarray_to_string(s1.a, i1, length).(string)
		} else {
			if i1 == 0 && len(s1.c) == length {
				s2.c = s1.c
			} else {
				s2.c = s1.c[i1 : i1+length]
			}
		}
		if len(s2.c) == s2.l {
			s2.t = 0
		} else {
			s2.t = 2
		}
	} else {
		// Partial
		if s2.t == 2 && i2 == len(s2.c) {
			if s1.t == 4 {
				s2.c += Caml_subarray_to_string(s1.a, i1, length).(string)
			} else {
				if i1 == 0 && len(s1.c) == length {
					s2.c += s1.c
				} else {
					s2.c += s1.c[i1 : i1+length]
				}
			}
			if len(s2.c) == s2.l {
				s2.t = 0
			} else {
				s2.t = 2
			}
		} else {
			if s2.t != 4 {
				Caml_convert_string_to_array(s2)
			}
			// Array
			if s1.t == 4 {
				if i2 <= i1 {
					for i := 0; i < length; i++ {
						s2.a[i2+i] = s1.a[i1+i]
					}
				} else {
					for i := length - 1; i >= 0; i-- {
						s2.a[i2+i] = s1.a[i1+i]
					}
				}
			} else {
				l := int_min(length, len(s1.c)-i1) // ?? 0;
				var i int
				for i = 0; i < l; i++ {
					s2.a[i2+i] = byte(s1.c[i1+i])
				}
				for ; i < length; i++ {
					s2.a[i2+i] = 0
				}
			}
		}
	}
	return 0
}

var Caml_blit_string = Caml_blit_bytes

func Caml_create_bytes(length_ V) V {
	length := length_.(int)
	if length < 0 {
		panic("Bytes.create")
	}
	var t int
	if length != 0 {
		t = 2
	} else {
		t = 9
	}
	return &MlBytes{t, []byte{}, "", length}
}

func Caml_string_of_bytes(s_ V) V {
	s := s_.(*MlBytes)
	if s.t == 4 {
		new_s := (string)(s.a)
		return &MlBytes{0, []byte{}, new_s, len(new_s)}
	}
	return s
}

func Caml_bytes_of_string(s_ V) V {
	s := s_.(*MlBytes)
	return s
}

func Caml_sys_const_backend_type(_ V) V {
	return nil
}

func Caml_mul(a_ V, b_ V) V {
	return a_.(int) * b_.(int)
}

func Left_shift_32(a V, b V) V {
	return int(int32(a.(int)) << uint(b.(int)))
}

func Right_shift_32(a V, b V) V {
	return int(int32(a.(int)) >> uint(b.(int)))
}

func Unsigned_right_shift_32(a V, b V) V {
	return int(uint32(a.(int)) >> uint(b.(int)))
}

type trampoline struct {
	tramp V
	args  []V
}

func Caml_trampoline_return(f V, args_ V) V {
	args := args_.([]V)
	return trampoline{tramp: f, args: args}
}

func Caml_trampoline(res V) V {
loop:
	for {
		switch v := res.(type) {
		case trampoline:
			ft := reflect.TypeOf(v.tramp)
			res = invoke(ft, v.tramp, v.args)
			continue
		default:
			break loop
		}
	}
	return res
}

func Caml_bytes_set(s_ V, i_ V, c_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	c := c_.(int)
	if i >= s.l {
		panic("index out of bounds")
	}
	return Caml_bytes_unsafe_set(s, i, c)
}

func Caml_bytes_unsafe_set(s_ V, i_ V, c_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	c := c_.(int)
	c &= 0xff
	if s.t != 4 {
		Caml_convert_string_to_array(s)
	}
	s.a[i] = byte(c)
	return nil
}

var Caml_string_unsafe_set = Caml_bytes_unsafe_set
var Caml_string_set = Caml_bytes_set

func Caml_bytes_get(s_ V, i_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	if i >= s.l {
		panic("index out of bounds")
	}
	return Caml_bytes_unsafe_get(s, i)
}

func Caml_bytes_unsafe_get(s_ V, i_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	switch s.t & 6 {
	case 0:
		return int(s.c[i])
	case 4:
		return int(s.a[i])
	default:
		if i >= len(s.c) {
			return 0
		}
		return int(s.c[i])
	}
}

var Caml_string_unsafe_get = Caml_bytes_unsafe_get
var Caml_string_get = Caml_bytes_get

func Caml_check_bound(array_ V, index_ V) V {
	array := array_.([]V)
	index := index_.(int)
	if index >= len(array)-1 {
		panic("index out of bounds")
	}
	return array
}

func Caml_format_int(format_ml_ V, x_ V) V {
	format_ml := format_ml_.(*MlBytes)
	format := Caml_jsbytes_of_string(format_ml).(string)
	x := x_.(int)
	str := fmt.Sprintf(format, x)
	return Caml_js_to_string(str)
}

var Caml_int64_format = Caml_format_int

func Caml_format_float(format_ml_ V, x_ V) V {
	format_ml := format_ml_.(*MlBytes)
	format := Caml_jsbytes_of_string(format_ml).(string)
	x := x_.(float64)
	str := fmt.Sprintf(format, x)
	return Caml_js_to_string(str)
}

func Caml_string_compare(s1_ V, s2_ V) V {
	s1 := s1_.(*MlBytes)
	s2 := s2_.(*MlBytes)
	if s1.t&6 != 0 {
		Caml_convert_string_to_bytes(s1)
	}
	if s2.t&6 != 0 {
		Caml_convert_string_to_bytes(s2)
	}
	switch {
	case s1.c < s2.c:
		return -1
	case s1.c > s2.c:
		return 1
	default:
		return 0
	}
}

func Caml_string_notequal(s1 V, s2 V) V {
	return Caml_string_compare(s1, s2).(int) != 0
}

func Caml_string_equal(s1 V, s2 V) V {
	return Caml_string_compare(s1, s2).(int) == 0
}

func Caml_int_compare(a_ V, b_ V) V {
	a := a_.(int)
	b := b_.(int)
	if a < b {
		return (-1)
	}
	if a == b {
		return 0
	}
	return 1
}

var Caml_int64_compare = Caml_int_compare

func is_mlbytes(x V) bool {
	x, ok := x.(*MlBytes)
	return ok
}

func int_or_zero(x V) V {
	if x, ok := x.(int); ok {
		return x
	}
	return 0
}

func is_array(x_ V) bool {
	x, ok := x_.([]V)
	return ok && Is_true__(Binop_eq_eq_eq__(x[0], int_or_zero(x[0])))
}

func is_number(x V) bool {
	switch v := x.(type) {
	case int:
		return true
	case float64:
		return true
	default:
		Mark_as_used(v)
		return false
	}
}

func is_comparable(x V) bool {
	x, ok := x.(Comparable)
	return ok
}

type Stack []V

func (s *Stack) IsEmpty() bool {
	return len(*s) == 0
}

func (s *Stack) Push(v V) {
	*s = append(*s, v)
}

func (s *Stack) Pop() V {
	if s.IsEmpty() {
		panic("attempt to pop from empty stack")
	} else {
		index := len(*s) - 1
		element := (*s)[index]
		*s = (*s)[:index]
		return element
	}
}

func Caml_compare_val(a V, b V, total_ V) V {
	total := total_.(int)
	var stack Stack
	for {
		if !(Is_true__(total) && Is_true__(Binop_eq_eq_eq__(a, b))) {
			if is_mlbytes(a) {
				if is_mlbytes(b) {
					if Is_true__(Binop_not_eq_eq__(a, b)) {
						var x = Caml_string_compare(a, b).(int)
						if x != 0 {
							return x
						}
					}
				} else {
					// Should not happen
					return 1
				}
			} else if is_array(a) {
				ta := a.([]V)[0].(int)
				// ignore double_array_tag
				if ta == 254 {
					ta = 0
				}
				// Forward object
				if ta == 250 {
					a = a.([]V)[1]
					continue
				} else if is_array(b) {
					tb := b.([]V)[0].(int)
					// ignore double_array_tag
					if tb == 254 {
						tb = 0
					}
					// Forward object
					if tb == 250 {
						b = b.([]V)[1]
						continue
					} else if Is_true__(Binop_not_eq_eq__(ta, tb)) {
						if ta < tb {
							return -1
						} else {
							return 1
						}
					} else {
						switch ta {
						case 248:
							{
								// Object
								var x = Caml_int_compare(a.([]V)[2], b.([]V)[2]).(int)
								if x != 0 {
									return x
								}
								break
							}
						case 251:
							{
								panic("equal: abstract value")
							}
						case 255:
							{
								// Int64
								var x = Caml_int64_compare(a, b)
								if x != 0 {
									return x
								}
								break
							}
						default:
							if len(a.([]V)) != len(b.([]V)) {
								if len(a.([]V)) < len(b.([]V)) {
									return -1
								} else {
									return 1
								}
							}
							if len(a.([]V)) > 1 {
								stack.Push(a)
								stack.Push(b)
								stack.Push(1)
							}
						}
					}
				} else {
					return 1
				}
			} else if is_mlbytes(b) || is_array(b) {
				return -1
			} else if !is_number(a) && a != nil && is_comparable(a) {
				var cmp = a.(Comparable).Compare(b, Is_true__(total))
				if cmp != 0 {
					return cmp
				}
			} else if reflect.TypeOf(a).Kind() == reflect.Func {
				panic("compare: functional value")
			} else {
				if Is_true__(Binop_lt__(a, b)) {
					return -1
				}
				if Is_true__(Binop_gt__(a, b)) {
					return 1
				}
				if Is_true__(Binop_not_eq__(a, b)) {
					if !Is_true__(total) {
						return math.NaN()
					}
					if Is_true__(Binop_eq__(a, a)) {
						return 1
					}
					if Is_true__(Binop_not_eq__(b, b)) {
						return -1
					}
				}
			}
		}
		if stack.IsEmpty() {
			return 0
		}
		i := stack.Pop()
		b = stack.Pop()
		a = stack.Pop()
		if i.(int)+1 < len(a.([]V)) {
			stack.Push(a)
			stack.Push(b)
			stack.Push(i.(int) + 1)
		}
		a = a.([]V)[i.(int)]
		b = b.([]V)[i.(int)]
	}
}

func Caml_compare(a V, b V) V {
	return Caml_compare_val(a, b, Caml_true)
}

func Caml_equal(a V, b V) V {
	return Caml_bool(Caml_compare_val(a, b, Caml_false).(int) == 0)
}

func Caml_notequal(a V, b V) V {
	return Caml_bool(Caml_compare_val(a, b, Caml_false).(int) != 0)
}

func Caml_greaterequal(a V, b V) V {
	return Caml_bool(Caml_compare_val(a, b, Caml_false).(int) >= 0)
}

func Caml_greaterthan(a V, b V) V {
	return Caml_bool(Caml_compare_val(a, b, Caml_false).(int) > 0)
}

func Caml_lessequal(a V, b V) V {
	return Caml_bool(Caml_compare_val(a, b, Caml_false).(int) <= 0)
}

func Caml_lessthan(a V, b V) V {
	return Caml_bool(Caml_compare_val(a, b, Caml_false).(int) < 0)
}

func Caml_sys_get_argv(_ V) V {
	main := os.Args[0]
	args := os.Args[1:]
	p := Caml_js_to_string(main)
	args2 := []V{0, p}
	for i := 0; i < len(args); i++ {
		args2 = append(args2, Caml_js_to_string(args[i]))
	}
	return []V{0, p, args2}
}

func Caml_fill_bytes(s_ V, i_ V, l_ V, c_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	l := l_.(int)
	c := c_.(int)
	if l > 0 {
		if i == 0 && (l >= s.l || (s.t == 2 && l >= len(s.c))) {
			if c == 0 {
				s.c = ""
				s.t = 2
			} else {
				s.c = string(bytes.Repeat([]byte{byte(c)}, l))
				if l == s.l {
					s.t = 0
				} else {
					s.t = (2)
				}
			}
		} else {
			if s.t != 4 {
				Caml_convert_string_to_array(s)
			}
			for l += i; i < l; i++ {
				s.a[i] = byte(c)
			}
		}
	}
	return nil
}

func Caml_hexstring_of_float(x_ V, prec_ V, style_ V) V {
	x := x_.(float64)
	prec := prec_.(int)
	if prec >= 0 && prec < 13 {
		/* If a precision is given, and is small, round mantissa accordingly */
		var cst = math.Pow(2, float64(prec*4))
		x = math.Round(x*cst) / cst
	}
	str := fmt.Sprintf("%x\n", math.Float64bits(x))
	return Caml_js_to_string(str)
}

func Caml_classify_float(x_ V) V {
	x := x_.(float64)
	if !math.IsInf(x, 0) {
		if math.Abs(x) >= 2.2250738585072014e-308 {
			return 0
		}
		if x != 0 {
			return 1
		}
		return 2
	}
	if math.IsNaN(x) {
		return 4
	} else {
		return 3
	}
}

func Caml_int_of_string(ml_s V) V {
	s := Caml_jsbytes_of_string(ml_s).(string)
	x, err := strconv.Atoi(s)
	if err != nil {
		Caml_failwith(err.Error())
	}
	return x
}

func Caml_sys_const_max_wosize(_ V) V {
	if strconv.IntSize == 64 {
		return (1 << 54) - 1
	} else {
		return (1 << 22) - 1
	}
}

func Caml_make_vect(length_ V, init V) V {
	length := length_.(int)
	v := make([]V, length+1)
	v[0] = 0
	for i := 1; i < length+1; i++ {
		v[i] = init
	}
	return v
}

func Caml_int64_float_of_bits(x_ V) V {
	x := x_.([]V)
	x1 := x[1].(int)
	x2 := x[2].(int)
	x3 := x[3].(int)
	exp := (x3 & 32767) >> 4
	if exp == 2047 {
		if (x1 | x2 | x3&15) == 0 {
			if x3&32768 != 0 {
				return math.Inf(-1)
			} else {
				return math.Inf(+1)
			}
		} else {
			return math.NaN()
		}
	}
	k := math.Pow(2, -24)
	res := (float64(x1)*k+float64(x2))*k + float64(x3&15)
	if exp > 0 {
		res = res + 16
		res *= math.Pow(2, float64(exp)-1027.0)
	} else {
		res *= math.Pow(2, -1026.0)
	}
	if x3&32768 != 0 {
		res = -res
	}
	return float64(res)
}

var dummy_functions = make(map[reflect.Value]V)

func Caml_alloc_dummy_function(_ /*size*/ V, arity_ V) V {
	arity := arity_.(int)
	var fn V
	var stub V
	switch arity {
	case 1:
		fn = func(x1 V) V {
			real_fn := dummy_functions[reflect.ValueOf(fn)].(func(V) V)
			return real_fn(x1)
		}
		stub = func(x1 V) V {
			panic("Calling updateable function before updated")
		}
		dummy_functions[reflect.ValueOf(fn)] = stub
	case 2:
		fn = func(x1 V, x2 V) V {
			real_fn := dummy_functions[reflect.ValueOf(fn)].(func(V, V) V)
			return real_fn(x1, x2)
		}
		stub = func(x1 V, x2 V) V {
			panic("Calling updateable function before updated")
		}
		dummy_functions[reflect.ValueOf(fn)] = stub
	default:
		panic("Unsupported arity for dummy function: " + strconv.Itoa(arity))
	}
	return fn
}

func Caml_update_dummy(x V, y V) V {
	_, x_is_df := dummy_functions[reflect.ValueOf(x)]
	_, y_is_df := dummy_functions[reflect.ValueOf(y)]
	if x_is_df && y_is_df {
		dummy_functions[reflect.ValueOf(x)] = dummy_functions[reflect.ValueOf(y)]
		return 0
	}
	if x_is_df {
		dummy_functions[reflect.ValueOf(x)] = y
		return 0
	}
	xa := x.([]V)
	ya := y.([]V)
	copy(xa, ya)
	return 0
}

func Caml_array_blit(a1_ V, i1_ V, a2_ V, i2_ V, len_ V) V {
	i1 := i1_.(int)
	i2 := i2_.(int)
	len := len_.(int)
	a1 := a1_.([]V)
	a2 := a2_.([]V)
	if i2 <= i1 {
		for j := 1; j <= len; j++ {
			a2[i2+j] = a1[i1+j]
		}
	} else {
		for j := len; j >= 1; j-- {
			a2[i2+j] = a1[i1+j]
		}
	}
	return 0
}

func Caml_sys_exit(x V) V {
	os.Exit(x.(int))
	return 0
}

func Caml_obj_tag(x_ V) V {
	switch x := x_.(type) {
	case []V:
		return x[0].(int)
	case *MlBytes:
		return 252
	default:
		panic(fmt.Sprintf("unexpected value for Caml_obj_tag: %#v", x_))
	}
}

func Caml_array_append(_ ...V) V {
	panic("Primitive Caml_array_append is not yet implemented")
}

func Caml_array_sub(_ ...V) V {
	panic("Primitive Caml_array_sub is not yet implemented")
}

func Caml_channel_descriptor(channel V) V {
	return channel
}

func Caml_convert_raw_backtrace(_ V) V {
	return []V{0}
}

func Caml_restore_raw_backtrace(_ V, _ V) V {
	return 0
}

func Caml_div(_ ...V) V {
	panic("Primitive Caml_div is not yet implemented")
}

var caml_ephe_key_offset = 3
var caml_ephe_data_offset = 2

func Caml_ephe_create(n_ V) V {
	n := n_.(int)
	if n < 0 {
		return Caml_invalid_argument("Weak.create")
	}
	x := make([]V, caml_ephe_key_offset+n)
	x[0] = 251
	x[1] = "caml_ephe_list_head"
	return x
}

func Caml_weak_set(x_ V, i_ V, v V) V {
	i := i_.(int)
	x := x_.([]V)
	if i < 0 || caml_ephe_key_offset+i >= len(x) {
		return Caml_invalid_argument("Weak.set")
	}
	x[caml_ephe_key_offset+i] = v
	return 0
}

func Caml_weak_get(x_ V, i_ V) V {
	i := i_.(int)
	x := x_.([]V)
	if i < 0 || caml_ephe_key_offset+i >= len(x) {
		return Caml_invalid_argument("Weak.set")
	}
	if x[caml_ephe_key_offset+i] == nil {
		return 0
	} else {
		return x[caml_ephe_key_offset+i]
	}
	return 0
}

func Caml_ephe_get_data(x_ V) V {
	x := x_.([]V)
	if x[caml_ephe_data_offset] == nil {
		return 0
	} else {
		return []V{0, x[caml_ephe_data_offset]}
	}
}

func Caml_ephe_get_key(x V, i V) V {
	return Caml_weak_get(x, i)
}

func Caml_ephe_set_data(x_ V, data V) V {
	x := x_.([]V)
	x[caml_ephe_data_offset] = data
	return 0
}

func Caml_ephe_set_key(x V, i V, v V) V {
	return Caml_weak_set(x, i, []V{0, v})
}

func Caml_get_exception_raw_backtrace(_ V) V {
	return []V{0}
}

func Caml_hash(_ ...V) V {
	panic("Primitive Caml_hash is not yet implemented")
}

func Caml_hash_univ_param(_ ...V) V {
	panic("Primitive Caml_hash_univ_param is not yet implemented")
}

func Caml_int64_of_string(_ ...V) V {
	panic("Primitive Caml_int64_of_string is not yet implemented")
}

func Caml_list_of_js_array(a_ V) V {
	a := a_.([]V)
	var l V = 0
	for i := len(a) - 1; i >= 0; i-- {
		e := a[i]
		l = []V{0, e, l}
	}
	return l
}

func Caml_md5_string(s_ V, off_ V, length_ V) V {
	s := s_.(*MlBytes)
	off := off_.(int)
	length := length_.(int)
	if s.t&6 != 0 {
		Caml_convert_string_to_bytes(s)
	}
	hasher := md5.New()
	hasher.Write([]byte(s.c[off : off+length]))
	sum := hasher.Sum(nil)
	return Caml_new_string(string(sum))
}

func Caml_ml_close_channel(_ ...V) V {
	panic("Primitive Caml_ml_close_channel is not yet implemented")
}

func Caml_ml_input(_ ...V) V {
	panic("Primitive Caml_ml_input is not yet implemented")
}

func Caml_ml_input_char(_ ...V) V {
	panic("Primitive Caml_ml_input_char is not yet implemented")
}

func Caml_ml_input_scan_line(_ ...V) V {
	panic("Primitive Caml_ml_input_scan_line is not yet implemented")
}

func Caml_ml_set_channel_name(_ ...V) V {
	panic("Primitive Caml_ml_set_channel_name is not yet implemented")
}

func Caml_mod(x_ V, y_ V) V {
	x := x_.(int)
	y := y_.(int)
	if y == 0 {
		return Caml_raise_zero_divide()
	}
	return x % y
}

func Caml_obj_set_tag(_ ...V) V {
	panic("Primitive Caml_obj_set_tag is not yet implemented")
}

func Caml_register_named_value(nm_ V, v V) V {
	nm := nm_.(*MlBytes)
	caml_named_values[Caml_jsbytes_of_string(nm).(string)] = v
	return 0
}

func Caml_int64_to_bytes(x_ V) V {
	x := x_.([]V)
	return []V{
		x[3].(int) >> 8,
		x[3].(int) & 255,
		x[2].(int) >> 16,
		x[2].(int) >> 8 & 255,
		x[2].(int) & 255,
		x[1].(int) >> 16,
		x[1].(int) >> 8 & 255,
		x[1].(int) & 255,
	}
}

func Caml_bytes_set64(s_ V, i_ V, i64 V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	if i < 0 || i+7 > Caml_ml_string_length(s).(int) {
		return Caml_string_bound_error()
	}
	a := Caml_int64_to_bytes(i64).([]V)
	for j := 0; j < 8; j++ {
		Caml_string_unsafe_set(s, i+7-j, a[j])
	}
	return 0
}

func Caml_bytes_set16(s_ V, i_ V, i16_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	i16 := uint16(i16_.(int))
	if i < 0 || i+1 > Caml_ml_string_length(s).(int) {
		return Caml_string_bound_error()
	}
	b2 := 255 & (i16 >> 8)
	b1 := 255 & i16
	Caml_string_unsafe_set(s, i+0, int(b1))
	Caml_string_unsafe_set(s, i+1, int(b2))
	return 0
}

func Caml_bytes_set32(s_ V, i_ V, i32_ V) V {
	s := s_.(*MlBytes)
	i := i_.(int)
	i32 := uint32(i32_.(int))
	if i < 0 || i+3 > Caml_ml_string_length(s).(int) {
		return Caml_string_bound_error()
	}
	b4 := 255 & (i32 >> 24)
	b3 := 255 & (i32 >> 16)
	b2 := 255 & (i32 >> 8)
	b1 := 255 & i32
	Caml_string_unsafe_set(s, i+0, int(b1))
	Caml_string_unsafe_set(s, i+1, int(b2))
	Caml_string_unsafe_set(s, i+2, int(b3))
	Caml_string_unsafe_set(s, i+3, int(b4))
	return 0
}

func Caml_bswap16(x_ V) V {
	x := x_.(int)
	return (x&255)<<8 | (x&65280)>>8
}

func Caml_int32_bswap(x_ V) V {
	x := x_.(int)
	return (x&255)<<24 |
		(x&65280)<<8 |
		Unsigned_right_shift_32((x&16711680), 8).(int) |
		Unsigned_right_shift_32((x&4278190080), 24).(int)
}

func Caml_int64_bswap(x_ V) V {
	x := x_.([]V)
	return []V{
		255,
		(x[3].(int)&65280)>>8 | (x[3].(int)&255)<<8 | x[2].(int)&16711680,
		(x[2].(int)&65280)>>8 | (x[2].(int)&255)<<8 | x[1].(int)&16711680,
		(x[1].(int)&65280)>>8 | (x[1].(int)&255)<<8,
	}
}
