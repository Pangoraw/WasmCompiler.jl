(module

    ;; ==== bootstrap.wat ====
    ;;
    ;; This file contains an attempt to implement enough of the julia runtime using the 
    ;; wasm GC proposal. This maybe too much work but the goal is to evaluate the difficulty along the
    ;; way. First of all, functions of the runtime which are written in C will have to get converted
    ;; to the GC proposal, this means extracting the IR somehow and applying the struct instructions for
    ;; field access somehow. Or we can rewrite the runtime in Julia and use the WasmCompiler to export
    ;; new WebAssembly for those functions. After poking at this problem, it seems that there are tradedoffs
    ;; between strictness or reimplementing exactly the runtime. For example, Julia can bootstrap itself
    ;; (resolve the circular dependency between DataType and TypeName) using nulls, we may want to avoid
    ;; null refs on our fields for wasm?
    ;;

    ;; IO Imports

    (import "console" "log" (func $log (param (ref string))))
    (import "console" "newline" (func $newline))

    ;; GC Type definitions

    (rec
        (type $jl-value-t
            (struct (field $jl-value-type (ref null $jl-datatype-t))))

        (type $jl-datatype-t
            (sub $jl-value-t
                (struct
                    (field $jl-value-type (ref null $jl-datatype-t))
                    (field $name (mut (ref null $jl-typename-t)))
                    (field $super (mut (ref null $jl-datatype-t)))
                    (field $parameters (mut (ref null $jl-simplevector-t)))
                    (field $types (ref null $jl-simplevector-t))
                    (field $instance (mut (ref null $jl-value-t))) ;; for singleton types
                    (field $layout i32)
                    (field $hash i32)
                    (field $flags i32))))

        (type $jl-typename-t
            (sub $jl-value-t
                (struct
                    (field $jl-value-type (ref null $jl-datatype-t))
                    (field $name (ref $jl-symbol-t))
                    (field $flags i32))))

        (type $jl-string-t
            (sub $jl-value-t
                (struct
                    (field $jl-value-type (ref null $jl-datatype-t))
                    (field $str (ref string)))))

        (type $jl-values-t (array (mut (ref null $jl-value-t))))
        (type $jl-simplevector-t
            (sub $jl-value-t
                (struct
                    (field $jl-value-type (ref null $jl-datatype-t))
                    (field $values (ref $jl-values-t)))))

        (type $jl-symbol-t
            (sub $jl-value-t
                (struct
                    (field $jl-value-type (ref null $jl-datatype-t))
                    (field $hash i32)
                    (field $str (ref $jl-string-t)))))
    )

    ;; Primitive types

    (type $jl-nothing-t
        (sub $jl-value-t
            (struct
                (field $jl-value-type (ref $jl-datatype-t)))))

    (global $jl-number-type  (export "jl_number_type")  (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-real-type    (export "jl_real_type")    (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-integer-type (export "jl_integer_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-signed-type  (export "jl_signed_type")  (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))

    (type $jl-int32-t
        (sub $jl-value-t
             (struct
                (field $jl-value-type (ref $jl-datatype-t))
                (field $val i32))))
    (global $jl-int32-type (export "jl_int32_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))

    (type $jl-int64-t
        (sub $jl-value-t
             (struct
                (field $jl-value-type (ref $jl-datatype-t))
                (field $val i64))))
    (global $jl-int64-type (export "jl_int64_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))

    (type $jl-float32-t
        (sub $jl-value-t
             (struct
                (field $jl-value-type (ref $jl-datatype-t))
                (field $val f32))))
    (global $jl-float32-type (export "jl_float32_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))

    (type $jl-float64-t
        (sub $jl-value-t
             (struct
                (field $jl-value-type (ref $jl-datatype-t))
                (field $val f64))))
    (global $jl-float64-type (export "jl_float64_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))

    (func $jl-box-int32 (export "jl_box_int32") (param i32) (result (ref $jl-int32-t))
        (struct.new $jl-int32-t
            (ref.as_non_null (global.get $jl-int32-type))
            (local.get 0)))
    (func $jl-unbox-int32 (export "jl_unbox_int32") (param (ref $jl-value-t)) (result i32)
        (struct.get $jl-int32-t $val (ref.cast $jl-int32-t (local.get 0))))
    (func $jl-box-int64 (export "jl_box_int64") (param i64) (result (ref $jl-int64-t))
        (struct.new $jl-int64-t
            (ref.as_non_null (global.get $jl-int64-type))
            (local.get 0)))
    (func $jl-unbox-int64 (export "jl_unbox_int64") (param (ref $jl-int64-t)) (result i64)
        (struct.get $jl-int64-t $val (ref.cast $jl-int64-t (local.get 0))))
    (func $jl-box-float32 (export "jl_box_float32") (param f32) (result (ref $jl-float32-t))
        (struct.new $jl-float32-t
            (ref.as_non_null (global.get $jl-float32-type))
            (local.get 0)))
    (func $jl-unbox-float32 (export "jl_unbox_float32") (param (ref $jl-float32-t)) (result f32)
        (struct.get $jl-float32-t $val (ref.cast $jl-float32-t (local.get 0))))
    (func $jl-box-float64 (export "jl_box_float64") (param f64) (result (ref $jl-float64-t))
        (struct.new $jl-float64-t
            (ref.as_non_null (global.get $jl-float64-type))
            (local.get 0)))
    (func $jl-unbox-float64 (export "jl_unbox_float64") (param (ref $jl-float64-t)) (result f64)
        (struct.get $jl-float64-t $val (ref.cast $jl-float64-t (local.get 0))))

    (type $jl-nonnull-values-t (array (ref $jl-value-t)))
    (type $jl-tuple-t
        (sub $jl-value-t
            (struct
                (field $jl-value-type (ref $jl-datatype-t))
                (field $values (ref $jl-nonnull-values-t)))))

    (global $jl-datatype-type (export "jl_datatype_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-type-type (export "jl_type_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-typename-type (export "jl_typename_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-symbol-type (export "jl_symbol_type") (mut (ref null $jl-datatype-t))   (ref.null $jl-datatype-t))
    (global $jl-simplevector-type (export "jl_simplevector_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-string-type (export "jl_string_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-any-type (export "jl_any_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))

    (global $jl-nothing-type (export "jl_nothing_type") (mut (ref null $jl-datatype-t)) (ref.null $jl-datatype-t))
    (global $jl-nothing (export "jl_nothing") (mut (ref null $jl-nothing-t)) (ref.null $jl-nothing-t))

    ;; The exception thrown is stored in the global and the tag is simple $jl-exception-tag
    (tag $jl-exception-tag (export "jl_exception_tag"))
    (global $jl-exception (export "jl_exception") (mut (ref null $jl-value-t)) (ref.null $jl-value-t))

    ;; Bootstraping requires that build Core.DataType since it is the value of $jl-value-type
    ;; but it requires Core.TypeName which in turns requires Symbol and svec among other things.
    ;; We use the start function $init to do that bootstrapping on the globals.

    (func $init
        (global.set $jl-datatype-type
            (struct.new $jl-datatype-t 
                (ref.null $jl-datatype-t)
                (ref.null $jl-typename-t)
                (ref.null $jl-datatype-t)
                (ref.null $jl-simplevector-t)
                (ref.null $jl-simplevector-t)
                (ref.null $jl-value-t)
                (i32.const 0)
                (i32.const 402395367)
                (i32.const 0x72)))

        (global.set $jl-typename-type
            (struct.new $jl-datatype-t 
                (global.get $jl-datatype-type)
                (ref.null $jl-typename-t)
                (ref.null $jl-datatype-t)
                (ref.null $jl-simplevector-t)
                (ref.null $jl-simplevector-t)
                (ref.null $jl-value-t)
                (i32.const 0)
                (i32.const -174284947)
                (i32.const 0x72)))

        (global.set $jl-symbol-type
            (struct.new $jl-datatype-t 
                (global.get $jl-datatype-type)
                (ref.null $jl-typename-t)
                (ref.null $jl-datatype-t)
                (ref.null $jl-simplevector-t)
                (ref.null $jl-simplevector-t)
                (ref.null $jl-value-t)
                (i32.const 0)
                (i32.const 1004224578)
                (i32.const 0x62)))

        (global.set $jl-any-type
            (call $jl-new-abstracttype
                (call $jl-symbol (string.const "Any"))
                (i32.const 507456893)
                (i32.const 0x60)))

        (struct.set $jl-datatype-t $super
            (global.get $jl-typename-type)
            (global.get $jl-any-type))

        (struct.set $jl-datatype-t $super
            (global.get $jl-symbol-type)
            (global.get $jl-any-type))

        (struct.set $jl-datatype-t $super
            (global.get $jl-any-type)
            (global.get $jl-any-type))

        (struct.set $jl-datatype-t $name
            (global.get $jl-datatype-type)
            (struct.new $jl-typename-t
                (global.get $jl-typename-type)
                (call $jl-symbol (string.const "DataType"))
                (i32.const 0x02)))

        (struct.set $jl-datatype-t $name
            (global.get $jl-typename-type)
            (struct.new $jl-typename-t
                (global.get $jl-typename-type)
                (call $jl-symbol (string.const "TypeName"))
                (i32.const 0x72)))

        (struct.set $jl-datatype-t $name
            (global.get $jl-symbol-type)
            (struct.new $jl-typename-t
                (global.get $jl-typename-type)
                (call $jl-symbol (string.const "Symbol"))
                (i32.const 0x02)))

        (global.set $jl-simplevector-type
            (call $jl-new-concretetype
                (call $jl-symbol (string.const "SimpleVector"))
                (i32.const -72802639)
                (i32.const 0x62)))

        (global.set $jl-type-type
            (call $jl-new-abstracttype
                (call $jl-symbol (string.const "Type"))
                (i32.const 2039908602)
                (i32.const 0x21)))

        (struct.set $jl-datatype-t $parameters
            (global.get $jl-type-type)
            (call $jl-svec1
                (call $jl-symbol (string.const "T"))))

        (struct.set $jl-datatype-t $parameters
            (global.get $jl-datatype-type)
            (call $jl-svec0))

        (struct.set $jl-datatype-t $parameters
            (global.get $jl-typename-type)
            (call $jl-svec0))

        (struct.set $jl-datatype-t $parameters
            (global.get $jl-symbol-type)
            (call $jl-svec0))

        (struct.set $jl-datatype-t $super
            (global.get $jl-datatype-type)
            (global.get $jl-type-type))

        (global.set $jl-string-type
            (call $jl-new-concretetype
                (call $jl-symbol (string.const "String"))
                (i32.const -308267535)
                (i32.const 0x62)))

        (global.set $jl-nothing-type
            (call $jl-new-primitivetype
                (call $jl-symbol (string.const "Nothing"))
                (i32.const -244432227)
                (i32.const 0x6a)))
        (global.set $jl-nothing
            (struct.new $jl-nothing-t
                (ref.as_non_null (global.get $jl-nothing-type))))
        (struct.set $jl-datatype-t $instance
            (global.get $jl-nothing-type)
            (global.get $jl-nothing))

        (global.set $jl-number-type
            (call $jl-new-abstracttype
                  (call $jl-symbol (string.const "Number"))
                  (i32.const 32)
                  (i32.const 0x60)))
        (global.set $jl-real-type
            (call $jl-new-abstracttype
                  (call $jl-symbol (string.const "Real"))
                  (i32.const 32)
                  (i32.const 0x60)))
        (global.set $jl-integer-type
            (call $jl-new-abstracttype
                  (call $jl-symbol (string.const "Integer"))
                  (i32.const 32)
                  (i32.const 0x60)))
        (global.set $jl-signed-type
            (call $jl-new-abstracttype
                  (call $jl-symbol (string.const "Signed"))
                  (i32.const 32)
                  (i32.const 0x60)))

        ;; (Int64, (Signed, (Integer, (Real, (Number, Any)))))
        (struct.set $jl-datatype-t $super
            (global.get $jl-real-type)
            (global.get $jl-number-type))
        (struct.set $jl-datatype-t $super
            (global.get $jl-integer-type)
            (global.get $jl-real-type))
        (struct.set $jl-datatype-t $super
            (global.get $jl-signed-type)
            (global.get $jl-integer-type))

        (global.set $jl-int32-type
            (call $jl-new-primitivetype
                (call $jl-symbol (string.const "Int32"))
                (i32.const -536228166)
                (i32.const 0xea)))
        (struct.set $jl-datatype-t $super
            (global.get $jl-int32-type)
            (global.get $jl-signed-type))

        (global.set $jl-int64-type
            (call $jl-new-primitivetype
                (call $jl-symbol (string.const "Int64"))
                (i32.const 121901828)
                (i32.const 0xea)))
        (struct.set $jl-datatype-t $super
            (global.get $jl-int64-type)
            (global.get $jl-signed-type))

        (global.set $jl-float32-type
            (call $jl-new-primitivetype
                (call $jl-symbol (string.const "Float32"))
                (i32.const 1799353626)
                (i32.const 0xea)))

        (global.set $jl-float64-type
            (call $jl-new-primitivetype
                (call $jl-symbol (string.const "Float64"))
                (i32.const 1462090786)
                (i32.const 0xea)))
    )

    (start $init)

    (func $jl-new-concretetype
          (param $name (ref $jl-symbol-t))
          (param $hash i32)
          (param $flags i32)
          (result (ref $jl-datatype-t))
          (call $jl-new-datatype
                (local.get 0)
                (local.get 1)
                (local.get 2)
                (i32.const 0x02)))
    (func $jl-new-primitivetype
          (param $name (ref $jl-symbol-t))
          (param $hash i32)
          (param $flags i32)
          (result (ref $jl-datatype-t))
          (call $jl-new-datatype
                (local.get 0)
                (local.get 1)
                (local.get 2)
                (i32.const 0x04)))
    (func $jl-new-abstracttype
          (param $name (ref $jl-symbol-t))
          (param $hash i32)
          (param $flags i32)
          (result (ref $jl-datatype-t))
          (call $jl-new-datatype
                (local.get 0)
                (local.get 1)
                (local.get 2)
                (i32.const 0x01)))
    (func $jl-new-datatype
            (param $name (ref $jl-symbol-t))
            (param $hash i32)
            (param $flags i32)
            (param $typename-flags i32)
            (result (ref $jl-datatype-t))
        (struct.new $jl-datatype-t
            (global.get $jl-datatype-type)
            (struct.new $jl-typename-t
                (global.get $jl-typename-type)
                (local.get $name)
                (local.get $typename-flags))
            (ref.null $jl-datatype-t)
            (struct.new $jl-simplevector-t
                (global.get $jl-simplevector-type)
                (array.new $jl-values-t (ref.null $jl-value-t) (i32.const 0)))
            (struct.new $jl-simplevector-t
                (global.get $jl-simplevector-type)
                (array.new $jl-values-t (ref.null $jl-value-t) (i32.const 0)))
            (ref.null $jl-value-t)
            (i32.const 0)
            (local.get $hash)
            (local.get $flags))
    )

    (func $jl-symbol (param $str (ref string)) (result (ref $jl-symbol-t))
        (struct.new $jl-symbol-t
            (global.get $jl-symbol-type)
            (i32.const 0)
            (struct.new $jl-string-t
                (global.get $jl-string-type)
                (local.get $str))))
    (export "jl_symbol" (func $jl-symbol))

    (func $jl-repr-type (param (ref null $jl-value-t))
        (local $pc i32) (local $type (ref $jl-datatype-t))
        (local $len i32)
        (local.set $type (ref.cast $jl-datatype-t (local.get 0)))
        (call $log
            (struct.get $jl-string-t $str ;; ref string
                (struct.get $jl-symbol-t $str ;; jl-string-t
                    (struct.get $jl-typename-t $name ;; jl-symbol-t
                        (struct.get $jl-datatype-t $name ;; jl-typename-t
                            (local.get $type))))))
        (if
            (local.tee $len
                (call $jl-sveclen (struct.get $jl-datatype-t $parameters (local.get $type))))
            (then
                (call $log (string.const "{"))))
        (loop
            (if (i32.lt_s (local.get $pc) (local.get $len))
                (then
                    (call $jl-repr
                        (ref.as_non_null
                            (array.get $jl-values-t
                                (struct.get $jl-simplevector-t $values
                                    (struct.get $jl-datatype-t $parameters (local.get $type)))
                                (local.get $pc))))
                    (local.set $pc (i32.add (local.get $pc) (i32.const 1)))
                    (if (i32.lt_s (local.get $pc) (local.get $len))
                        (then (call $log (string.const ","))))
                    (br 1))))
        (if (local.get $len)
            (then (call $log (string.const "}"))))
        (call $newline))

    ;; An example of single dispatch using Wasm GC casts.
    (func $jl-repr (param (ref $jl-value-t))
        (block 
            (block $i32
                (block $nothing
                    (block $sym
                        (block $datatype
                            (br_if $sym (ref.test $jl-symbol-t (local.get 0)))
                            (br_if $datatype (ref.test $jl-datatype-t (local.get 0)))
                            (br_if $nothing (ref.test $jl-nothing-t (local.get 0)))
                            (br_if $i32 (ref.test $jl-int32-t (local.get 0)))
                            (unreachable))
                        (call $jl-repr-type (ref.cast $jl-datatype-t (local.get 0)))
                        (return))
                    (call $log
                        (struct.get $jl-string-t $str ;; ref string
                            (struct.get $jl-symbol-t $str ;; jl-string-t
                                (ref.cast $jl-symbol-t (local.get 0)))))
                    (return))
                (call $log (string.const "nothing"))
                (return))
            (call $log (string.const "42"))
            (return)))

    ;; Builtins

    (export "jl_repr" (func $jl-repr))

    (func $jl-typeof (export "jl_typeof") (param (ref $jl-value-t)) (result (ref $jl-datatype-t))
        (ref.as_non_null (struct.get $jl-value-t $jl-value-type
            (local.get 0))))


    (func $is-mutable (param $x (ref $jl-value-t)) (result i32)
          (call $is-mutable-type
                (call $jl-typeof (local.get $x))))
    (func $is-mutable-type (param $x (ref $jl-datatype-t)) (result i32)
          (i32.eq
                (i32.const 2)
                (i32.and
                       (i32.const 2)
                       (struct.get $jl-datatype-t $flags ;; i32
                           (local.get $x)))))

    (func (export "jl_typename_flags") (param (ref $jl-datatype-t)) (result i32)
        (struct.get $jl-typename-t $flags ;; i32
            (struct.get $jl-datatype-t $name ;; $jl-typename-t
                (local.get 0))))
    (func $is-abstract-type (export "jl_isabstracttype") (param $x (ref $jl-datatype-t)) (result i32)
          (i32.eq
                (i32.const 1)
                (i32.and
                    (i32.const 1)
                    (struct.get $jl-typename-t $flags ;; i32
                        (struct.get $jl-datatype-t $name ;; $jl-typename-t
                            (local.get $x))))))

    (func $is-in-parent (param $tx (ref $jl-datatype-t)) (param $t (ref $jl-datatype-t)) (result i32)
          (if (ref.eq (local.get $tx) (local.get $t))
              (then (return (i32.const 1))))
          (if (ref.eq (ref.as_non_null (global.get $jl-any-type)) (local.get $t))
              (then (return (i32.const 1))))
          (if (ref.eq (ref.as_non_null (global.get $jl-any-type)) (local.get $tx))
              (then (return (i32.const 0))))
          (call $is-in-parent
                (ref.as_non_null (struct.get $jl-datatype-t $super (local.get $tx)))
                (local.get $t)))
 
    (func $jl-isa (export "jl_isa") (param $x (ref $jl-value-t)) (param $t (ref $jl-datatype-t)) (result i32)
          (if (call $is-abstract-type (local.get $t))
              (then
                    (return
                        (call $is-in-parent
                              (call $jl-typeof
                                    (local.get $x))
                              (local.get $t)))))
          (ref.eq
              (call $jl-typeof (local.get $x))
              (local.get $t)))

    ;; wast equivalent of then `Base.:(===)` builtin.
    (func $jl-egal (export "jl_egal") (param $a (ref $jl-value-t)) (param $b (ref $jl-value-t)) (result i32)
          (local $ta (ref $jl-datatype-t)) (local $tb (ref $jl-datatype-t))
          (local.set $ta (call $jl-typeof (local.get $a)))
          (local.set $tb (call $jl-typeof (local.get $b)))
          (if (i32.eqz (ref.eq (local.get $ta) (local.get $tb))) ;; types not equal
              (then (return (i32.const 0))))
          (if (call $is-mutable-type (local.get $ta))
              (then (return (ref.eq (local.get $a) (local.get $b)))))
          (if (ref.eq (local.get $ta) (global.get $jl-int32-type))
              (then
                  (return (i32.eq
                          (call $jl-unbox-int32 (ref.cast $jl-int32-t (local.get $a)))
                          (call $jl-unbox-int32 (ref.cast $jl-int32-t (local.get $b)))))))
          (if (ref.eq (local.get $ta) (global.get $jl-int64-type))
              (then
                  (return (i64.eq
                      (call $jl-unbox-int64 (ref.cast $jl-int64-t (local.get $a)))
                      (call $jl-unbox-int64 (ref.cast $jl-int64-t (local.get $b)))))))
          (i32.const 0))

    ;; wast equivalent of `Base.:(<:)` builtin.
    (func $jl-subtype (export "jl_subtype") (param $a (ref $jl-datatype-t)) (param $b (ref $jl-datatype-t)) (result i32)
        (call $is-in-parent
            (local.get $a)
            (local.get $b)))

    (func $jl-sveclen (param (ref null $jl-simplevector-t)) (result i32)
        (array.len (struct.get $jl-simplevector-t $values (local.get 0))))

    (func $jl-svec0 (result (ref $jl-simplevector-t))
        (struct.new $jl-simplevector-t
            (global.get $jl-simplevector-type)
            (array.new $jl-values-t (ref.null $jl-value-t) (i32.const 0))))
    (func $jl-svec1 (param (ref $jl-value-t)) (result (ref $jl-simplevector-t))
        (struct.new $jl-simplevector-t
            (global.get $jl-simplevector-type)
            (array.new $jl-values-t (local.get 0) (i32.const 1))))
    (func $jl-svec2 (param (ref $jl-value-t) (ref $jl-value-t)) (result (ref $jl-simplevector-t))
        (local $values (ref $jl-values-t))
        (local.set $values
            (array.new $jl-values-t
                (ref.null $jl-value-t)
                (i32.const 2)))
        (array.set $jl-values-t
            (local.get $values) (i32.const 0) (local.get 0))
        (array.set $jl-values-t
            (local.get $values) (i32.const 1) (local.get 1))
        (struct.new $jl-simplevector-t
            (global.get $jl-simplevector-type)
            (local.get $values)))
)
