(module

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
                    (field $name (ref $jl-symbol-t)))))

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

    (type $jl-int32-t
        (sub $jl-value-t
             (struct
                (field $jl-value-type (ref $jl-datatype-t))
                (field $val i32))))

    (import "bootstrap" "jl_box_int32" (func $jl-box-int32 (param i32) (result (ref $jl-int32-t))))
    (import "bootstrap" "jl_repr" (func $jl-repr (param (ref $jl-value-t))))

    (import "bootstrap" "jl_datatype_type" (global $jl-datatype-type (ref $jl-datatype-t)))

    (func $init
        (if (ref.is_null (global.get $jl-datatype-type))
            (then (unreachable)))
        (call $jl-repr
            (call $jl-box-int32 (i32.const 42))))

    (start $init)
)