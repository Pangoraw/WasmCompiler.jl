(module
        (type $A
            (struct
                (field $x (mut i32))
                (field $y (mut i32))))

    (func $make-ref (param $x i32) (param $y i32)
                    (result (ref $A))
        (struct.new 
            $A
            (local.get $x)
            (local.get $y)
        )
    )
    (export "makeRef" (func $make-ref))

    (func $get-x (param (ref $A)) (result i32)
        (struct.get $A 0 (local.get 0)))
    (export "getX" (func $get-x))

    ;; (func $add (param $a i32) (param $b i32)
    ;;            (result i32)
    ;;     (
    ;;     local.get $a
    ;;     local.get $b
    ;;     i32.add
    ;;    ))
    ;; (export "add" $add)
)