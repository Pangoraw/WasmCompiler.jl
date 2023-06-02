(module
    ;; (type $bits (array (mut i8)))
    ;; (rec (type $any (struct (field $type (ref eq)))))

    ;; (type $A
    ;;     (struct
    ;;         (field $x (mut i32))
    ;;         (field $y (mut i32))))

    ;; (func $make-ref (param $x i32) (param $y i32)
    ;;                 (result anyref)
    ;;     (struct.new 
    ;;         $A
    ;;         (local.get $x)
    ;;         (local.get $y)
    ;;     )
    ;; )
    ;; (export "makeRef" (func $make-ref))

    ;; (func $add (param $a i32) (param $b i32)
    ;;            (result i32)
    ;;     (
    ;;     local.get $a
    ;;     local.get $b
    ;;     i32.add
    ;;    ))
    ;; (export "add" $add)

    (func $pow (param $x i32) (param $n i32)
               (result i32)
               (local $s i32)
        i32.const 1
        local.set $s
        
        loop 
            local.get $n
            i32.eqz

            if
                nop
            else
                local.get $s
                local.get $x
                i32.mul
                local.set $s

                local.get $n
                i32.const -1
                i32.add
                local.set $n

                br 1
            end
        end

        local.get $s
    )

    (export "pow" (func $pow))

    (func (param i64) (param i64) (result i64) 
    (local i64)
    (local i64)
    (local i64)
    (local i32)
    (local i64)
    (local i64)
    (local i64)
        block 
            i64.const 0
            local.get 2
            i64.lt_s
            local.set 3
            loop 
                if 
                    local.get 2
                    i64.const 1
                    i64.sub
                    local.set 4
                    local.get 5
                    i64.const 1
                    i64.add
                    local.set 6
                    br 0
                else
                    local.get 5
                    return
                end
            end
        end
    )
)