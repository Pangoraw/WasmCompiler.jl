
"""
```
 1
 ↓\\
 2 3
 ↓/↑
 4 |
 ↓\\|
 6 5
```
"""
function ir_cfg(cond1, cond2)
    s = 0

    cond1 && @goto loop2
@label loop1

    s += 1

@label loop2

    s *= 2

    cond2 && @goto loop1

    return s
end
