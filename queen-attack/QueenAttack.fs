module QueenAttack

let create ((a, b): int * int): bool =
    a >= 0 && a <= 7 && b >= 0 && b <= 7

let canAttack ((q1a, q1b): int * int) ((q2a, q2b): int * int): bool =
    q1a = q2a || q1b = q2b || abs(q1a - q2a) = abs(q1b - q2b)