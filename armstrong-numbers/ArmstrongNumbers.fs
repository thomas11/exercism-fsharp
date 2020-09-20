module ArmstrongNumbers

let calcArmstrong number =
    let numStr = sprintf "%i" number
    let numDigits = String.length numStr
    let raise digit = pown digit numDigits
    Seq.sumBy
        (string
         >> int
         >> raise) numStr

let isArmstrongNumber (number: int): bool = number = calcArmstrong number
