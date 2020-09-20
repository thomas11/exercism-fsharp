module Clock

type Clock = int * int

let create hours minutes: Clock =
    let minutes' = minutes % 60

    let negMinutesRollover =
        if minutes' < 0 then -1 else 0
    let minutes' =
        if minutes' < 0 then minutes' + 60 else minutes'

    let hoursInMinutes = minutes / 60
    let hours' = (hours + hoursInMinutes + negMinutesRollover) % 24

    let hours' =
        if hours' < 0 then hours' + 24 else hours'

    (hours', minutes')

let add minutes clock: Clock = create (fst clock) ((snd clock) + minutes)

let subtract minutes clock = add -minutes clock

let display (clock: Clock) =
    let h, m = clock
    sprintf "%02i:%02i" h m
