module Raindrops

let convert (number: int): string =
    let factors =
        [ (3, "Pling")
          (5, "Plang")
          (7, "Plong") ]

    let sounds =
        factors
        |> Seq.choose (fun (factor, sound) ->
            if number % factor = 0 then Some(sound) else None)

    if Seq.isEmpty sounds then string number else String.concat "" sounds
