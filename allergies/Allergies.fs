module Allergies

open System

[<FlagsAttribute>]
type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let allergicTo codedAllergies allergen =
    let intAllergen = int allergen
    intAllergen &&& codedAllergies = intAllergen

let list codedAllergies =
    Enum.GetValues(typeof<Allergen>) :?> Allergen []
    |> Array.filter (fun a -> allergicTo codedAllergies a)
    |> Array.toList
