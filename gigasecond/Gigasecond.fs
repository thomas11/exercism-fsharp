module Gigasecond

open System

let gigaSecond = TimeSpan.FromSeconds(float (pown 10 9))

let add (beginDate: DateTime) = beginDate + gigaSecond
