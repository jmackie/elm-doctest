module Lib exposing (addOne, alwaysFive)

{-| A silly module to illustrate and test elm-doctest

@docs addOne, alwaysFive

-}


{-| Adds one. It's pretty useful.

Here's how you might use it...

    > addOne 1
    2 : Int

Woah. I know right. Here's some more...

    > addOne 20
    21 : Int

    > (addOne >> addOne) 20
    22 : Int

It works with big numbers too!

    > addOne 10000000
    10000001 : Int

-}
addOne : Int -> Int
addOne n =
    n + 1


{-| Gives you five.

Inspired by <https://github.com/jackdclark/five>

At the time of writing we get an error if this expression isn't enclosed in
parentheses, I'm pretty sure it's a bug in the compiler.

    > (alwaysFive "foo" == 5)
    True : Bool

-}
alwaysFive : a -> Int
alwaysFive _ =
    5
