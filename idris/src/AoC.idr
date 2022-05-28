module AoC

infixl 5 |>
export
(|>) : a -> (a -> b) -> b
x |> f = f x
