module Binary exposing (toBinary, binaryString)

toBinary : Int -> List Int
toBinary = List.reverse << binary

-- TODO: Make this tail recursive
binary : Int -> List Int
binary number =
  if number < 2 then List.singleton number
  else
    let quotient  = number // 2
        remainder = modBy 2 number
        bits      = remainder :: (binary quotient)
    in bits

binaryString : Int -> String
binaryString = List.foldr (\v a -> (String.fromInt v) ++ a) "" << toBinary

-- [0, 0, 1]