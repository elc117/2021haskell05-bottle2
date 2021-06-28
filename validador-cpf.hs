import Data.Char

cpfValid :: [Int] -> Bool
cpfValid cpf =
  let digits = take 9 cpf
      dv1 = cpfDV digits [10,9..]
      dv2 = cpfDV (digits ++ [dv1]) [11,10..]
  in dv1 == cpf !! 9 && dv2 == cpf !! 10

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = if expr < 2 then 0 else 11 - expr
  where expr = (sum $ zipWith (*) digits mults) `mod` 11

main :: IO()
main = do
  putStr "CPF: "
  cpf <- getLine
  let digits = (map digitToInt cpf)
  putStrLn (if cpfValid digits then "OK" else "Not OK")
