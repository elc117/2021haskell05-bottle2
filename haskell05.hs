import Data.Char

bmi :: Float -> Float -> String
bmi peso altura =
  let imc = peso / (altura * altura)
  in if imc <= 18.5 then "ABAIXO" else if imc >= 30 then "ACIMA" else "NORMAL"

bmi' :: Float -> Float -> String
bmi' peso altura
  | imc <= 18.5 = "ABAIXO"
  | imc >= 30   = "ACIMA"
  | otherwise   = "NORMAL"
  where imc = peso / (altura * altura)

{-
cpfValid :: [Int] -> Bool
cpfValid cpf =
  let digits = take 9 cpf
      dv1 = cpfDV digits [10,9..]
      dv2 = cpfDV (digits ++ [dv1]) [11,10..]
  in dv1 == cpf !! 9 && dv2 == cpf !! 10

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = if expr < 2 then 0 else 11 - expr
  where expr = (sum $ zipWith (*) digits mults) `mod` 11
-}

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11 - expr

main :: IO()
main = do
  putStr "CPF: "
  cpf <- getLine
  let digits = (map digitToInt cpf)
  putStrLn (if cpfValid digits then "OK" else "Not OK")

andTable :: [(Bool, Bool, Bool)]
andTable = zip3 p q (zipWith (\p_i q_i -> p_i && q_i) p q)
  where p = take 4 (table !! 1)
        q = take 4 (table !! 0)
        table = map booleans [1..]
        booleans repeating = cycle $ concat $ map (replicate repeating) [True, False]
