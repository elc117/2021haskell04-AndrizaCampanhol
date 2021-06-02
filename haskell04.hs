-- Prática 04 de Haskell
-- Nome: Andriza Campanhol

--01
faixaIdoso :: Int -> String
faixaIdoso idade
  | idade >= 60 && idade <= 64 = "IDO64"
  | idade >= 65 && idade <= 69 = "IDO69"
  | idade >= 70 && idade <= 74 = "IDO74"
  | idade >= 75 && idade <= 79 = "IDO79"
  | idade >= 80 = "IDO80"
  | otherwise = "ND"

--02
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos tupla = [(str, idade, faixaIdoso idade) | (str, idade) <- tupla]
--outra forma:
--classifIdosos tuplalist = [(fst tupla, snd tupla, faixaIdoso (snd tupla)) | tupla <- tuplalist]

--03
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' tuplalist = map (\(str, idade) -> (str, idade, faixaIdoso idade)) tuplalist

--04
strColor :: (Int,Int,Int) -> String
--show -> int p/ string
strColor (red, green, blue) = "rgb(" ++ show red ++ "," ++ show green ++ "," ++ show blue ++ ")"

--05
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = [(x, cy, r) | x <- [cx, cx+10..cx+10*(n-1)]]

--06
genReds :: Int -> [(Int,Int,Int)]
genReds n = [(x, 0, 0) | x <- [80, 90..if 80+10*n > 255 then 255 else 80+10*n]]