module Main where

import Lib
import Data.List
import Debug.Trace

main :: IO ()
main = putStrLn "jedu"

data Barva = Cer | Fia | Ruz | Ora | Zlu | Mod | Azu | Zel | Oli | Hne | Sed | Bez
   deriving (Eq, Show, Ord)

type Zkum = [Barva]

data Stav = Stav [Zkum] [(Int, Int, Barva)]
   deriving (Show)

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = [] 
deleteFirst a (b:bc) | a == b    = bc 
                     | otherwise = b : deleteFirst a bc

replacee :: a -> Int -> [a] -> [a]
replacee e i xs  = before ++ [e] ++ after
  where
    (before, _ : after) = splitAt i xs

isPlna :: Zkum -> Bool
isPlna zk = length zk >= 4

isPrazdna :: Zkum -> Bool
isPrazdna [] = True
isPrazdna _ = False

isVicebarevna :: Zkum -> Bool
isVicebarevna [] = False
isVicebarevna [_] = False
isVicebarevna (x : y : s) = x /= y || isVicebarevna(y : s)

isJednobarevna :: Zkum -> Bool
isJednobarevna = not . isVicebarevna

delkaNejvyssiBarvy :: Zkum -> Int
delkaNejvyssiBarvy [] = 0
delkaNejvyssiBarvy (b : s) = length (takeWhile (==b) s) + 1

lzePrelit :: Zkum -> Zkum -> Bool
lzePrelit [] _ = False
lzePrelit zk [] = isVicebarevna zk
lzePrelit (b1 : _) (b2 : _)
  | b1 /= b2 = False
lzePrelit zk1 zk2
  | delkaNejvyssiBarvy zk1 + length zk2 > 4 = False
  | isJednobarevna zk1 && isJednobarevna zk2 && length zk1 > length zk2 = False

lzePrelit _ _ = True 

prelij :: Zkum -> Zkum -> (Zkum, Zkum)
prelij zk1@(b : _) zk2 =
    let (prelivanec, zbytek) = span (==b) zk1
    in (zbytek, prelivanec ++ zk2)

--    [   Stav (nova1 : nova2 : (deleteFirst zk1 . deleteFirst zk2 $ s))  (b: bs) |


pokrok :: Stav -> [Stav]
pokrok (Stav s bs) =
    [   Stav (replacee nova1 i1 . replacee nova2 i2 $ s) ((i1+1, i2+1, b) : bs) |
        let sPoradim = zip s [0..],
        (zk1, i1) <- sPoradim, 
        (zk2, i2) <- sPoradim, 
        zk1 /= zk2 || i1 < i2,
        lzePrelit zk1 zk2,
        let (nova1, nova2@(b : _)) = prelij zk1 zk2
    ]

isVyreseno :: Stav -> Bool
isVyreseno (Stav s _) = all (\x -> not (isVicebarevna x) && (length x == 4 || length x == 0))  s

testPokrok = pokrok (Stav [ [Cer, Zel], [Cer, Mod], [Zlu, Sed], [Zlu, Zlu, Zlu] ] [])
testPokrok2 = pokrok (Stav [ [Cer, Zel], [Cer, Zel], [Zlu] ] [])

reseni :: Stav -> [[(Int, Int, Barva)]]
reseni stav = do
    s@(Stav _ vysl) <- pokrok stav
    if isVyreseno s then return vysl
                    else reseni s


reseniNejmensi :: [Stav] -> [[(Int, Int, Barva)]]
reseniNejmensi stav =
    let novestavy = trace ("Iterace " ++ show (length stav)) stav >>= pokrok
        vysledky = map (\(Stav _ b) -> b) . filter isVyreseno $ novestavy
    in  if null vysledky then reseniNejmensi novestavy
                         else vysledky



res  x =  head $ reseni (Stav x [])

resNejmensi x = reseniNejmensi [(Stav x [])]


kontrolaZadani :: [[Barva]] -> Bool
kontrolaZadani zadani =
    let barvy = sort . concat $ zadani
    in length barvy `mod` 4 == 0 
       && length barvy < length zadani * 4
       && jsouCtverice barvy
  where 
    jsouCtverice [] = True
    jsouCtverice (a : b : c : d : rest) =
        all (==a) [b,c,d] && jsouCtverice rest


zadani1 = [[Cer, Zel, Cer, Zel], [Zel, Zel], [Cer, Cer]]

zadani2 = [[Cer, Zel, Zlu, Zlu], [Zel, Zel, Cer, Cer], [Zlu, Zlu, Zel, Cer], []]

zadani3 = [
      [Zlu, Sed, Fia, Zel],
      [Mod, Zlu, Ruz, Fia],
      [Ora, Sed, Ruz, Fia],
      [Cer, Zel, Cer, Mod],
      [Azu, Ora, Hne, Bez],
      [Hne, Mod, Cer, Bez],
      [Zel, Cer, Sed, Mod],
      [Fia, Zlu, Oli, Azu],
      [Oli, Bez, Ruz, Oli],
      [Sed, Hne, Ora, Oli],
      [Bez, Ora, Azu, Ruz],
      [Azu, Zlu, Zel, Hne],
      [],
      []
   ]