{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use unwords" #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Não é possível retornar primeiro elemento pois a lista fornecida é vazia."
head (x : _) = x

tail :: [a] -> [a]
tail [] = error "Não é possível retornar sublista pois a lista fornecida é vazia."
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null (_ : _) = False

length :: Integral i => [a] -> i
length [] = 0
length (_ : xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = sum xs + x

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = product xs * x

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y : ys) = y : snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
minimum [] = error "Não é possível retornar o valor mínimo de uma lista vazia"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

-- maximum :: Ord a => [a] -> a
maximum :: Ord a => [a] -> a
maximum [] = error "Não é possível retornar o valor máximo de uma lista vazia"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

-- take
take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x : xs) = x : take (n-1) xs

-- drop
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_ : xs) = drop (n-1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x : xs) =
  if f x
  then x : takeWhile f xs
  else []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x : xs) =
  if f x
  then dropWhile f xs
  else x : xs

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- init
init :: [a] -> [a]
init [] = error "Não é possível utilizar o init em uma lista vazia"
init (x : xs) =
  if null xs
  then []
  else x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = xs : inits (init xs)

-- subsequences

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) =
  f x || any f xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x : xs) =
  f x && all f xs

-- and
and :: [Bool] -> Bool
and [] = True
and (x : xs) =
  x && and xs

-- or
or :: [Bool] -> Bool
or [] = False
or (x : xs) =
  x || or xs

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool -- For any type that can use == for comparison
elem x = any (== x)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) =
  x == y || elem' x ys

-- (!!)
(!!) :: [a] -> Int -> a
[] !! _ = error "Não foi possível encontrar elemento na posição especificada"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) =
  if f x
  then x : filter f xs
  else filter f xs

-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = error "Não é possível realizar este processo com uma lista vazia"
cycle xs = xs ++ cycle xs

-- repeat
repeat :: a -> [a]
repeat x = x : repeat x

-- replicate
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x
-- Do i need to implement `replicate (-1) x`?

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf []  _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = (x == y) && isPrefixOf xs ys

-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs (y : ys) = isPrefixOf xs (y : ys) || isInfixOf xs ys

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf xs (y : ys) =
  length xs == length (y : ys) && isPrefixOf xs (y : ys)
  || isSuffixOf xs ys

-- zip
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys

-- zipWith
zipWith :: (a -> b -> c) -> ([a] -> ([b] -> [c]))
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [ys] = ys
intercalate _ [] = []
intercalate xs (y : ys) = y ++ xs ++ intercalate xs ys

-- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) =
  if elem' x xs
  then nub xs
  else x : nub xs

-- splitAt
splitAt :: (Integral i) => i -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt i (x : xs) =
  if i <= 0
  then ([], x : xs)
  else
    let (untilAt, afterAt) = splitAt (i - 1) xs
    in (x : untilAt, afterAt)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break f (x : xs) =
  if f x
  then ([], x : xs)
  else
    let (ys, zs) = break f xs
    in (x : ys, zs)

-- lines
lines :: String -> [String]
lines "" = []
lines st =
  let (leftStr, rightStr) = break (== '\n') st
  in if rightStr == ""
  then [leftStr]
  else leftStr : lines (tail rightStr)

-- words
words :: String -> [String]
words "" = []
words wrd =
  let (leftWrd, rightWrd) = break (== ' ') wrd
  in if rightWrd == ""
  then [leftWrd]
  else
    if leftWrd == ""
    then words (tail rightWrd)
    else leftWrd : words (tail rightWrd)

-- unlines
unlines :: [String] -> String
unlines [] = ""
unlines (x : xs) = x ++ "\n" ++ unlines xs

-- unwords
unwords :: [String] -> String
unwords = intercalate " " -- Leverages the intermediary function gen. by intercalate func.

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

