{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    , fst
    , snd
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where
    O == O = True
    S n == S m = n == m
    _ == _ = False

instance Ord Nat where
    O <= _ = True
    S _ <= O = False
    S n <= S m = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = zero
    min _ O = zero
    min (S n) (S m) = S (min n m)

    max O n = n
    max n O = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd n = not (even n)


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n <+> m)

infixl 6 <+>  -- (<+>)-assL; precedence 6

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (<->)

(<->) :: Nat -> Nat -> Nat
O <-> _ = O
n <-> O = n
(S n) <-> (S m) = n <-> m

infixl 6 <->  -- (<->)-assL; precedence 6

-- multiplication
(<*>) :: Nat -> Nat -> Nat
_ <*> O = zero
n <*> (S m) = (n <*> m) <+> n

times :: Nat -> Nat -> Nat
times = (<*>)

infixl 7 <*>  -- (<*>)-assL; precedence 7

-- power / exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> O = S O
n <^> (S m) = n <^> m <*> n

pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

infixr 8 <^>  -- (<^>)-assR; precedence 8


-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)  -- returns quotient and remainder
eucdiv (_, O) = undefined
eucdiv (n, m) =
    if n < m
    then (O, n) -- remainder is the n itself
    else (quo, rem)
        where
            (quo', rem') = eucdiv (n <-> m, m) -- we regress to a smaller division
            quo = S quo' -- quo will be succ of smaller division's quo'
            rem = rem'  -- rem will remain the same

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> m = fst (eucdiv (n, m))

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = snd (eucdiv (n, m))

-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> n = isZero n
n <|> m = isZero (m <%> n)

divides = (<|>)

infix 4 <|>  -- precedence 4


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)

(|-|) :: Nat -> Nat -> Nat
n |-| m = if n < m then m <-> n else n <-> m

dist :: Nat -> Nat -> Nat
dist = (|-|)

infixl 6 |-| -- (|-|)-assL; precedence 6

(!) :: Nat -> Nat
(!) O = one
(!) (S n) = S n <*> (!) n

factorial :: Nat -> Nat
factorial = (!)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined  -- logarithm with base 0 is undefined on naturals world
lo _ O = undefined  -- same
lo (S O) _ = undefined  -- same
lo n m =
    if m < n
    then zero  -- 'cause we don't have access to decimal numbers
    else S (lo n (m </> n))  -- will be one when m == n


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n
  | n < 0 = undefined
  | n == 0 = O
  | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
-- 1 and 0 are from type Integral
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger(x - 1))

