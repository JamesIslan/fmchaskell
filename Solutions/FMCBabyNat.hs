module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- Sugar
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

-- (two + ) five === two + five
-- (+ two) five === five + two

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S _) = O

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg _ = one

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- less than
(<) :: Nat -> Nat -> Nat
O < (S _) = S O
_ < O = O
(S n) < (S m) = n < m

lt :: Nat -> Nat -> Nat
lt = (<)
infix 4 <

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S (S n)) = even n

odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S n)) = odd n

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

infixl 6 +  -- (+)-assL; precedence 6

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (-*)

(-*) :: Nat -> Nat -> Nat
O -* _ = O
n -* O = n
(S n) -* (S m) = n -* m

infixl 6 -*

-- multiplication
(*) :: Nat -> Nat -> Nat
_ * O = zero
n * (S m) = (n * m) + n

infixl 7 *  -- (*)-assL; precedence 7

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S m) = n ^ m * n

infixr 8 ^  -- (^)-assR; precedence 8

-- quotient
(/) :: Nat -> Nat -> Nat
_ / O = undefined
O / _ = zero
n / m =
  case n < m of
    S O -> zero -- Limited behaviour due to Naturals nature (lol :p)
    O -> one + (n -* m) / m

infixl 7 / -- Same as multiplication

-- remainder
(%) :: Nat -> Nat -> Nat
_ % O = undefined
n % m =
  case n < m of
    S O -> n
    O -> (n -* m) % m

infixl 7 %  -- Same as multiplication

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
(|||) :: Nat -> Nat -> Nat
O ||| n = isZero n
n ||| m = isZero (m % n)

divides :: Nat -> Nat -> Nat
divides = (|||)

infix 4 |||

-- x `absDiff` y = |x - y|
(|-|) :: Nat -> Nat -> Nat
n |-| m =
  case n < m of
    S O -> m -* n
    O -> n -* m

absDiff :: Nat -> Nat -> Nat
absDiff = (|-|)

infixl 6 |-|  -- Same as monus

-- factorial
(!) :: Nat -> Nat
(!) O = one
(!) (S n) = S n * (!) n

factorial :: Nat -> Nat
factorial = (!)

-- logarithm
-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined  -- logarithm with base 0 is undefined on naturals world
lo _ O = undefined  -- same
lo (S O) _ = undefined  -- same
lo n m =
  case m < n of  -- logarithm base greater than argument
    S O -> zero  -- 'cause we don't have access to decimal numbers
    O -> one + lo n (m / n)  -- will be one when m == n



