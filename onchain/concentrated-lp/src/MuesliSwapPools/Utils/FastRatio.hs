{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module MuesliSwapPools.Utils.FastRatio(
    -- * Type
    FRational
    -- * Construction
    , unsafeFRatio
    , ffromInteger
    , fratio
    -- * Other functionality
    , numerator
    , denominator
    , fround
    , ftruncate
    , fproperFraction
    , frecip
    , fabs
    , fnegate
    , fhalf
    , ffromGHC
    , toGHC
    , freduce
    , freduced
    , freduceFRational
    ) where

import qualified PlutusTx.Applicative as P
import qualified PlutusTx.Base as P
import qualified PlutusTx.Bool as P
import qualified PlutusTx.Eq as P
import qualified PlutusTx.ErrorCodes as P
import PlutusTx.Integer (Integer)
import PlutusTx.Bool (Bool)
import qualified PlutusTx.IsData as P
import qualified PlutusTx.Lift as P
import qualified PlutusTx.Maybe as P
import qualified PlutusTx.Numeric as P
import qualified PlutusTx.Ord as P
import qualified PlutusTx.Trace as P

import qualified PlutusTx.Builtins as Builtins

import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:))
import qualified GHC.Real as Ratio
import Prelude (Ord (..), Show, (*))
import qualified Prelude as Haskell

import PlutusTx.Ratio (gcd)

-- | Represents an arbitrary-precision ratio.
data FRational = FRational Integer Integer
  deriving stock (
    Haskell.Eq,
    Show
    )

-- We maintain no invariants for FastFRational
-- the option to norm rational is totally up to the developer

instance P.Eq FRational where
  {-# INLINABLE (==) #-}
  -- n/d == n'/d' <--> n * d' == n' * d
  FRational n d == FRational n' d' = n P.* d' P.== n' P.* d

instance P.Ord FRational where
  {-# INLINABLE compare #-}
  compare (FRational n d) (FRational n' d') = P.compare (n P.* d') (n' P.* d)
  {-# INLINABLE (<=) #-}
  FRational n d <= FRational n' d' = (n P.* d') P.<= (n' P.* d)
  {-# INLINABLE (>=) #-}
  FRational n d >= FRational n' d' = (n P.* d') P.>= (n' P.* d)
  {-# INLINABLE (<) #-}
  FRational n d < FRational n' d' = (n P.* d') P.< (n' P.* d)
  {-# INLINABLE (>) #-}
  FRational n d > FRational n' d' = (n P.* d') P.> (n' P.* d)

instance Ord FRational where
  compare (FRational n d) (FRational n' d') = compare (n * d') (n' * d)
  FRational n d <= FRational n' d' = (n * d') <= (n' * d)
  FRational n d >= FRational n' d' = (n * d') >= (n' * d)
  FRational n d < FRational n' d' = (n * d') < (n' * d)
  FRational n d > FRational n' d' = (n * d') > (n' * d)

instance P.AdditiveSemigroup FRational where
  {-# INLINABLE (+) #-}
  FRational n d + FRational n' d' =
    let newNum = (n P.* d') P.+ (n' P.* d)
        newDen = d P.* d'
     in FRational newNum newDen

instance P.AdditiveMonoid FRational where
  {-# INLINABLE zero #-}
  zero = FRational P.zero P.one

instance P.AdditiveGroup FRational where
  {-# INLINABLE (-) #-}
  FRational n d - FRational n' d' =
    let newNum = (n P.* d') P.- (n' P.* d)
        newDen = d P.* d'
     in FRational newNum newDen

instance P.MultiplicativeSemigroup FRational where
  {-# INLINABLE (*) #-}
  FRational n d * FRational n' d' =
    let newNum = n P.* n'
        newDen = d P.* d'
     in FRational newNum newDen

instance P.MultiplicativeMonoid FRational where
  {-# INLINABLE one #-}
  one = FRational P.one P.one

instance P.Module Integer FRational where
  {-# INLINABLE scale #-}
  scale i (FRational n d) = let newNum = i P.* n in
    FRational newNum d

instance P.ToData FRational where
  {-# INLINABLE toBuiltinData #-}
  -- we want builtin data rationals to always be as small as possible
  toBuiltinData (FRational n d) 
    | d P.== P.zero = Builtins.error ()
    | d P.< P.zero = P.toBuiltinData P.$ FRational (P.negate n) (P.negate d)
    | P.True =
        let gcd' = euclid n d
        in P.toBuiltinData (n `Builtins.quotientInteger` gcd', d `Builtins.quotientInteger` gcd')

-- These instances ensure that the following invariants don't break:
--
-- 1. The denominator is greater than 0; and
-- 2. The numerator and denominator are coprime.
--
-- For invariant 1, fromBuiltinData yields Nothing on violation, while
-- unsafeFromData calls error. Invariant 2 is kept maintained by use of
-- unsafeFRatio.

instance P.FromData FRational where
  {-# INLINABLE fromBuiltinData #-}
  fromBuiltinData dat = do
    (n, d) <- P.fromBuiltinData dat
    guard (d P./= P.zero)
    P.pure P.. unsafeFRatio n P.$ d

instance P.UnsafeFromData FRational where
  {-# INLINABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = P.uncurry unsafeFRatio P.. P.unsafeFromBuiltinData

-- | This mimics the behaviour of Aeson's instance for 'GHC.Rational'.
instance ToJSON FRational where
  toJSON (FRational n d) =
    object
      [ ("numerator", toJSON n)
      , ("denominator", toJSON d)
      ]

-- | This mimics the behaviour of Aeson's instance for 'GHC.Rational'.
instance FromJSON FRational where
  parseJSON = withObject "FRational" Haskell.$ \obj -> do
    n <- obj .: "numerator"
    d <- obj .: "denominator"
    case fratio n d of
      Haskell.Nothing -> Haskell.fail "Zero denominator is invalid."
      Haskell.Just r  -> Haskell.pure r

-- | Makes a 'FRational' from a numerator and a denominator.
--
-- = Important note
--
-- If given a zero denominator, this function will error. If you don't mind a
-- size increase, and care about safety, use 'ratio' instead.
{-# INLINABLE unsafeFRatio #-}
unsafeFRatio :: Integer -> Integer -> FRational
unsafeFRatio n d
  | d P.== P.zero = Builtins.error ()
  | P.True = FRational n d

-- | Safely constructs a 'FRational' from a numerator and a denominator. Returns
-- 'Nothing' if given a zero denominator.
{-# INLINABLE fratio #-}
fratio :: Integer -> Integer -> P.Maybe FRational
fratio n d
  | d P.== P.zero = P.Nothing
  | P.True = P.Just (FRational n d)

-- | Converts a 'FRational' to a GHC 'Ratio.Rational', preserving value. Does not
-- work on-chain.
toGHC :: FRational -> Ratio.Rational
toGHC (FRational n d) = n Ratio.% d

-- | Returns the numerator of its argument.
--
-- = Note
--
-- It is /not/ true in general that @'numerator' '<$>' 'ratio' x y = x@; this
-- will only hold if @x@ and @y@ are coprime. This is due to 'FRational'
-- normalizing the numerator and denominator.
{-# INLINABLE numerator #-}
numerator :: FRational -> Integer
numerator (FRational n _) = n

-- | Returns the denominator of its argument. This will always be greater than,
-- or equal to, 1, although the type does not describe this.
--
-- = Note
--
-- It is /not/ true in general that @'denominator' '<$>' 'ratio' x y = y@; this
-- will only hold if @x@ and @y@ are coprime. This is due to 'FRational'
-- normalizing the numerator and denominator.
{-# INLINABLE denominator #-}
denominator :: FRational -> Integer
denominator (FRational _ d) = d

-- | 0.5
{-# INLINABLE fhalf #-}
fhalf :: FRational
fhalf = FRational 1 2

-- | Converts an 'Integer' into the equivalent 'FRational'.
{-# INLINABLE ffromInteger #-}
ffromInteger :: Integer -> FRational
ffromInteger num = FRational num P.one

-- | Converts a GHC 'Ratio.Rational', preserving value. Does not work on-chain.
ffromGHC :: Ratio.Rational -> FRational
ffromGHC r = unsafeFRatio (Ratio.numerator r) (Ratio.denominator r)

-- | Produces the additive inverse of its argument.
--
-- = Note
--
-- This is specialized for 'FRational'; use this instead of the generic version
-- of this function, as it is significantly smaller on-chain.
{-# INLINABLE fnegate #-}
fnegate :: FRational -> FRational
fnegate (FRational n d) = FRational (P.negate n) d

-- | Returns the absolute value of its argument.
--
-- = Note
--
-- This is specialized for 'FRational'; use this instead of the generic version
-- in @PlutusTx.Numeric@, as said generic version produces much larger on-chain
-- code than the specialized version here.
{-# INLINABLE fabs #-}
fabs :: FRational -> FRational
fabs rat@(FRational n d)
  | (n P.> P.zero) P./= (d P.> P.zero) = FRational (P.negate n) d
  | P.True = rat

-- | @'properFraction' r@ returns the pair @(n, f)@, such that all of the
-- following hold:
--
-- * @'fromInteger' n 'P.+' f = r@;
-- * @n@ and @f@ both have the same sign as @r@; and
-- * @'abs' f 'P.<' 'P.one'@.
{-# INLINABLE fproperFraction #-}
fproperFraction :: FRational -> (Integer, FRational)
fproperFraction (FRational n d) = (n `Builtins.quotientInteger` d, FRational (n `Builtins.remainderInteger` d) d)

-- | Gives the reciprocal of the argument; specifically, for @r 'P./='
-- 'P.zero'@, @r 'P.*' 'recip' r = 'P.one'@.
--
-- = Important note
--
-- The reciprocal of zero is mathematically undefined; thus, @'recip' 'P.zero'@
-- will error. Use with care.
{-# INLINABLE frecip #-}
frecip :: FRational -> FRational
frecip (FRational n d)
  | n P.== P.zero = Builtins.error ()
  | P.True = FRational d n

-- | Returns the whole-number part of its argument, dropping any leftover
-- fractional part. More precisely, @'truncate' r = n@ where @(n, _) =
-- 'properFraction' r@, but is much more efficient.
{-# INLINABLE ftruncate #-}
ftruncate :: FRational -> Integer
ftruncate (FRational n d) = n `Builtins.quotientInteger` d

-- | @'round' r@ returns the nearest 'Integer' value to @r@. If @r@ is
-- equidistant between two values, the even value will be given.
{-# INLINABLE fround #-}
fround :: FRational -> Integer
fround x =
  let (n, r) = fproperFraction x
      m = if r P.< P.zero then n P.- P.one else n P.+ P.one
      flag = fabs r P.- fhalf
   in if
          | flag P.< P.zero -> n
          | flag P.== P.zero -> if Builtins.modInteger n 2 P.== P.zero
                                then n
                                else m
          | P.True -> m

-- From GHC.Real
-- | Given a numerator and denominator, produces a 'Rational' by dividing both
-- numerator and denominator by their greatest common divisor.
{-# INLINABLE freduce #-}
freduce :: Integer -> Integer -> FRational
freduce x y
    | y P.== 0 = P.traceError P.ratioHasZeroDenominatorError
    | P.True     =
        let d = gcd x y in
          FRational (x `Builtins.quotientInteger` d)
                   (y `Builtins.quotientInteger` d)

-- produce an FRational which is reduced by the greatest common divisor
{-# INLINABLE freduceFRational #-}
freduceFRational :: FRational -> FRational
freduceFRational (FRational x y) = freduce x y

-- check whether an FRational is reduced
{-# INLINABLE freduced #-}
freduced :: FRational -> Bool
freduced (FRational x y) = 
    let (FRational x' y') = freduce x y in
    x P.== x' P.&& y P.== y'


-- Helpers

-- Euclid's algorithm
{-# INLINABLE euclid #-}
euclid :: Integer -> Integer -> Integer
euclid x y
  | y P.== P.zero = x
  | P.True = euclid y (x `Builtins.modInteger` y)

P.makeLift ''FRational

{- HLINT ignore -}

{- Note [FRatio]

An important invariant is that the denominator is always positive. This is
enforced by

* Construction of 'FRational' numbers with 'unsafeFRational' (the constructor
  of 'FRational' is not exposed)
* Normalizing after every numeric operation.

The 'StdLib.Spec' module has some property tests that check the behaviour of
'round', 'truncate', '>=', etc. against that of their counterparts in
'GHC.Real'.

-}

{- NOTE [Integer division operations]

Plutus Core provides built-in functions 'divideInteger', 'modInteger',
'quotientInteger' and 'remainderInteger' which are implemented as the Haskell
functions 'div', 'mod', 'quot', and 'rem' respectively.

The operations 'div' and 'mod' go together, as do 'quot' and 'rem': * DO NOT use
'div' with 'rem' or 'quot' with 'mod' *.  For most purposes users shoud probably
use 'div' and 'mod': see below for details.

For any integers a and b with b nonzero we have

  a * (a  `div` b) + a `mod` b = a
  a * (a `quot` b) + a `rem` b = a

(all operations give a "divide by zero" error if b = 0).  The analagous
identities for div/rem and quot/mod don't hold in general, and this can
lead to problems if you use the wrong combination of operations.

For positive divisors b, div truncates downwards and mod always returns a
non-negative result (0 <= a `mod` b <= b-1), which is consistent with standard
mathematical practice.  The `quot` operation truncates towards zero and `rem`
will give a negative remainder if a<0 and b>0.  If a<0 and b<0 then `mod` willl
also yield a negative result.  Results for different combinations of signs are
shown below.

-------------------------------
|   n  d | div mod | quot rem |
|-----------------------------|
|  41  5 |  8   1  |   8   1  |
| -41  5 | -9   4  |  -8  -1  |
|  41 -5 | -9  -4  |  -8   1  |
| -41 -5 |  8  -1  |   8  -1  |
-------------------------------

For many purposes (in particular if you're doing modular arithmetic),
a positive remainder is what you want.  Using 'div' and 'mod' achieves
this for positive values of b (but not for b negative, although doing
artimetic modulo a negative number would be unusual).

There is another possibility (Euclidean division) which is arguably more
mathematically correct than both div/mod and quot/rem. Given integers a and b
with b != 0, this returns numbers q and r with a = q*b+r and 0 <= r < |b|.  For
the numbers above this gives

-------------------
|   n  d |  q   r |
|-----------------|
|  41  5 |  8   1 |
| -41  5 | -9   4 |
|  41 -5 | -8   1 |
| -41 -5 |  9   4 |
-------------------

We get a positive remainder in all cases, but note for instance that the pairs
(41,5) and (-41,-5) give different results, which might be unexpected.

For a discussion of the pros and cons of various versions of integer division,
see Raymond T. Boute, "The Euclidean definition of the functions div and mod",
ACM Transactions on Programming Languages and Systems, April 1992.  (PDF at
https://core.ac.uk/download/pdf/187613369.pdf)
-}
