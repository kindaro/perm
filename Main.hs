#! /usr/bin/env stack
{- stack
        --resolver lts-9.0
    script

-}

module Main where


import Control.Exception
import Numeric
import System.IO 
import Data.List
import Data.Maybe
import Data.Char


data FiniteCyclicGroup = Member { i :: Integer, n :: Integer } deriving (Ord)

makeMember :: Integer -> Integer -> FiniteCyclicGroup
makeMember i n 
            | i < 0 || n < 1  =  undefined
            | i == 0          =  Member n n
            | i <= n          =  Member i n
            | otherwise       =  Member (i `rem` n) n

(***) :: FiniteCyclicGroup -> Integer -> FiniteCyclicGroup
(***) x c = makeMember (i x * c) (n x) 

instance Eq (FiniteCyclicGroup) where

        (==) member_A member_B
            | n member_A == n member_B = i member_A == i member_B
            -- | otherwise = undefined 

instance Show (FiniteCyclicGroup) where
    
        -- show x = foldr (++) "" ["{ ", show (i x), "/", show (n x), " }" ]
        show x = foldr (++) "" [show (i x), "/", show (n x)]

instance Num (FiniteCyclicGroup) where

        (+) x y
            | n x == n y     =  makeMember (i x + i y) (n x)
            -- | otherwise      =  undefined

        (*) x y
            | n x == n y     =  Member (i x * i y) (n x)
            -- | otherwise      =  undefined

        abs x                =  x

        signum x             =  1

        fromInteger integer
            | integer > 0    =  Member 1 integer
            -- | otherwise      =  undefined

        negate x             =  makeMember (n x - i x) (n x)

instance Enum (FiniteCyclicGroup) where
        fromEnum member = fromEnum (i member)
        toEnum enum = Member 1 (toEnum enum)


instance Real (FiniteCyclicGroup) where
        toRational member = toRational (i member)


instance Integral (FiniteCyclicGroup) where

        toInteger member
            | i member <= n member  =  i member
            -- | otherwise             =  i member `rem` n member

        quotRem member_A member_B
            | n member_A == n member_B = (Member ((toInteger member_A) `quot` (toInteger member_B)) (n member_A), Member ((toInteger member_A) `rem` (toInteger member_B)) (n member_A))
            -- | otherwise = undefined

(///) = makeMember

inc, dec :: FiniteCyclicGroup -> FiniteCyclicGroup
inc m = m + 1 /// (n m)

dec m = m - 1 /// (n m)

to :: (Integral a) => FiniteCyclicGroup -> FiniteCyclicGroup -> a
to start end = fromIntegral $ inc $ end - start

slice :: [a] -> FiniteCyclicGroup -> FiniteCyclicGroup -> [a]
slice list start end  -- Start and end points included.
        | toInteger (length list) == n start && toInteger (length list) == n end
            =  slice' list start end
        | otherwise  =  undefined

    where

        slice' list start end
            = take
                (start `to` end)
                (drop (fromIntegral start - 1) list ++ take (fromIntegral start - 1) list)


data MarkedRing a = MarkedRing
        { value :: [a]
        , mark :: FiniteCyclicGroup
        }
    deriving (Eq,Ord,Show)

makeMarkedRing list
        = MarkedRing list (1///l)
    where l = fromIntegral $ length list

isCanonical ring
        | i (mark ring) == 1 = True
        | otherwise = False

shiftList :: Integer -> [a] -> [a]
shiftList clicks list
        = drop c list ++ take c list
    where
        l = fromIntegral $ length list
        c = fromIntegral $ i (clicks /// l)

shift :: FiniteCyclicGroup -> MarkedRing a -> MarkedRing a
shift clicks ring = MarkedRing
                        (shiftList (i clicks) $ value ring)
                        (mark ring - clicks)

canonicalize :: MarkedRing a -> MarkedRing a
canonicalize ring
        = shift (dec $ mark ring) ring

(+++) :: MarkedRing a -> MarkedRing a -> MarkedRing a
(+++) x y = makeMarkedRing (value x ++ value y)

startFrom :: FiniteCyclicGroup -> MarkedRing a -> MarkedRing a
startFrom start ring =
        assert ( (n start) == (n $ mark ring) ) $ shift (dec start) ring

sliceR :: FiniteCyclicGroup -> FiniteCyclicGroup -> MarkedRing a -> MarkedRing a
sliceR start end ring = makeMarkedRing $ take (start `to` end) $ value $ startFrom start ring

newtype P = P FiniteCyclicGroup

instance Show P where
    show (P x) = [mapIntToChar (fromIntegral $ i x)]

frag, nD, nPoints :: Integer
frag = 17  -- The number of points in each dial.
step = fromIntegral $ frag - 1  -- The step to take from a position in one dial to the same position in the next.
nD = fromIntegral $ min (length keys) ((length symbols) `quot` (fromIntegral step)) - 1 -- The number of dials.
-- nD = 3
nPoints = nD * step  -- The number of points in the daisy chain in total.

rootPoint dx = inc $ (dx * step) /// nPoints  -- The first point in every dial is called a root.
endPoint dx = rootPoint (dx + 1)  -- The end point of a dial is the same as the root of the next one.


daisyChain  =  makeMarkedRing $ take (fromIntegral nPoints) [ i | i <- symbols ]

final = value $ mutate daisyChain 1


-- dialSub dial list
--     | n dial == fromInteger . length list
--         = slice list (rootPoint dial) (endPoint dial)
--     | otherwise = undefined

-- turnDial dial clicks list = turnSub (rootPoint dial) (endPoint dial) clicks list
-- 
-- turnSubRing :: FiniteCyclicGroup -> FiniteCyclicGroup ->
--                     FiniteCyclicGroup -> MarkedRing -> MarkedRing
-- 

shiftSubRing :: FiniteCyclicGroup -> FiniteCyclicGroup ->
                    FiniteCyclicGroup -> MarkedRing a -> MarkedRing a
shiftSubRing start end clicks ring
    = assert (l == (n clicks))
        canonicalize $ MarkedRing (value $ (shift clicks $ sliceR (1///l') (l///l') r) +++ (sliceR (inc $ l///l') (0///l') r)) (mark r)
            where 
            l = start `to` end
            l' = n $ mark ring
            r = shift (dec start) ring

data Knob = Knob { start :: FiniteCyclicGroup, end :: FiniteCyclicGroup } deriving (Eq, Show)
makeKnob start end = assert (n start == n end) Knob start end
knobs = [ makeKnob (rootPoint i) (endPoint i) | i <- [0..nD-1] ]


-- d1 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (a4,a1,a2, a3,a5,a6, a7,a8,a9, aA,aB,aC)
-- d2 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (a1,a2,a3, a7,a4,a5, a6,a8,a9, aA,aB,aC)
-- d3 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (a1,a2,a3, a4,a5,a6, aA,a7,a8, a9,aB,aC)
-- d4 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (aC,a2,a3, a4,a5,a6, a7,a8,a9, a1,aA,aB)


keys = "qwerasdfzxcvtyuighjkbnm,"
symbols = map chr [33..126]
mapCharToInt c = elemIndex c $ take (length knobs) keys
mapIntToChar i = symbols !! i

turn knob = shiftSubRing (start knob) (end knob) (1 /// (start knob `to` end knob))

main = hSetEcho stdin False >> hSetBuffering stdin NoBuffering >> putStrLn (value daisyChain) >> (mutateIO daisyChain)

mutateIO s 
    | value s == final
        = putStrLn "You won dear friend."
    | otherwise = getChar >>= mutateIO' s >>= \x -> (putStrLn ( value x ) >> mutateIO x)

mutateIO' s c
    | fromEnum c == 27  -- Escape
        = undefined
    | isJust $ mapCharToInt c
        = return (mutate s (fromJust $ mapCharToInt c))
    | otherwise
        = return s

mutate daisyChainInstance knobID = turn (knobs !! fromIntegral knobID) daisyChainInstance
--

-- There are some nD dials. Each has frag fragments. Every two dials share one point position.
-- Dials thus form a daisy chain.
-- nD point positions are inner ones, and other X * nD are outer ones.

