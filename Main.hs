module Main where


import Control.Exception


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
            | otherwise = undefined 

instance Show (FiniteCyclicGroup) where
    
        -- show x = foldr (++) "" ["{ ", show (i x), "/", show (n x), " }" ]
        show x = foldr (++) "" [show (i x), "/", show (n x)]

instance Num (FiniteCyclicGroup) where

        (+) x y
            | n x == n y     =  makeMember (i x + i y) (n x)
            | otherwise      =  undefined

        (*) x y
            | n x == n y     =  Member (i x * i y) (n x)
            | otherwise      =  undefined

        abs x                =  x

        signum x             =  1

        fromInteger integer
            | integer > 0    =  Member 1 integer
            | otherwise      =  undefined

        negate x             =  makeMember (n x - i x) (n x)

instance Enum (FiniteCyclicGroup) where
        fromEnum member = fromEnum (i member)
        toEnum enum = Member 1 (toEnum enum)


instance Real (FiniteCyclicGroup) where
        toRational member = toRational (i member)


instance Integral (FiniteCyclicGroup) where

        toInteger member
            | i member <= n member  =  i member
            | otherwise             =  i member `rem` n member

        quotRem member_A member_B
            | n member_A == n member_B = (Member ((toInteger member_A) `quot` (toInteger member_B)) (n member_A), Member ((toInteger member_A) `rem` (toInteger member_B)) (n member_A))
            | otherwise = undefined

(+++) :: FiniteCyclicGroup -> FiniteCyclicGroup
(+++) member = (makeMember (toInteger member + 1)) (n member)


nD, frag :: Integer
nD = 4  -- The number of dials.
frag = 4  -- The number of points in each dial.
step = frag - 1  -- The step to take from a position in one dial to the same position in the next.
nPoints = nD * step  -- The number of points in the daisy chain in total.

rootPoint dx = dx * step  -- The first point in every dial is called a root.
endPoint dx = rootPoint (dx + 1)  -- The end point of a dial is the same as the root of the next one.


daisyChain  =  [ makeMember i nPoints | i <- [1..nPoints] ]

slice :: [a] -> FiniteCyclicGroup -> FiniteCyclicGroup -> [a]
slice list start end
        | toInteger (length list) == n start && toInteger (length list) == n end
            =  slice' list start end
        | otherwise  =  undefined

    where
        to :: FiniteCyclicGroup -> FiniteCyclicGroup -> Int
        to start end = fromIntegral (i end - i start + 1)
        slice' list start end
            -- | start `to` end < 1  =  drop (i start - 1) list ++ take (i end) list
            -- | otherwise  =  take drop (i start - 1)

            -- = take (start `to` end) (drop (start `to` end) list ++ take (start `to` end) list)

-- d1 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (a4,a1,a2, a3,a5,a6, a7,a8,a9, aA,aB,aC)
-- d2 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (a1,a2,a3, a7,a4,a5, a6,a8,a9, aA,aB,aC)
-- d3 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (a1,a2,a3, a4,a5,a6, aA,a7,a8, a9,aB,aC)
-- d4 (a1,a2,a3, a4,a5,a6, a7,a8,a9, aA,aB,aC) = (aC,a2,a3, a4,a5,a6, a7,a8,a9, a1,aA,aB)




--

-- There are some nD dials. Each has frag fragments. Every two dials share one point position.
-- Dials thus form a daisy chain.
-- nD point positions are inner ones, and other X * nD are outer ones.

