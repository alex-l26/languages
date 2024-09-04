-- simple 

tx a b = a * b
t3 = tx 3

--Defining a constant
e :: Double
e = exp 1

--Defining a function
square :: Double -> Double
square x = x**2

--Exercises 2
--2.1
f :: Double -> Double
f x = sqrt (x+1)

--2.2
g = 9.81
yRock30 :: Double -> Double
-- yRock30 > 0
yRock30 t = 30*t - 1/2 * 9.81 * t**2

--2.3
vRock30 :: Double -> Double
vRock30 t = 30 - 9.81 * t

--2.4
sinDeg x = sin (x * pi / 180)

--2.5
cuberoot x = x**(1/3)
g2 x = exp x + 8**x
h x = 1 / sqrt ((x-5)**2  + 16)
-- \x -> 1 / sqrt (1 - x**2)
u x = 1 / (10 + x) + 1 / (10 - x)
f4 x = sqrt (x * (x + 1))
f2 x  = 1 / (abs x)**3
f3 x = 1 / (x**2 +4)**3/2

--2.6
-- \x -> 1 / sqrt (1 - x**2)

stepFunction :: Double -> Double
stepFunction x = if x <= 0 
    then 0
    else 1

--Describing motion (1D)
averageVelocity :: Time -> Time -> PositionFunction -> Velocity
averageVelocity t0 t1 x = (x t1 - x t0)/ (t1 - t0)

averageVelocity2 :: Time -> TimeInterval -> PositionFunction -> Velocity
averageVelocity2 t dt x = (x (t + dt/2) - x (t - dt/2))/dt 

type R = Double

type Time = R
type TimeInterval = R
type Position = R
type Velocity = R

type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity

type Derivative = (R -> R) -> R -> R
derivative :: R -> Derivative
derivative dt x t = (x (t + dt/2) - x (t - dt/2))/dt 

--Modeling

carPosition :: Time -> Position
carPosition t = cos t 

carVelocity :: Time -> Velocity
carVelocity =  derivative 0.01 carPosition

carVelocityAnalylytic :: Time -> Velocity
carVelocityAnalylytic t = -sin t

-- Close but not equal 
    -- carVelocity 2 = -0.9092936380911187
    -- carVelocityAnalytic 2 = -0.9092974268256817

velFromPos :: R              --dt
    -> (Time -> Position)    --position function
    -> (Time -> Velocity)    --VelocityFunction
velFromPos dt x = derivative dt x
  
--Constant Velocity
positionCV :: Position -> Velocity -> Time -> Position -- CV -> constant velocity
positionCV x0 v0 t = v0 * t + x0

--Modelling Acceleration
type Acceleration = R
accFromVel :: R                --dt
    -> (Time -> Velocity)      --velocity function
    -> (Time -> Acceleration) --acceleration function
accFromVel = derivative

--Constant Acceleration
velocityCA :: Velocity -> Acceleration -> Time -> Velocity
velocityCA v0 a0 t = a0 * t + v0 

positionCA :: Position -> Velocity -> Acceleration -> Time -> Position
positionCA x0 v0 a0 t = a0 * t**2 / 2 + v0 * t + x0

--Exercises 

--4.1
f4_1 x = x**2 / 2
--derivative 10 f4_1 2  = 2
--derivative 1 f4_1 2   = 2
--derivative 0.1 f4_1 2 = 1.9999999999999996
-- 0.1 does not have an exact binary representation so does not return exactly 2

--4.2
f4_2 x = x ** 3
analyticDf4_2 x = 3 * x ** 2
errorf4_2 a x =  derivative a f4_2 x - analyticDf4_2 x
-- error 0.1 = 0.0025 (2.5e-3), 1 = 0.25, 5 = 6.25 10 = 25,
-- error = a ** 2 / 4 where a is the derivative value
-- x = 4, analyticDf4_2 x = 48 -> for error = 0.001, a = sqrt(4 * 0.001) = 6.324555320336758e-2
-- x=0.1, analyticDf4_2 = 0.03 -> for error = 0.001, a is same as above

--4.3
-- for a = 0.01, error >= 0.1
f4_3 x =  x 
analyticDf4_3 x = 1 
errorf4_3 a x = derivative a f4_3 x - analyticDf4_3 x
-- just use f4_2 with a very large value of x to get a big error 

--4.4
--values of t around axis
--values between axis and not small 

--4.5
pos1 :: Time -> Position
pos1 t = if t < 0 
    then 0
    else 5 * t**2 

vel1Analytic :: Time -> Velocity
vel1Analytic t = if t < 0
    then 0 
    else 10 * t 

acc1Analytic :: Time -> Acceleration
acc1Analytic t = if t < 0
    then 0
    else 10

vel1Numerical :: Time -> Velocity
vel1Numerical t = derivative 0.01 pos1 t

acc1Numerical :: Time -> Acceleration
acc1Numerical t = derivative 0.01 vel1Numerical t

--Lists

--e.g
physicists :: [String]
physicists = ["Einstein" , "Newton" , "Maxwell"]
velocities :: [R]
velocities = [0 , -9.8 , -29.6 , -29.4]
-- velocities !! 0 = 0 etc
moreVelocities :: [R]
moreVelocities = [-39.2 , -49]
--Concatenation -> velocities ++ moreVelocities
-- this one works as they have the same type
shortWords :: [String]
shortWords = ["am" , "I" , "to"]
--concat [shortWords , physicists , shortWords] = ["am","I","to","Einstein","Newton","Maxwell","am","I","to"]
-- maintains order

--Arithmetic Sequences
ns :: [Int]
ns = [0..10] --Integers from 0 to 10
-- can use a different increment step by modifying the second term
-- [-2 , -1.5 .. 1]

--List Types
--square :: R -> R
--square x = x**2
funcs :: [R -> R]
funcs = [cos , square , sin]

-- :t length
--length :: Foldable t => t a -> Int    ->> complicated type (type class)

--List Comprehensions
ts :: [R]
ts = [0 , 0.1 .. 6] 
--Recall yRock30 function from before
--We can use yRock30 and apply it to ts to create a list of positions
xs :: [R]
xs = [yRock30 t | t <- ts]

--Sum and Products
--With list comprehensions can mimic sigma and pi notation
-- sum [f(i) | i <- [m ..n ]]
-- product [f(i) | i <- [m .. n]]

--Infinite lists
--[n..]
--Computer will continually generate numbers if you type in an infite list
--Take function is useful when dealing with infinite lists, particularly when combined with other functions
-- take 10 (cycle [4 , 7 , 8])

--List Constructors
-- 3 : [4 , 5] == [3 , 4 , 5] -> True
--Lists are represented differently internally
    --[13 , 6 , 4] == 13 : 6 : 4 : []
--cons operator (:) is right associative -> x : z : _ == x : (z : _)

sndItem :: [a] -> a
sndItem ys = case ys of
    [] -> error "Empty list has no second element"
    (x : xs) -> if null xs
                then error "1-item list has no second item"
                else head xs
-- In this x and xs are assigned locally, so only have meaning here

sndItem2 :: [a] -> a
sndItem2 [] = error "Empty list has no second element"
sndItem2 (x : xs) = if null xs
                    then error "1-item list has no second item"
                    else head xs

sndItem3 :: [a] -> a
sndItem3 ys = case ys of
                [] -> error "Empty list has no second element"
                (x : []) -> error "1-item list has no second item"
                (x : z : _) -> z


--Exercises
--5.1
numbers5_1 :: [R]
numbers5_1 = [-2 , -1.2 .. 2.0]

--5.2
sndItem0 :: [a] -> a
sndItem0 xs = xs !! 1

--5.3
--Type = Int, value = 13

--5.4
f5_4 :: Int -> [Int]
f5_4 n = [1 .. n]
--Creates a list of all integers up to the imput

--5.5
null' :: [a] -> Bool
null' ys = length ys < 1
            

--5.6
last' xs = head (reverse xs)

--5.7
palindrome :: String -> Bool
palindrome xs = if reverse xs == xs
                then True
                else False

--5.8
-- take 5 [9 , 1 ..]

--5.11
cycle' :: [a] -> [a]
cycle' xs = concat (repeat xs)

--5.10
-- (a)
--Not valid different types in list (string/int)
-- (b)
--not valid as different types (char/string)
-- (c)
--valid, type = char
-- (d)
--valid, type = int
-- (e)
--valid, type = int
-- (f)
-- valid, type = reverse :: [a] -> [a]

--5.11
--Because of how floating point numbers work the value may exceed a specifically given endpoint, as it is in the second example

--5.12
eulerpi m n = sum [1 / (i ** 2) | i <- [m .. n]]

--5.13
factorial :: Int -> Int 
factorial n = product [i | i <- [1 .. n]]

--5.14
expList :: R -> [R]
expList x = [(1 + x / n) ** n | n <- [1 ..]]

--5.15
expSeries :: R -> [R]
--expSeries x = [sum [x ** m / factorial m | n <- [1..10], m <- [1 ..]]]
expTerm :: R -> Int -> R
expTerm x m = x ** (fromIntegral m) / fromIntegral (factorial m)
expSeries x = scanl1 (+) (map (expTerm x) [0..])

--Higher Order Functions
springForce :: R -> R -> R
springForce k x = -k * x
-- higher order function as R -> R -> R == (R -> R) -> R

--Digital Integration
type Integration = (R -> R) --function
                    -> R    --lower limit
                    -> R    --upper limit
                    -> R    --Result
integral :: R -> Integration
integral dt f a b = sum [f t * dt | t <- [a + dt / 2 , a + 3 * dt / 2 .. b - dt / 2]]
--e.g integral 0.01 (\x -> x**2) 0 1   -- we write the anonymous fuction as its easier than writing an independant definition, this is somewhere where anonymous functions can be very useful

--Implementing antiderivatives
--because of how antiderivatives work we cannot do it similarly to how we would by hand
--we must make a function that has an initial value
type AntiDerivative = R         --initial value
                    -> (R -> R) --funtion
                    -> (R -> R) --antiderivative of function
antiDerivative :: R -> AntiDerivative
antiDerivative dt v0 a t = v0 + integral dt a 0 t 

--we did vel from pos and acc from vel, now we can do the reverse

velFromAcc :: R                       --dt
            -> Velocity               --initial velocity
            -> (Time -> Acceleration) --acceleration functin
            -> (Time -> Velocity)     --velocity function
velFromAcc dt v0 a t = antiDerivative dt v0 a t

posFromVel :: R                   --dt
            -> Position           --initial position
            -> (Time -> Velocity) --velocity function
            -> (Time -> Position) --position function
posFromVel = antiDerivative

--Exercises
--6.1
yRock :: R -> R -> R
yRock v0 t = v0 * t - (1 / 2) * 9.81 * t ** 2

vRock :: R -> R -> R
vRock v0 t = v0 - 9.81 * t

--6.2
--take 4 :: [a] -> [a]

--6.3
--map not :: [Bool] -> [Bool]

--6.4
greaterThanOrEq7' :: Int -> Bool
greaterThanOrEq7' n = n > 6

--6.5
f6_4 :: Int -> String -> Bool
f6_4 n str = length str >= n 
--The function checks if the length of the string is greatere than the integer

--6.6
f6_6 :: [a] -> Bool
f6_6 ls = length ls > 6

--6.7
-- 'x' :: Char, a list with elements of type Char is a string

--6.8
-- map (**2) [1..1000]

--6.9
repeat' :: a -> [a]
repeat' = iterate (\n -> n)

--6.10
replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x)

--6.11
lsCarVel = iterate (\n -> n + 5) 0 

--6.12
map' :: (a -> b) -> [a] -> [b]
map' (f) ls = [f x |x <- ls]

--6.13
filter' :: (a -> Bool) -> [a] -> [a]
filter' (f) ls = [x | x <- ls, f x] 

--6.14
average :: [R] -> R
average xs = (sum [x | x <- xs]) / (fromIntegral (length xs))

--6.25 -> skip

--6.16
trapIntegrate :: Int        --number of trapezoids n
                -> (R -> R) --function f
                -> R        --lower limit a
                -> R        --upper limit b
                -> R        --result
trapIntegrate n f a b = 
    let 
        h = (b - a) / fromIntegral n
        xValues6_16 = [a + h * fromIntegral i | i <- [0..n]]
        fxValues6_16 = map f xValues6_16
    in
    (h / 2) * (head fxValues6_16 + last fxValues6_16 + 2 * sum (init (tail fxValues6_16)))