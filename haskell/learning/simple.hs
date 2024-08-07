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
--Concanation -> velocities ++ moreVelocities
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