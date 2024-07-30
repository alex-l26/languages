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

carVelocityAnyalytic :: Time -> Velocity
carVelocityAnyalytic t = -sin t

velFromPos :: R              --dt
    -> (Time -> Position)    --position function
    -> (Time -> Velocity)    --VelocityFunction
velFromPos dt x = derivative dt x
 