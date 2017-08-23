{-# LANGUAGE NamedFieldPuns #-}

module KspQuad where

  data PID = PID { kp :: Float, ki :: Float, kd :: Float, sumErr :: Float, lastErr :: Point }

  data Point = Point { val :: Float, time :: Int } -- replace time with timestamp type

  dxdt ::  Point -> Point -> Float
  dxdt (Point v1 t1) (Point v2 t2) = (v1 - v2) / (fromIntegral $ t1 - t2)

  evalPid :: PID -> Point -> (Float, PID)
  evalPid pid err = (prop err + int err + der err, pid')
    where prop err = (kp pid) * (val err)
          int err  = (ki pid) * (sumErr pid)
          der err  = (kd pid) * dxdt err (lastErr pid)
          pid'     =  pid { sumErr = sumErr pid + val err, lastErr = err }

  data Vehicle = Vehicle
    { dragCoef :: Float
    , armLength :: Float
    , mass :: Float
    , motorStr :: Float
    } -- k is TWR

  k v = 4 * motorStr v / mass v

  data ControlState = ControlState
    { eTheta :: Float
    , ePhi :: Float
    , ePsi :: Float
    , eThrust :: Float
    }

  xRot :: Vehicle -> ControlState -> Float
  xRot v cs = 2 * (dragCoef v) * (ePhi cs) * (ix v) + (ePsi cs) * (iz cs) * (k v) * (armLength v) / (4 * (dragCoef v) * (k v) * (armLength v))

  yRot :: Vehicle -> ControlState -> Float
  yRot v cs = (eTheta v) * (iy v) / (2 * (k v) * (armLength v))

  zRot :: Vehicle -> ControlState -> Float
  zRot v cs = (ePsi v) * (iz v) / (4 * dragCoef v)

  data MotorState = MotorState Float Float Float Float

  data Vector a = Vector a a a

  add :: Num a => Vector a -> Vector a -> Vector a
  add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

  scale :: Num a => a -> Vector a -> Vector a
  scale k (Vector x y z) = Vector k*x k*y k*z

  roll :: Vector a -> a
  roll (Vector x _ _) = x

  pitch :: Vector a -> a
  pitch (Vector _ y _) = y

  yaw :: Vector a -> a
  yaw (Vector _ _ z) = z

  cross :: Num a => Vector a -> Vector a -> Vector a
  cross (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y2 * x1)

  rotate :: Num a => Vector a -> Vector a -> Vector a
  rotate rot vec =
  data VehicleState = VehicleState
    { pos :: Vector
    , vel :: Vector
    , rot :: Vector
    , rv  :: Vector
    , v   :: Vehicle
    }

  stepPhysics :: VehicleState -> MotorState -> Int -> VehicleState

  vehicleAccel :: VehicleState -> MotorState -> Vector
  vehicleAccel VehicleState { rot, v } (MotorState a b c d) = scale fMag (Vector x y z)
    where fMag = (a + b + c + d) * (motorStr v)
          x = (cos $ yaw rot) * (cos $ pitch rot)
          y = (sin $ yaw rot) * (cos $ pitch rot)
          z = (sin $ pitch rot)

  gVec :: Vector Float
  gVec = Vector 0 0 -9.8

  linPhysics :: VehicleState -> MotorState -> Int -> VehicleState
  linPhysics vs ms dt = vs { pos = pos', vel = vel' }
    where vel' = scale dt (vehicleAccel vs ms) `add` (vel vs) `add` scale dt gVec
          pos' = pos `add` scale dt vel'

  torque :: Vector

  rotPhysics :: VehicleState -> MotorState -> Int -> VehicleState
  rotPhysics vs ms dt = vs { rot = rot', rv = rv' }
    where rot' =
          rv'  =
