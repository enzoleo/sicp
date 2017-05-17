-- The solution of exercise 1.36
-- Modify `fixed-point` so that it prints the sequence of approximations it
-- generates, using the `newline` and `display` primitives shown in
-- exercise 1.22. Then find a solution to x ^ x = 1000 by finding a fixed
-- point of x -> log(1000)/log(x). (Use Scheme's primitive `log` procedure,
-- which computes natural logarithms.) Compare the number of steps this
-- takes with and without average damping. (Note that you cannot start
-- `fixed-point` with a guess of 1, as this would cause division by
-- log(1) = 0.)
--
-- -------- (above from SICP)
--
import Debug.Trace

-- The tolerance used in the fixed-point procedure
tolerance = 0.00001 :: Double

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess =
  let closeEnough v1 v2 =
        abs (v1 - v2) < tolerance
      try guess step =
        let next = f guess
        in trace ("Step[" ++ show step ++ "] " ++ show next) $
           if closeEnough guess next
           then next
           else (try next (step + 1))
  in try firstGuess 1

-- The procedure computes the root of equation: x ^ x = 1000
rootExpXX :: Double -> Double
rootExpXX firstGuess =
  fixedPoint (\x -> (log 1000) / (log x)) firstGuess

-- The procedure also computes the root of equation: x ^ x = 1000, but
-- using average damping. This technique computes the result with much less
-- steps.
rootExpXXAd :: Double -> Double
rootExpXXAd firstGuess =
  fixedPoint (\x -> (x + (log 1000) / (log x)) / 2) firstGuess

--
-- TEST
--
-- *Main> rootExpXX 1.5
-- Step[1] 17.036620761802716
-- Step[2] 2.436284152826871
-- Step[3] 7.7573914048784065
-- Step[4] 3.3718636013068974
-- Step[5] 5.683217478018266
-- Step[6] 3.97564638093712
-- Step[7] 5.004940305230897
-- Step[8] 4.2893976408423535
-- Step[9] 4.743860707684508
-- Step[10] 4.437003894526853
-- Step[11] 4.6361416205906485
-- Step[12] 4.503444951269147
-- Step[13] 4.590350549476868
-- Step[14] 4.532777517802648
-- Step[15] 4.570631779772813
-- Step[16] 4.545618222336422
-- Step[17] 4.562092653795064
-- Step[18] 4.551218723744055
-- Step[19] 4.558385805707352
-- Step[20] 4.553657479516671
-- Step[21] 4.55677495241968
-- Step[22] 4.554718702465183
-- Step[23] 4.556074615314888
-- Step[24] 4.555180352768613
-- Step[25] 4.555770074687025
-- Step[26] 4.555381152108018
-- Step[27] 4.555637634081652
-- Step[28] 4.555468486740348
-- Step[29] 4.555580035270157
-- Step[30] 4.555506470667713
-- Step[31] 4.555554984963888
-- Step[32] 4.5555229906097905
-- Step[33] 4.555544090254035
-- Step[34] 4.555530175417048
-- Step[35] 4.555539351985717
-- 4.555539351985717
-- *Main> rootExpXXAd 1.5
-- Step[1] 9.268310380901358
-- Step[2] 6.185343522487719
-- Step[3] 4.988133688461795
-- Step[4] 4.643254620420954
-- Step[5] 4.571101497091747
-- Step[6] 4.5582061760763715
-- Step[7] 4.555990975858476
-- Step[8] 4.555613236666653
-- Step[9] 4.555548906156018
-- Step[10] 4.555537952796512
-- Step[11] 4.555536087870658
-- 4.555536087870658



