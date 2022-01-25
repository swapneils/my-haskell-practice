-- |

-- module nil where
import Data.List
import Data.Char
import Control.Monad


main = do
  times <- fmap read getLine
  forM_ [1..times] $ \_ -> do
    input <- getLine
    putStrLn $ concatMap show $ replacePairs' $ map digitToInt input


-- Problem B
replacePairs inp =
  -- maximum $ map (\k -> shower (inserter inp k)) [1..(length inp - 1)]
  read (foldr comparing "0" $ map converter [(el-1),(el-2)..1]) :: Integer
  where converter k = shower $ inserter inp k
        shower xs = (foldr (++) "" $ map show xs)
        inserter a k = (let (front, back) = splitAt (k-1) a in front ++ [head back + head (drop 1 back)] ++ (drop 2 back))
        el = length inp
        intread x = (read x) :: Integer
        comparing temp acc = if (ltemp == el) then temp else
                                 show (max (intread acc) (intread temp))
          where ltemp = length temp

-- Taken from divanik's solution
replacePairs' inp@(x:xs) = let sums = zipWith (+) xs inp in
                             let t = lastOcc sums (>= 10) in
                                 if t >= 0 then
                                   take t inp ++ [sums !! t] ++ drop (t+2) inp
                                 else
                                   case xs of
                                     (y:ys) -> (x+y):ys
                                     [] -> [0]
  where
    lastOcc [] _ = -1
    lastOcc (x:xs) pred = (if pred x || later >= 0 then (1+) else id) $ later
      where later = lastOcc xs pred

-- Notes:
    -- Thought of zipWith, though didn't know of the function itself
            -- Obviously in standard lib based on Haskell culture, should have looked instead of assuming it wasn't
            -- See also "Hyperfocused on current tools"
    -- Didn't think of making a lazy list to access from (though didn't know of !!)
    -- Didn't think of using "any" and direct retrieval to replace the foldr expression
            -- Hyperfocused on current tools instead of constructing new ones
                    -- May be a consequence of generative search?
                    -- Backwards causality seems safer from that POV, though it has its own issues regarding variety...
            -- Tried to imitate others' apparent design principles rather than using them as fuel for my own, founded models
    -- Thought of lastOcc (after checking tutorial for problem)
    -- Thought of insertion method (though didn't know of !!)
    -- Didn't think of the else/case section (if not >=10, maximize the greatest-order digit)
    -- Forgot to keep considering [Integer] after I switched to using the String representations
            -- Increase short term memory?
            -- Try to make the natural DFS thinking strategy mimic BFS via thresholds?
            -- Further calibrate initial instincts?
                    -- Note: Why was that my first instinct? Why did I switch?
    -- Didn't think of using >=10 instead of direct length-checking
            -- May have been thought of via laziness? "As little dependency as possible" would likely entail minimizing checks to only where the diff was
            -- May have been thought of via iteration on the various subcomponents?
                    -- Might be an offshoot of "clearer mental definition"...
            -- May have been thought of via clearer mental definition of problem, to better clarify paths of inquiry?
                    -- How to make consistent use of clearer definitions viable?
    -- Wanted python-like symmetry too much, hyperfocused on making a single central function, which reduced likelihood of sticking with [Integer]
            -- Take more care to modulate hyperparameters on context
            -- Further unify action models to reduce hyperparameters in favor of direct inputs
            -- Figure out how to achieve the above via mass discretization rather than unification
    -- THE ABOVE IS LIKELY INSUFFICIENT TO DEVELOP THE SOLUTION, but I'm not sure what else I didn't think of
