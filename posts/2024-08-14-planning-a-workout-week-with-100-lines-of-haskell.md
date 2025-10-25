---

title: Planning Weekly Workouts in 100 lines of Haskell

description: A lightning post on logic programming in Haskell to construct a
             workout weekly schedule given the set of exercises, days and constraints.

tags: haskell

---

I have recently started doing some outdoors bodyweight workouts.
I also want to start running again, but I'm recovering from a minor knee injury
until the start of next month.

Tonight I decided to put together a weekly schedule to start following next
month. The first pen and paper versions were fine, but I wasn't completely
satisfied. The next logical step was to write a quick program to see what
possible plans I was missing.

The schedule must satisfy a few constraints, but the core of it is that I should
do, every week, on one axis, one *short run* (high-intensity) and one *long run*
(long distance), and, on the other axis, have two *pull days* (as in pull-ups),
two *push days* (as in push-ups), and two *leg days* (as in squats).

Finding a weekly workout that satisfies certain constraints is an
answer-set-programming kind of problem, best solved by some kind of logic
programming. Rather than turning to Prolog or
[Clingo](https://potassco.org/clingo/), I decided to just stick to Haskell and
use the logic-programming monad from [logict](https://hackage.haskell.org/package/logict)!

# A workout planner in 100 lines of Haskell

What follows is mostly just the demonstration of using `logict` applied to this
particular problem. I believe the `weeklySchedule` function can be easily understood
in general, even by anyone unfamiliar with Haskell and/or logic programming --
and that's the meat of this short post and program.

Note that the program is a
[cabal](https://cabal.readthedocs.io/en/latest/index.html) shell script which
can be run by executing the script file (as in `./ScheduleExercise`, as long as
`cabal` is in path). It is standalone, and exactly 100 lines (with comments,
shebangs and everything). Feel free to try and modify it!

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base, logict
-}

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Data.List
import Data.Maybe

workout = ["Push day", "Pull day", "Leg day", "No workout"]
running = ["Long run", "Short run", "No run"]
weekdays = ["Seg.", "Ter.", "Qua.", "Qui.", "Sex.", "Sab.", "Dom."]

weeklySchedule :: Logic [(String, [String])]
weeklySchedule = do
  -- For every weekday, pick an element from `workout` and one from `running`
  -- that satisfy the "nested" conditions
  p <- forM weekdays $ \d -> do
    w <- choose workout
    r <- choose running

    -- No running on leg day
    w == "Leg day" ==> r == "No run"

    -- Short intervals run is after an outdoor pull/push workout
    r == "Short run" ==> w /= "No workout"

    -- Workout on Monday outdoors always, not legs
    d == "Seg." ==> w /= "No workout"
    d == "Seg." ==> w /= "Leg day"

    -- Pull day during the week?
    w == "Pull day" ==> (d /= "Sab." && d /= "Dom.")

    pure [w,r]

  -- Now, pick the set `p` of (weekdays X exercises) that satisfy the following conditions:

  -- One long run, one short run
  exactly 1 "Long run" p
  exactly 1 "Short run" p

  -- Two push, two pull, two leg
  exactly 2 "Push day" p
  exactly 2 "Pull day" p
  exactly 2 "Leg day" p

  -- Long run on weekend
  onDay "Long run" "Sab." p <|> onDay "Long run" "Dom." p

  -- Run spaced out at least 2 days
  daysBetween 2 "Short run" "Long run" p
  daysBetween 2 "Long run" "Short run" p

  -- Space out workouts at least 2 days
  spacedOut "Push day" 2 p
  spacedOut "Pull day" 2 p
  spacedOut "Leg day" 2 p

  -- No leg day before short run
  daysBetween 1 "Leg day" "Short run" p
  -- No leg day before a long run
  daysBetween 1 "Leg day" "Long run"  p

  -- At least one of the runs without a leg day after please
  daysBetween 1 "Short run" "Leg day" p <|> daysBetween 1 "Long run" "Leg day" p

  return (zip weekdays p)

--------------------------------------------------------------------------------
-- Logic utils

choose = foldr ((<|>) . pure) empty

exactly n s p = guard (length (filter (s `elem`) p) == n)

onDay s d p = guard (s `elem` getDay d p) where
  getDay s p = p !! fromMaybe undefined (elemIndex s weekdays)

-- space out as at least n days
spacedOut a n p = guard (all (>n) dists) where
  dists = zipWith (-) (drop 1 is) is
  is = findIndices (a `elem`) (take 14 $ cycle p {- cycle week around -})

daysBetween n a b p =
  forM_ (take 14 (cycle p) `zip` [1..]) $ \(x,i) -> do
    a `elem` x ==> all (b `notElem`) (take n $ drop i (take 14 (cycle p)))

(==>) a b = guard (not a || b)
infixr 0 ==>

--------------------------------------------------------------------------------
-- Main

printSched = mapM (\(d, ls) -> putStrLn (d ++ " " ++ intercalate ", " ls))

main = do let r = observeAll weeklySchedule
          mapM (const (putStrLn "") <=< printSched) r
```

The heavy lifting is done by the [`logict`](https://hackage.haskell.org/package/logict) package. 
It allows us to consider alternative values as solutions to logic statements.
The possible alternatives are separated by `<|>`, and `observeAll` returns
a list with all valid alternatives. A trivial example:

```haskell
x = pure "a" <|> pure "b" <|> pure "c"
print (observeAll x)

Result: ["a", "b", "c"]
```

If the alternative is `empty`, it is not an answer and therefore not included in the result.
The `guard` combinator takes a boolean and returns `empty` if it is false,
therefore constraining the solution.

```haskell
x = pure "a" <|> empty <|> pure "c"
print (observeAll x)

Result: ["a", "c"]
```

Finally, we can use the monadic `do` notation to combine sets of alternatives.
Put together with `guard`, we can write slightly more interesting programs:
```haskell
x = do
  y <- pure "a" <|> pure "b" <|> pure "c"
  z <- pure "a" <|> pure "b" <|> pure "c"
  guard (y /= z)
  return (y,z)
print (observeAll x)

Result: [("a","b"),("a","c"),("b","a"),("b","c"),("c","a"),("c","b")]
```

All in all, the body of `weeklySchedule` uses just these principles plus a few
domain-specific combinators I wrote in the `Logic utils` section of the code
(which themselves use also only these principles from `logict`, plus some
laziness-coolness). The program entry point (`main`) prints out the
schedule.

And by the way, these are the workout schedules satisfying all those constraints:

```txt
Seg. Pull day, No run
Ter. Push day, Short run
Qua. No workout, No run
Qui. Leg day, No run
Sex. Pull day, No run
Sab. Push day, Long run
Dom. Leg day, No run

Seg. Pull day, No run
Ter. Leg day, No run
Qua. Push day, No run
Qui. Pull day, Short run
Sex. Leg day, No run
Sab. Push day, No run
Dom. No workout, Long run

Seg. Pull day, No run
Ter. Leg day, No run
Qua. Push day, No run
Qui. Pull day, Short run
Sex. Leg day, No run
Sab. No workout, No run
Dom. Push day, Long run
```
