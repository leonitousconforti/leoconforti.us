---

title: Implementing Unsure Calculator in 100 lines of Haskell

description: Implementing a calculator with normal ranges, embedded in Haskell, using a simplified probability monad.

tags: haskell, probability

---

# Unsure Calculator

The recently trendy [Unsure Calculator](https://filiph.github.io/unsure/) makes
reasoning about numbers with some uncertainty just as easy as calculating with
specific numbers.

The key idea is to add a new "range" operator (written `~`) to the vocabulary
of a standard calculator. The range `x~y` denotes that a real value is uncertain, but
we are 95% sure that it falls between `x` and `y`^[More precisely, the range denotes a normal distribution where each end is two standard deviations from the mean.].

> Reading the notation is easy: when you see 10~15, you say: "ten to fifteen".

Arithmetic operations and friends (e.g. `sin`, or `log`)
transparently operate on ranges and literal numbers alike.
Calculation results in a plot with a range of values that
the input expression can take, and with what frequency.


<!--```txt-->
<!--     above | ▒▒▒▒-->
<!--       236 | ▒▒▒-->
<!--       221 | ▒▒▒▒-->
<!--       205 | ▒▒▒▒▒▒-->
<!--       190 | ▒▒▒▒▒▒▒▒▒-->
<!--       175 | ▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--       159 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--       144 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--       129 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--       114 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--        98 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--        83 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ (79)-->
<!--        68 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--        52 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--        37 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--        22 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--         6 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--        -9 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒-->
<!--       -24 | ▒▒▒▒▒▒▒▒▒▒-->
<!--       -39 | ▒▒▒▒▒▒▒-->
<!--       -55 | ▒▒▒▒▒-->
<!--       -70 | ▒▒▒-->
<!--     below | ▒▒▒▒-->
<!--```-->

The motivation behind the original article is neat, so I'll just recommend you
read it [there](https://filiph.github.io/unsure/) to learn how and why you'd use such a calculator. Here's a
real life example they used:

```txt
1400~1700 * 0.55~0.65 - 600~700 - 100~200 - 30 - 20
```

Now, let's implement it.

## Sampling it up

Summon a [probability monad](https://www.youtube.com/watch?v=qZ4O-1VYv4c) from the void^[[Practical Probabilistic Programming with Monads](https://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf).
Our version is much simpler than the paper's because we don't care about
conditional probabilities and because we only ever use normal distributions.].

```haskell
data Dist a where
  Return :: a -> Dist a
  Bind   :: Dist b -> (b -> Dist a) -> Dist a
  Normal :: Double -> Double -> Dist Double

instance Monad       Dist where (>>=) = Bind
instance Applicative Dist where pure  = Return; (<*>) = ap
instance Functor     Dist where fmap  = liftM
```

The monad instance is free: `pure = Return` and `(>>=) = Bind`. The `Normal`
constructor denotes a normal distribution given the standard deviation and
mean. With do-notation we can easily construct a complex tree mixing `Return`s,
`Bind`s, and `Normal`s. For instance:

```txt
d = do
  s <- Normal 0 1
  return (5 + s)
```

desugars to

```txt
d = Bind (Normal 0 1) (\s -> Return (5 + s))
```

Then, embue meaning onto a `Dist a` by allowing an `a` to be sampled according to the distribution the `Dist` represents.
We use `StdGen` from `random` as a source of uniform pseudo-randomness:

```haskell
sample :: StdGen -> Dist a -> a
sample g d = case d of
  Return x -> x
  Normal mean std_dev -> n1 * std_dev + mean
    where ((u1, u2), _) = uniformR ((0,0), (1,1)) g
          (n1,  _)      = boxMuller u1 u2
  Bind d f -> sample g1 (f (sample g2 d))
    where (g1, g2) = splitGen g
```

Sampling a distribution is simple:

- For a constant `x`, return `x`
- For a normal distribution, use the [Box Muller
  transform](https://en.wikipedia.org/wiki/Box–Muller_transform) to transform a
  pair of *uniformly* distributed random numbers in `[0, 1]` into a pair of
  *normally* distributed random numbers. Then, scale according to the normal
  distribution parameters (`mean`, `std_dev`).
- For `Bind`, sample `a` from `Dist a`, feed it to the continuation, and then sample from the resulting `Dist b`.
  Do make sure the PRN generator is [split](https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random.html#v:splitGen).

Box-Muller is implemented very literally as:
```haskell
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2
```

We can try sampling the example above:
```txt
> sample (mkStdGen 3) (Bind (Normal 0 1) (\s -> Return (5+s)))
5.079952851990287
```

And we can also use `sequence . repeat` to generate an infinite list of samples:
```txt
> take 5 $ sample (mkStdGen 4) $ sequence $ repeat $ (Bind (Normal 0 1) (\s -> Return (5+s)))
[4.133071135842417,5.270868355973887,6.9698645379055355,3.8369680071523447,4.2986629449038425]
```

## Calculator Expressions

Conjure up an eDSL for calculator expressions:

```haskell
data Expr
  = Num Double
  | Add Expr Expr   | Mul Expr Expr
  | Abs Expr        | Signum Expr
  | Negate Expr     | Div Expr Expr
  | Exp Expr        | Log Expr
  | Sin Expr        | Cos Expr
  | Range Expr Expr

(~) = Range

instance Num Expr where
  (+) = Add; (*) = Mul
  negate = Negate; abs = Abs
  signum = Signum; fromInteger = Num . fromInteger

instance Fractional Expr where
  (/) = Div; fromRational = Num . fromRational

instance Floating Expr where
  pi = Num pi; exp = Exp; log = Log; sin = Sin; cos = Cos
```

By defining `~` to be `Range` and overloading math we can immediately write
```txt
1400~1700 * 0.55~0.65 - 600~700
```
to mean
```txt
Add (Mul (Range 1400 1700) (Range 0.55 0.65)) (Negate (Range 600 700)))
```

Breathe life into expressions by allowing them to be evaluated to a distribution
that can later be sampled. This is where the `Functor`, `Applicative`, and `Monad`
instances for `Dist` come into play:

```haskell
eval :: Expr -> Dist Double
eval e = case e of
  Num d       -> return d
  Add e1 e2   -> (+) <$> eval e1 <*> eval e2
  Mul e1 e2   -> (*) <$> eval e1 <*> eval e2
  Negate e    -> negate <$> eval e
  Abs e       -> abs <$> eval e
  Signum e    -> signum <$> eval e
  Div e1 e2   -> (/) <$> eval e1 <*> eval e2
  Exp e       -> exp <$> eval e
  Log e       -> log <$> eval e
  Sin e       -> sin <$> eval e
  Cos e       -> cos <$> eval e
  Range e1 e2 -> do
    a <- eval e1
    b <- eval e2
    let mean = (a + b) / 2
        std_dev = (b - a) / 4
    Normal mean std_dev
```

Most cases just recursively `eval` the sub-expressions into `Dist Double`s and
then lift a math operation on `Double`s into the probability monad.
The interesting case is `Range`, which uses the evaluated sub-expressions, referring to
the lower and upper range bound, to compute the standard deviation and mean of
the normal distribution -- a `Dist` is returned by applying the "primitive"
`Normal` constructor accordingly.

We can take a few samples of the first example in this post:
```txt
> take 10 $ sample (mkStdGen 5) $ sequence $ repeat $ eval (1400~1700 * 0.55~0.65 - 600~700 - 100~200 - 30 - 20)
[8.808397505000045,-20.049171720836654,12.753715226577157,-22.77067243260501,-70.3671725933005,-11.610935890955716,157.612422580839,75.61254061496092,88.04359089198991,-37.280109436085866]
```
Even though we are taking samples from the distribution constructed by that compound expression, we cannot make too much sense of these numbers.
We need to plot them in a histogram, just like the original calculator does, to make this useful.

## Showing up

I'll spare the nitty-gritty details, but, lastly, we define a `Show` instance
for `Expr` that plots a number of samples from the distribution resulting from
the given expression. In order:

- Eval the expression `e` into a `Dist Double`
- Take 25000 samples
- Group each sample into the interval it is closest to (in `collapseIntervals`)
- Compute the ratio of samples in each box over the total number of samples, to display as a percentage
- For each box, plot a line (in `line` and `showRow`)

```haskell
instance Show Expr where
  show e = concatMap showRow intervals where
    samples = take 25000 . sample (mkStdGen 0) . sequence . repeat . eval
    intervals = collapseIntervals 40 (samples e)
    line p = replicate spaces ' ' ++ replicate normalized ':'
      where spaces = 35 - normalized
            normalized = round ((p/max_prob) * 30)
            max_prob = maximum $ map snd intervals
    showRow (elem, prob) = line prob ++ " | " ++ printf "%.1f (%.1f" elem (prob*100) ++ "%)\n"

collapseIntervals :: Int -> [Double] -> [(Double, Double)]
collapseIntervals n (sort -> samples) =
  let (low, high) = (head samples, last samples)
      step  = (high - low) / fromIntegral n
      boxes = [low, low+step .. high] 
      total = fromIntegral (length samples)
   in M.toList $ M.map (/total) $ M.fromListWith (+)
        [ (minimumBy (compare `on` \box -> abs (box - s)) boxes, 1)
        | s <- samples ]
```

Let's try to print out the first example now, by simply typing in GHCi:
```txt
> 1400~1700 * 0.55~0.65 - 600~700 - 100~200 - 30 - 20

                                    | -188.2 (0.0%)
                                    | -162.2 (0.0%)
                                    | -149.2 (0.0%)
                                    | -136.1 (0.1%)
                                    | -123.1 (0.1%)
                                    | -110.1 (0.1%)
                                  : | -97.1 (0.2%)
                                 :: | -84.1 (0.4%)
                                 :: | -71.1 (0.6%)
                               :::: | -58.1 (1.1%)
                            ::::::: | -45.1 (1.7%)
                            ::::::: | -32.1 (1.8%)
                        ::::::::::: | -19.0 (2.9%)
                    ::::::::::::::: | -6.0 (3.8%)
                  ::::::::::::::::: | 7.0 (4.3%)
              ::::::::::::::::::::: | 20.0 (5.3%)
          ::::::::::::::::::::::::: | 33.0 (6.1%)
        ::::::::::::::::::::::::::: | 46.0 (6.7%)
     :::::::::::::::::::::::::::::: | 59.0 (7.4%)
     :::::::::::::::::::::::::::::: | 72.0 (7.5%)
     :::::::::::::::::::::::::::::: | 85.0 (7.5%)
       :::::::::::::::::::::::::::: | 98.0 (7.0%)
         :::::::::::::::::::::::::: | 111.1 (6.6%)
            ::::::::::::::::::::::: | 124.1 (5.8%)
              ::::::::::::::::::::: | 137.1 (5.3%)
                  ::::::::::::::::: | 150.1 (4.3%)
                      ::::::::::::: | 163.1 (3.3%)
                       :::::::::::: | 176.1 (2.9%)
                          ::::::::: | 189.1 (2.2%)
                            ::::::: | 202.1 (1.7%)
                              ::::: | 215.1 (1.2%)
                                ::: | 228.2 (0.8%)
                                 :: | 241.2 (0.6%)
                                  : | 254.2 (0.3%)
                                  : | 267.2 (0.2%)
                                    | 280.2 (0.1%)
                                    | 293.2 (0.0%)
                                    | 306.2 (0.0%)
                                    | 319.2 (0.0%)
                                    | 332.2 (0.0%)
```

Success! This looks very similar to the original histogram for the same expression.
Let's also try Drake's equation as described in the original article:

```txt
> 1.5~3 * 0.9~1.0 * 0.1~0.4 * 0.1~1.0 * 0.1~1.0 * 0.1~0.2 * 304~10000

                                    | -307.0 (0.0%)
                                    | -193.4 (0.0%)
                                    | -155.5 (0.0%)
                                    | -117.7 (0.0%)
                                    | -79.8 (0.1%)
                                  : | -42.0 (0.6%)
                       :::::::::::: | -4.1 (8.3%)
     :::::::::::::::::::::::::::::: | 33.7 (20.9%)
        ::::::::::::::::::::::::::: | 71.6 (19.1%)
              ::::::::::::::::::::: | 109.4 (14.6%)
                    ::::::::::::::: | 147.3 (10.5%)
                        ::::::::::: | 185.2 (7.4%)
                           :::::::: | 223.0 (5.3%)
                              ::::: | 260.9 (3.6%)
                               :::: | 298.7 (2.6%)
                                ::: | 336.6 (1.9%)
                                 :: | 374.4 (1.4%)
                                  : | 412.3 (0.9%)
                                  : | 450.1 (0.7%)
                                  : | 488.0 (0.5%)
                                    | 525.9 (0.3%)
                                    | 563.7 (0.3%)
                                    | 601.6 (0.2%)
                                    | 639.4 (0.1%)
                                    | 677.3 (0.1%)
                                    | 715.1 (0.1%)
                                    | 753.0 (0.1%)
                                    | 790.8 (0.0%)
                                    | 828.7 (0.0%)
                                    | 866.5 (0.0%)
                                    | 904.4 (0.0%)
                                    | 942.3 (0.0%)
                                    | 980.1 (0.0%)
                                    | 1018.0 (0.0%)
                                    | 1055.8 (0.0%)
                                    | 1093.7 (0.0%)
                                    | 1169.4 (0.0%)
                                    | 1207.2 (0.0%)
```

That's it!

## Conclusion

A few final remarks:

- This is a neat and simple implementation. It highlights some of Haskell's strengths in an interesting task.
- Since our calculator implementation is embeded in Haskell, we
  can leverage the GHCi REPL -- most importantly, we can do variable binding and
  define functions with no additional implementation cost.
- You can try the [full program](https://gist.github.com/alt-romes/58346dee164a91ddab48c5a111d1b5ea) (exactly 100 lines) on your machine by loading it with `cabal repl <file.hs>` and typing unsure expressions at will.

<!--- This could almost certainly use a closed form implementation -->
<!--- It'd be good to not display too many 0.0% boxes -->

