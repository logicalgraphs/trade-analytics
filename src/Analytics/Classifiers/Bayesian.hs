module Analytics.Classifiers.Bayesian where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bag hiding (merge)     -- http://lpaste.net/107815

-- A solution to the problem posted at http://lpaste.net/108901

-- http://lpaste.net/108916

{--

Our training set is of the form:

Class,Value1,Value2,...

where values are defined in ¤7 of letter-recognition.names

 --}

type Prob = [Double] -- there are 16 characteristics 
                     -- ... so, ya know: ... Vector 16? *shrugs*

type Class = Char

data Known = Know { each :: Map Class Prob,
                    totes :: Prob,
                    trainedRows :: Int }
   deriving Show

dnada :: Known
dnada = Know Map.empty (replicate 16 0.0) 0

data Row = R Class Prob
type RowPair = (Class, Prob)

pair :: Row -> RowPair
pair (R c p) = (c, p)

instance Read Row where
   readsPrec _ line = [(ans, "")]
      where ans  = R (head line) list
            list = map fromInteger (read ('[' : tail (tail line) ++ "]"))

merge :: Prob -> Prob -> Prob
merge a b = zipWith (+) a b

updateClass :: Ord k => k -> Prob -> Map k Prob -> Map k Prob
updateClass k v map = Map.insertWith merge k v map

train :: FilePath -> IO Known
train trainingData = readFile trainingData >>=
   return . foldr (\(k,v) (Know map total rows) -> 
                    Know (updateClass k v map) (merge total v) (rows + 1))
                  dnada 
          . map (pair . read) . lines

-- so I did a training set on lr.training, which is the first 250 rows
-- of the 20,000 rows of sampled data

-- our classifier: from a knowledge base classifies a new (unknown) input

scale :: Prob -> Integer -> Prob
scale row rowCountIGNORE = map (/ ratio) row
   where sumer = sum row
         -- sumtotes = sum totes
         ratio = sumer  -- * fromInteger rowCount -- scaling the scale? BAD!

squareDiff :: Num a => a -> a -> a
squareDiff a b = (a - b) * (a - b)

distance :: Prob -> Prob -> Double
distance a b = sqrt $ sum $ zipWith squareDiff a b

classify :: Known -> Prob -> Class
classify (Know eaches totoes cnt) unclassifiedRow =
   c' (Map.assocs eaches) (scale unclassifiedRow 1) ('A', 1.0) (toInteger cnt)

{--

So, this is ... 'interesting.' All the data are present for all the classes
it's just a matter of degree per class, so this is a bit more than na•ve
bayes in that we use a distance function instead of probability-of-presence
measure and the NEAREST distance (that is, closest to zero) is the winner
instead of, as per usual-na•ve, the HIGHEST probability (that is, closest 
to one) is the winner.

Hm. Live and learn.

 --}

      where c' [] _ (cls, _) _ = cls
            c' ((newcls, cnts): rest) vect old@(cls, dist) t = 
               let newdist = distance vect (scale cnts t)
                   newset = (newcls, newdist)
                   newguess = if newdist < dist then newset else old 
               in  c' rest vect newguess t

{--

Some samples for a pulse-check on the system:

*Algorithmic.Bayesian> let ont = train "letters/lr.training"
*Algorithmic.Bayesian> ont >>= return . flip classify
                       [7,9,10,8,11,6,6,6,5,5,5,8,11,11,9,12] -- 'W'
'M' -- WRONG! -_-
*Algorithmic.Bayesian> ont >>= return . flip classify
                       [1,9,0,6,0,7,7,4,4,7,6,8,0,8,0,8] -- 'I'
'I' -- Right! ^^)
*Algorithmic.Bayesian> ont >>= return . flip classify
                       [4,7,5,5,3,8,6,8,5,10,6,9,3,8,3,8] -- should be 'O'
'O' -- Right! ^^)
*Algorithmic.Bayesian> ont >>= return . flip classify
                       [4,7,6,5,5,7,8,5,5,9,6,6,3,7,6,7] -- should be 'B'
'B' -- Right! ^^)

So, very small training set, very small sample set, but encouraging results,
none-the-less.

So, the question arises: do we increase our sample size to make our Bayesian
system smarter at the get-go (sounds good)...

OOOOOORRRRRR!

Since we have classification information, do we provide a feedback loop for
our classifier, eh? Should we do that? Huh? Should we? Ya think?

 --}

-- first we'll see the results of static knowledge (a baseline of sorts)

data Wrong = Nope { expected :: Char, actual :: Char }
   deriving (Eq, Ord, Show)

type RightnWrong = (Bag Char, Bag Wrong)

go :: FilePath -> FilePath -> Bool -> IO RightnWrong
go trainingSet testSet learn = train trainingSet >>= \kb@(Know _ _ zonk) ->
   snarf testSet >>= return . 
      (if learn then feed kb else foldr (divvy kb)) starter . drop zonk
          where starter = (emptyBag, emptyBag)

divvy :: Known -> RowPair -> RightnWrong -> RightnWrong
divvy kb row@(expected, vect) (rights, wrongs) =
   let actual = classify kb vect
   in  if expected == actual then (add actual rights, wrongs)
       else (rights, add (Nope expected actual) wrongs)

feed :: Known -> RightnWrong -> [RowPair] -> RightnWrong
feed _ ans [] = ans
feed kb@(Know m t l) bags (row@(cls, prob):rows) =
   let newbags = divvy kb row bags
       newkb   = Know (updateClass cls prob m) t l
   in  feed newkb newbags rows

snarf :: FilePath -> IO [RowPair]
snarf testSet = readFile testSet >>=  return . map (pair . read) . lines

{--

Okay, our results.

*Algorithmic.Bayesian Control.Arrow> let bags =
   go "letters/lr-250-rows.training" 
      "letters/letter-recognition.data.txt" False

wrongs: 

*Algorithmic.Bayesian Control.Arrow> bags >>= return . snd ~> ... etc
(Nope {expected = 'Z', actual = 'Q'},21),
(Nope {expected = 'Z', actual = 'R'},3),
(Nope {expected = 'Z', actual = 'S'},112),
(Nope {expected = 'Z', actual = 'X'},91)]
*Algorithmic.Bayesian Control.Arrow> bags >>= return . length . toList . snd
11480

rights:

*Algorithmic.Bayesian Control.Arrow> bags >>= return . toAssocList . fst ~> ...
[('A',624),('B',288),('C',305),('D',304),('E',234),('F',337),('G',200),
 ('H',74),('I',426),('J',318),('K',206),('L',422),('M',407),('N',346),
 ('O',299),('P',350),('Q',260),('R',324),('S',215),('T',493),('U',293),
 ('V',361),('W',529),('X',175),('Y',172),('Z',308)]
*Algorithmic.Bayesian Control.Arrow> bags >>= return . length . toList . fst
8270

... so this begs the question, did we 'guess' 2 out of 5 correctly?

Let's try increasing the size of our training set from 250 rows to 1000 rows 
to see how that affects our results.

*Algorithmic.Bayesian Control.Arrow> let bags =
   go "letters/lr-1000-rows.training"
      "letters/letter-recognition.data.txt" False

*Algorithmic.Bayesian Control.Arrow> bags >>= return . length . toList . fst
9423
*Algorithmic.Bayesian Control.Arrow> bags >>= return . length . toList . snd
9577

... So that means with a training set of 1/20th of the data (5%) we have an
even split of good verses bad guesses.

Eh.

One more training set. A biggie this time:

*Algorithmic.Bayesian Control.Arrow> let bags =
   go "letters/lr-5krows.training"
      "letters/letter-recognition.data.txt" False
-- training took no time
*Algorithmic.Bayesian Control.Arrow> bags >>= return . length . toList . fst
8137
*Algorithmic.Bayesian Control.Arrow> bags >>= return . length . toList . snd
6863

So, really just a slight improvement, but still around an even split.

Now, the question is that we already have the data classified. Will 
in-process feedback give us better results. By 'better results' I mean:
'MUCH better results'? Let's see.

*Algorithmic.Bayesian> let bags = go "letters/lr-250-rows.training" 
             "letters/letter-recognition.data.txt" True
*Algorithmic.Bayesian> bags >>= return . length . toList . fst
10659
*Algorithmic.Bayesian> bags >>= return . length . toList . snd
9091

... Okay, that is better, but perhaps unrealistic in that we may not have
classification provided for us in the data set we are processing. But what
if we scale along the scope of the entire data set? That is: factor in the
totes information as we scale our sets to classify?

BLEH! Scaling just makes things much worse! :( SHOOT!

*Algorithmic.Bayesian> let bags = go "letters/lr-250-rows.training" "letters/letter-recognition.data.txt" False
*Algorithmic.Bayesian> bags >>= return . length . toList . fst
3865
*Algorithmic.Bayesian> bags >>= return . length . toList . snd
15885

... and learning while scaling only improves result slightly:

*Algorithmic.Bayesian> let bags = go "letters/lr-250-rows.training" "letters/letter-recognition.data.txt" True
*Algorithmic.Bayesian> bags >>= return . length . toList . fst
4506
*Algorithmic.Bayesian> bags >>= return . length . toList . snd
15244

So, we'll revert to letting Bayes do what it do!

So it seems either feedback-learning with a small seed OR a larger seed,
such as 1000+ training rows is the best approach. 

 --}

-- TODO: post Alternate exercise solution
