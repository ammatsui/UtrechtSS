-- Toss a coin six times and count the number of heads, then roll a dice; 
-- if the number of eyes on the dice is greater than or equal to the number of heads then win, else lose. 


import Control.Applicative 
import Control.Monad       
import Data.Foldable      
import Data.Monoid         
import Data.Ratio          
import System.Random       


-- Exercise 2: Give Random instances for Coin and Dice.

data Coin = H | T deriving (Bounded, Eq, Enum, Ord, Show)


instance Random Coin where
  random = random
  randomR (l, h) g = let (a, g') = randomR (fromEnum l, fromEnum h) g in (toEnum a, g')


data Dice = D1 | D2 | D3 | D4 | D5 | D6 deriving (Bounded, Eq, Enum, Ord, Show)


instance Random Dice where
  random = random
  randomR (l, h) g = let (a, g') = randomR (fromEnum l, fromEnum h) g in (toEnum a, g')


data OutCome = Win | Lose deriving (Eq, Ord, Show)


class Monad m => MonadGamble m where
  toss :: m Coin
  roll :: m Dice


-- Exercise 3: Give a MonadGamble instance for the IO monad.

instance MonadGamble IO where
  toss = randomIO 
  roll = randomIO


count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


-- Exercise 1: Write a function game :: MonadGamble m => m Outcome that implements the game above.

game :: MonadGamble m => m OutCome
game = do
    coins <- replicateM 6 toss
    dRes <- roll
    if ((1 + (fromEnum dRes)) >= (count (== H) coins)) then return Win else return Lose


-- Exercise 4: Write a function that runs a game of chance (given as the first parameter, not necessarily this game) 
-- n times (n > 0, the second parameter) and returns the fraction of games won.

-- simulation of a game
simulate :: IO OutCome -> Integer -> IO Rational
simulate g n = do
  ress     <- replicateM (fromIntegral n) g
  return $ toInteger (count (== Win) ress) % toInteger (length ress)


data DecisionTree a = Result a | Decision [DecisionTree a]


-- Exercise 5: Give a Monad instance for DecisionTree. 
-- Hint: use the types of (>>=) and return for guidance: 
-- it’s the most straightforward, type-correct definition that isn’t an infinite loop.

instance Functor DecisionTree where
  fmap = liftM


instance Applicative DecisionTree where
  pure = return
  (<*>) = ap


instance Monad DecisionTree where
  return a                  = Result a
  (Result a)          >>= f = f a
  (Decision treelist) >>= f = Decision (map (>>= f) treelist) 


-- Exercise 6: Give a MonadGamble instance for DecisionTree.

instance MonadGamble DecisionTree where
  toss = Decision [Result H, Result T]
  roll = Decision [Result D1, Result D2, Result D3, Result D4, Result D5, Result D6]


-- 'counting' decision tree 
listTree :: DecisionTree a -> [DecisionTree a]
listTree (Result a) = [Result a]
listTree (Decision xs) = Prelude.concatMap listTree xs

-- Exercise 7: Write a function that, given a decision tree, computes the probability of winning.

probabilityOfWinning :: DecisionTree OutCome -> Rational
probabilityOfWinning g =
   ( toInteger (count ( \(Result x) -> x == Win) (listTree g) ) % toInteger (length (listTree g) ) )   
   
