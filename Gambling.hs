
import Control.Applicative 
import Control.Monad       
import Data.Foldable      
import Data.Monoid         
import Data.Ratio          
import System.Random       


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


instance MonadGamble IO where
  toss = randomIO 
  roll = randomIO


count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


-- the game itself
game :: MonadGamble m => m OutCome
game = do
    coins <- replicateM 6 toss
    dRes <- roll
    if ((1 + (fromEnum dRes)) >= (count (== H) coins)) then return Win else return Lose


-- simulation of the game
simulate :: IO OutCome -> Integer -> IO Rational
simulate g n = do
  ress     <- replicateM (fromIntegral n) g
  return $ toInteger (count (== Win) ress) % toInteger (length ress)


data DecisionTree a = Result a | Decision [DecisionTree a]


instance Functor DecisionTree where
  fmap = liftM


instance Applicative DecisionTree where
  pure = return
  (<*>) = ap


instance Monad DecisionTree where
  return a                  = Result a
  (Result a)          >>= f = f a
  (Decision treelist) >>= f = Decision (map (>>= f) treelist) 


instance MonadGamble DecisionTree where
  toss = Decision [Result H, Result T]
  roll = Decision [Result D1, Result D2, Result D3, Result D4, Result D5, Result D6]


-- 'counting' decision tree 
listTree :: DecisionTree a -> [DecisionTree a]
listTree (Result a) = [Result a]
listTree (Decision xs) = Prelude.concatMap listTree xs


probabilityOfWinning :: DecisionTree OutCome -> Rational
probabilityOfWinning g =
   ( toInteger (count ( \(Result x) -> x == Win) (listTree g) ) % toInteger (length (listTree g) ) )   
   