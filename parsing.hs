import Control.Applicative

newtype ErrorMsg = ErrorMsg String
newtype Parser a = Parser (String -> Either ErrorMsg (a, String))

--A parser consists of a function that reads from a String to produce either an error message or a result of type a and the remaining String that has not been parsed. 
--This parser type does not allow backtracking and is less expressive than the list-based parsers.


helper :: (a->b) -> (Either ErrorMsg (a, String)) -> (Either ErrorMsg (b, String))
helper g (Right (a, str)) = Right (g a, str)
helper g (Left error)     = Left error


instance Functor Parser where
  fmap g (Parser f) = Parser ((helper g).f)


instance Applicative Parser where
  pure f                      = Parser (\str   -> (Right (f, str)))
  (<*>) (Parser g) (Parser f) = Parser (\ s    -> case g s of 
                                (Left error)   -> (Left error)
                                (Right (h, s)) -> case f s of 
                                                  (Left error)   -> (Left error)
                                                  (Right (q, s)) -> (Right (h q, s)) ) 


instance Monad Parser where
 return             = pure
 (>>=) (Parser f) g = do 
                      f <- Parser f
                      g f


instance Alternative Parser where
  empty                       = Parser (\s -> (Left (ErrorMsg s)) )
  (<|>) (Parser f) (Parser g) = Parser (\s -> case f s of
                                       (Left error)   -> case g s of
                                                         (Left error)   -> (Left error)
                                                         (Right (b, s)) -> (Right (b, s))
                                       (Right (a, s)) -> (Right (a, s)) )


-- Describe the Parser type as a series of monad transformers.
-- type Parser a = SomeMonadT <some set of monads and types>