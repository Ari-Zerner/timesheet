-- inspired by https://dev.to/kakkun61/the-simplest-monadfail-instance-2i4e
-- the package wasn't working with stack so I made my own implementation

module Result where

import Control.Monad (ap)
data Result a = Failure String | Success a

instance Show a => Show (Result a) where
  show (Failure msg) = "Error: " ++ msg
  show (Success x)   = show x

instance Functor Result where
  fmap _ (Failure msg) = Failure msg
  fmap f (Success x)   = Success $ f x

instance Applicative Result where
  pure = Success
  (Failure msg) <*> _ = Failure msg
  (Success f) <*> r   = f <$> r

instance Foldable Result where
  foldr _ b (Failure msg) = b
  foldr f b (Success x)   = f x b

instance Traversable Result where
  sequenceA (Failure msg) = pure $ Failure msg
  sequenceA (Success x)   = Success <$> x

instance Monad Result where
  Failure msg >>= _ = Failure msg
  Success x   >>= f = f x

instance MonadFail Result where
  fail = Failure

isSuccess :: Result a -> Bool
isSuccess (Failure _) = False
isSuccess (Success _) = True

withDefault :: a -> Result a -> a
withDefault d (Failure _) = d
withDefault _ (Success x) = x

noOpOnFail :: (a -> Result a) -> a -> a
noOpOnFail = ap withDefault

resultFromMaybe :: String -> Maybe a -> Result a
resultFromMaybe msg = maybe (Failure msg) Success

maybeFromResult :: Result a -> Maybe a
maybeFromResult = withDefault Nothing . fmap Just

forceResult :: Result a -> IO a
forceResult (Failure msg) = fail msg
forceResult (Success x)   = return x