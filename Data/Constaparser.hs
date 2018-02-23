{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

{-# OPTIONS_GHC -O2 #-}

module Data.Constaparser 
  ( Constaparser(..)
  , ConstaparserST(..)
  , constaparserToAttoparsec
  , withMutableVector
  , cpInt 
  , dropTrailingSpace 
  ) where

import Control.Applicative
import Control.Monad.ST
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Constaparser a = Constaparser
  {-# UNPACK #-} !Int
  (ByteString -> Either String a)
  deriving (Functor)

instance Applicative Constaparser where
  pure a = Constaparser 0 (const (Right a))
  Constaparser n f1 <*> Constaparser m f2 = Constaparser (n + m) $
    \bs -> do
      let (bs1,bs2) = BC.splitAt n bs
      func <- f1 bs1
      val <- f2 bs2
      Right (func val)

data ConstaparserST s e a = ConstaparserST
  {-# UNPACK #-} !Int
  (e -> ByteString -> ST s (Either String a))
  deriving (Functor)

instance Applicative (ConstaparserST s e) where
  pure a = ConstaparserST 0 (\_ _ -> pure (Right a))
  ConstaparserST n f1 <*> ConstaparserST m f2 = ConstaparserST (n + m) $
    \e bs -> do
      let (bs1,bs2) = BC.splitAt n bs
      efunc <- f1 e bs1
      case efunc of
        Left err -> pure (Left err)
        Right func -> do
          eval <- f2 e bs2
          case eval of
            Left err -> pure (Left err)
            Right val -> pure (Right (func val))

constaparserToAttoparsec :: Constaparser a -> Parser a
constaparserToAttoparsec (Constaparser n f) = do
  bs <- AB.take n
  case f bs of
    Left err -> fail ("constaparser failed: " ++ err)
    Right a -> pure a

withMutableVector
  :: Int 
  -> (forall s. ConstaparserST s (MVector s a) ())
  -> Constaparser (Vector a)
withMutableVector sz cp@(ConstaparserST n _) = Constaparser n $
  \bs -> runST $ do
    case cp of
      ConstaparserST _ f -> do
        mv <- MV.new sz
        e  <- f mv bs
        case e of
          Left err -> pure (Left err)
          Right _ -> do
            v <- V.unsafeFreeze mv
            pure (Right v)

cpInt :: String -> Int -> Constaparser Int
cpInt note n = Constaparser n $
  \bs -> case BC.readInt (dropTrailingSpace bs) of
    Nothing -> Left (note ++ " string was: " ++ BC.unpack bs)
    Just (i, remaining) -> if BC.null remaining
      then Right i
      else Left note

dropTrailingSpace :: ByteString -> ByteString
dropTrailingSpace = fst . BC.spanEnd (== ' ')
