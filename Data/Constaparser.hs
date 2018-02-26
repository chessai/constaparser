{-# OPTIONS_GHC -O2 -Wall #-}

module Data.Constaparser 
  ( Constaparser
  , ConstaparserST
  , constaparserToAttoparsec
  , withMutableVector
  , cpInt 
  , dropTrailingSpace 
  ) where

import Data.Constaparser.Internal
