module Common.SourcePosition where

data SourcePosition = SourcePosition
  { line :: Int
  , col  :: Int
  } deriving (Eq)

instance Ord SourcePosition where
  compare a b = compare (line a, col a) (line b, col b)

instance Show SourcePosition where
  show p = "line: " ++ show (line p) ++ ", col: " ++ show (col p)

data SourceLocation = SourceLocation
  { start :: SourcePosition
  , stop  :: SourcePosition
  } deriving (Show, Eq)

data WithSourceLocation a = WithSourceLocation
  { loc   :: SourceLocation
  , value :: a
  } deriving (Eq)

class HasSourceLocation a where
  getSourceLoc :: a -> SourceLocation
