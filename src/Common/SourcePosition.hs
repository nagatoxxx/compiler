{-# LANGUAGE CPP #-}

module Common.SourcePosition where

data SourcePosition = SourcePosition
  { line :: Int
  , col  :: Int
  } deriving (Eq)

instance Ord SourcePosition where
  compare :: SourcePosition -> SourcePosition -> Ordering
  compare a b = compare (line a, col a) (line b, col b)

instance Show SourcePosition where
  show :: SourcePosition -> String
  show p = "line: " ++ show (line p) ++ ", col: " ++ show (col p)

data SourceLocation = SourceLocation
  { start :: SourcePosition
  , stop  :: SourcePosition
  } deriving (Show, Eq)

instance Semigroup SourceLocation where
  (<>) :: SourceLocation -> SourceLocation -> SourceLocation
  (<>) = merge

instance Monoid SourceLocation where
  mempty :: SourceLocation
  mempty = SourceLocation (SourcePosition 0 0) (SourcePosition 0 0)

data WithSourceLocation a = WithSourceLocation
  { loc   :: SourceLocation
  , value :: a
  } deriving (Eq)

instance Functor WithSourceLocation where
  fmap :: (a -> b) -> WithSourceLocation a -> WithSourceLocation b
  fmap f (WithSourceLocation loc a) = WithSourceLocation loc (f a)

instance Applicative WithSourceLocation where
  pure :: a -> WithSourceLocation a
  pure a = WithSourceLocation mempty a

  (<*>) :: WithSourceLocation (a -> b) -> WithSourceLocation a -> WithSourceLocation b
  WithSourceLocation loc1 f <*> WithSourceLocation loc2 a
    = WithSourceLocation (loc1 <> loc2) (f a)

instance (Show a) => Show (WithSourceLocation a) where
  show :: (WithSourceLocation a) -> String
#ifdef DEBUG
  show (WithSourceLocation l e) = show e ++ " @ " ++ show l
#else
  show (WithSourceLocation _ e) = show e
#endif

merge :: SourceLocation -> SourceLocation -> SourceLocation
merge a b = SourceLocation (start a) (stop b)

(@@) :: a -> SourceLocation -> (WithSourceLocation a)
t @@ loc = WithSourceLocation loc t
