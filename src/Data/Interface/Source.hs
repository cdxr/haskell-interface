module Data.Interface.Source where


data Source = Source !FilePath !SrcSpan
    deriving (Show, Read, Eq, Ord)

data SrcSpan = SrcSpan
    {-# UNPACK #-} !SrcLoc  -- ^ start source location
    {-# UNPACK #-} !SrcLoc  -- ^ one past end source location
    deriving (Show, Read, Eq, Ord)

data SrcLoc = SrcLoc
    {-# UNPACK #-} !Int  -- ^ source line
    {-# UNPACK #-} !Int  -- ^ source column
    deriving (Show, Read, Eq, Ord)
