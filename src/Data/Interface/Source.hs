module Data.Interface.Source where


-- | The origin of a declaration
data Origin
    = WiredIn              -- ^ built into the compiler
    | UnknownSource        -- ^ source location is unknown
    | KnownSource !Source  -- ^ the known source
    deriving (Show, Read, Eq, Ord)

data Source = Source !FilePath !SrcSpan
    deriving (Show, Read, Eq, Ord)

data SrcSpan = SrcSpan
    {-# UNPACK #-} !SrcLoc  -- start source location
    {-# UNPACK #-} !SrcLoc  -- one past end source location
    deriving (Show, Read, Eq, Ord)

data SrcLoc = SrcLoc
    {-# UNPACK #-} !Int  -- source line
    {-# UNPACK #-} !Int  -- source column
    deriving (Show, Read, Eq, Ord)
