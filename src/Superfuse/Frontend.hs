module Superfuse.Frontend where

import Data.Int
import Data.Kind
import Data.Word
import GHC.TypeLits

-- | Signed integral types
data IType = I8 | I16 | I32 | I64

-- | Unsigned integral types
data WType = W16 | W32 | W64

-- | Floating-point types
data FType = F32 | F64

-- | Scalar types
data SType = SIType IType | SWType WType | SFType FType

-- | Scalar types' corresponding Haskell types
type family HsSType t = h | h -> t where
    HsSType ('SIType 'I8) = Int8
    HsSType ('SIType 'I16) = Int16
    HsSType ('SIType 'I32) = Int32
    HsSType ('SIType 'I64) = Int64
    HsSType ('SWType 'W16) = Word16
    HsSType ('SWType 'W32) = Word32
    HsSType ('SWType 'W64) = Word64
    HsSType ('SFType 'F32) = Float
    HsSType ('SFType 'F64) = Double

-- | Scalar binary operators. The Bool indicates whether it satisfies monoid laws.
data SBinOp :: SType -> Bool -> Type where
    Add, Mul :: SBinOp t 'True
    Sub, Div :: SBinOp t 'False

-- | Vector shapes
type VShape = [Nat]

-- | Vector expressions
data VExpr :: SType -> VShape -> Type where
    SLift :: HsSType t -> VExpr t '[]
    SCopy :: VExpr t '[] -> VExpr t sh
    VBinApp :: SBinOp t f -> VExpr t sh -> VExpr t sh -> VExpr t sh
    VFold :: SBinOp t 'True -> VExpr t sh -> VExpr t '[]
