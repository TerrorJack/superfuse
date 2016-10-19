module Superfuse.Frontend where

import Data.Int
import Data.Kind
import Data.Word
import GHC.TypeLits

-- | Signed integral types
data IType = I8 | I16 | I32 | I64

-- | Unsigned integral types
data UType = U8 | U16 | U32 | U64

-- | Floating-point types
data FType = F32 | F64

-- | Scalar types
data SType = SIType IType | SUType UType | SFType FType

-- | Scalar types' corresponding Haskell types
type family HsSType t = h | h -> t where
    HsSType ('SIType 'I8)  = Int8
    HsSType ('SIType 'I16) = Int16
    HsSType ('SIType 'I32) = Int32
    HsSType ('SIType 'I64) = Int64
    HsSType ('SUType 'U8)  = Word8
    HsSType ('SUType 'U16) = Word16
    HsSType ('SUType 'U32) = Word32
    HsSType ('SUType 'U64) = Word64
    HsSType ('SFType 'F32) = Float
    HsSType ('SFType 'F64) = Double

-- | Scalar unary operators.
data SUnaryOp :: SType -> Type where
    Neg :: SUnaryOp t
    Abs :: SUnaryOp t

-- | Scalar binary operators. The Bool indicates whether it satisfies monoid laws.
data SBinOp :: SType -> Bool -> Type where
    Add, Mul :: SBinOp t 'True
    Sub, Div :: SBinOp t 'False
    Max, Min :: SBinOp t 'True

-- | Scalar trinary operators.
data STriOp :: SType -> Type where
    Fma :: STriOp t    -- fused multiply and add operation: a * b + c

-- | Vector expressions
data VExpr :: SType -> Nat -> Type where
    SLift :: HsSType t -> VExpr t 0
    SCopy :: VExpr t 0 -> VExpr t dim
    VBinApp :: SBinOp t f -> VExpr t dim -> VExpr t dim -> VExpr t dim
    VFold :: SBinOp t 'True -> VExpr t dim -> VExpr t 0
