{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLanguage where
import AbsLanguage
import LexLanguage
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ((Maybe (Int, Int), Ident)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Ident))
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ((Maybe (Int, Int), String)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), String))
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ((Maybe (Int, Int), Integer)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Integer))
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ((Maybe (Int, Int), Program (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Program (Maybe (Int, Int))))
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ((Maybe (Int, Int), LValue (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), LValue (Maybe (Int, Int))))
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ((Maybe (Int, Int), [Expr (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [Expr (Maybe (Int, Int))]))
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ((Maybe (Int, Int), AsgnFields (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), AsgnFields (Maybe (Int, Int))))
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ((Maybe (Int, Int), NewFieldAsgn (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), NewFieldAsgn (Maybe (Int, Int))))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ((Maybe (Int, Int), [NewFieldAsgn (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [NewFieldAsgn (Maybe (Int, Int))]))
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ((Maybe (Int, Int), ExprOrTuple (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), ExprOrTuple (Maybe (Int, Int))))
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ((Maybe (Int, Int), Stmt (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Stmt (Maybe (Int, Int))))
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ((Maybe (Int, Int), Stmt (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Stmt (Maybe (Int, Int))))
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ((Maybe (Int, Int), [Stmt (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [Stmt (Maybe (Int, Int))]))
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ((Maybe (Int, Int), TupleTarget (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TupleTarget (Maybe (Int, Int))))
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ((Maybe (Int, Int), IdentOrIgnr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), IdentOrIgnr (Maybe (Int, Int))))
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ((Maybe (Int, Int), [IdentOrIgnr (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [IdentOrIgnr (Maybe (Int, Int))]))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ((Maybe (Int, Int), VarDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), VarDecl (Maybe (Int, Int))))
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ((Maybe (Int, Int), VarSpec (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), VarSpec (Maybe (Int, Int))))
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ((Maybe (Int, Int), StrcDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), StrcDecl (Maybe (Int, Int))))
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ((Maybe (Int, Int), StrcMembers (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), StrcMembers (Maybe (Int, Int))))
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ((Maybe (Int, Int), DeclStrcMember (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), DeclStrcMember (Maybe (Int, Int))))
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ((Maybe (Int, Int), [DeclStrcMember (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [DeclStrcMember (Maybe (Int, Int))]))
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ((Maybe (Int, Int), FunDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FunDecl (Maybe (Int, Int))))
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ((Maybe (Int, Int), Bind (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Bind (Maybe (Int, Int))))
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ((Maybe (Int, Int), InvokeExprList (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), InvokeExprList (Maybe (Int, Int))))
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ((Maybe (Int, Int), DeclFunParam (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), DeclFunParam (Maybe (Int, Int))))
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ((Maybe (Int, Int), [DeclFunParam (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [DeclFunParam (Maybe (Int, Int))]))
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: ((Maybe (Int, Int), ReturnExpr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), ReturnExpr (Maybe (Int, Int))))
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ((Maybe (Int, Int), FunParams (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FunParams (Maybe (Int, Int))))
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ((Maybe (Int, Int), FuncRetT (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FuncRetT (Maybe (Int, Int))))
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ((Maybe (Int, Int), Type (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Type (Maybe (Int, Int))))
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ((Maybe (Int, Int), [Type (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [Type (Maybe (Int, Int))]))
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ((Maybe (Int, Int), [Ident])) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [Ident]))
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ((Maybe (Int, Int), Boolean (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Boolean (Maybe (Int, Int))))
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\xc4\x76\xcf\x38\x00\x00\x00\x00\x00\x20\x02\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x02\x10\x00\x00\x01\x00\x00\x00\x00\x00\x20\x00\x60\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x22\x09\x07\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x01\x44\x12\x0e\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\x49\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x84\x40\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x02\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x22\x09\x07\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\x49\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x44\x12\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x40\x24\xe1\x00\x00\x00\x00\x00\x00\x08\x00\x80\x00\x22\x09\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x10\x00\x00\x01\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x88\x24\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x04\x10\x49\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x44\x12\x0e\x00\x00\x00\x00\x00\x80\x00\x00\x00\x20\x92\x70\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x91\x84\x03\x00\x00\x00\x00\x00\x20\x00\x00\x00\x88\x24\x1c\x00\x00\x00\x00\x00\x00\x01\x00\x00\x40\x24\xe1\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x22\x09\x07\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\x49\x38\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x48\xc2\x01\x00\x00\x00\x00\x00\x10\x00\x00\x00\x44\x12\x0e\x00\x00\x00\x00\x00\x80\x00\x00\x00\x20\x92\x70\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x91\x84\x03\x00\x00\x00\x00\x00\x20\x00\x00\x00\x88\x24\x1c\x00\x00\x00\x00\x00\x00\x01\x00\x00\x40\x24\xe1\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x22\x09\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x80\x48\xc2\x01\x00\x00\x00\x00\x00\x10\x00\x00\x00\x44\x12\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x91\x84\x03\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x10\x80\x40\x08\x04\x00\x00\x00\x00\x00\x80\x00\x10\x80\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\xc0\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x20\x80\x48\xc2\x01\x00\x00\x00\x00\x00\x08\x01\x00\x08\x00\x80\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x62\xbb\x67\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x10\x02\x01\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x01\x08\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x02\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x04\x00\x40\x00\x00\x00\x00\x00\x00\x44\x00\x00\xc4\x76\xcf\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x20\x04\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x00\x20\xb6\x7b\xc6\x01\x00\x00\x00\x00\x00\x10\x00\x00\x00\x44\x12\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x42\x20\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x08\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\x49\x38\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x88\x24\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x40\x00\x00\x04\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x91\x84\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x80\xd8\xee\x99\x07\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x80\x00\x40\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x10\x49\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x02\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x10\xdb\x3d\xe3\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x22\x09\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x08\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x08\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x20\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\xb1\xdd\x33\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","Ident","String","Integer","Program","LValue","Expr","Expr1","Expr2","Expr3","Expr4","Expr6","Expr7","Expr8","Expr5","ListExpr","AsgnFields","NewFieldAsgn","ListNewFieldAsgn","ExprOrTuple","Stmt","Stmt1","ListStmt","TupleTarget","IdentOrIgnr","ListIdentOrIgnr","VarDecl","VarSpec","StrcDecl","StrcMembers","DeclStrcMember","ListDeclStrcMember","FunDecl","Bind","InvokeExprList","DeclFunParam","ListDeclFunParam","ReturnExpr","FunParams","FuncRetT","Type","ListType","ListIdent","Boolean","'!'","'!='","'%'","'&&'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'..'","'/'","':'","'::'","';'","'<'","'<='","'='","'=='","'>'","'>='","'@'","'['","']'","'^'","'^^'","'_'","'assert'","'bool'","'break'","'continue'","'else'","'false'","'for'","'if'","'int'","'new'","'print'","'return'","'scan'","'string'","'struct'","'true'","'while'","'{'","'||'","'}'","L_ident","L_quoted","L_integ","%eof"]
        bit_start = st * 99
        bit_end = (st + 1) * 99
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..98]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xd8\xff\x00\x00\xe1\xff\x01\x00\x37\x01\x00\x00\x00\x00\xda\x00\xfd\xff\xdf\x03\x09\x01\x0a\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\xa2\x00\x3d\x00\x69\x00\x4b\x00\x7d\x00\x00\x00\x7b\x00\x76\x00\xea\xff\x7f\x00\x92\x00\x79\x00\x9e\x00\x00\x00\xb5\x00\xb6\x00\x93\x00\xcb\x00\x8b\x00\xe0\x00\x00\x00\xee\x00\x00\x00\x00\x00\xbd\x00\xe9\xff\xef\x00\xf5\x00\x97\x00\x00\x00\xc8\x00\xbd\x00\xbd\x00\xd3\x00\xbd\x00\xe3\x00\x00\x00\x00\x00\xbd\x00\x8b\x00\x00\x00\x11\x01\xf8\x00\x00\x00\x1a\x01\x53\x00\x14\x01\x1e\x01\x00\x00\xfe\xff\x15\x01\x00\x00\x00\x00\x99\x00\x00\x00\x12\x01\xab\x00\x00\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\xbd\x00\x00\x00\xbd\x00\xbd\x00\xf3\x00\xbd\x00\x18\x01\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\xff\xff\x0a\x01\xc2\x02\x91\x00\x00\x00\xdf\x03\xdf\x03\xdf\x03\x09\x01\x09\x01\x09\x01\x09\x01\x09\x01\x09\x01\x0a\x00\x0a\x00\x0a\x00\x1f\x01\x1f\x01\x1f\x01\x00\x00\x2e\x01\xab\x00\x56\x00\x35\x01\x00\x00\x22\x00\x13\x01\x0c\x04\x38\x01\x3e\x01\x00\x00\x00\x00\x16\x01\x00\x00\x36\x01\x00\x00\xea\xff\x3a\x01\xc5\x00\x3d\x01\xb3\x00\x1b\x01\x48\x01\x45\x01\x00\x00\x00\x00\x47\x01\x5a\x01\x00\x00\x00\x00\x00\x00\xcd\x00\x64\x00\x00\x00\xe9\xff\x00\x00\x50\x01\x4e\x01\x32\x01\x5d\x01\x00\x00\x64\x00\xbd\x00\x00\x00\x00\x00\x00\x00\xe9\xff\x67\x01\x00\x00\x00\x00\x39\x01\x00\x00\xe9\xff\x00\x00\x00\x00\x00\x00\xbd\x00\x58\x01\x00\x00\x00\x00\x59\x01\xbd\x00\x3c\x01\x60\x01\x3b\x01\x61\x01\x00\x00\x97\x00\xbd\x00\x00\x00\x00\x00\x43\x00\x5b\x01\x00\x00\x00\x00\x98\x00\x00\x00\xc6\x00\x41\x01\x00\x00\xbd\x00\x00\x00\x00\x00\x00\x00\x97\x00\x00\x00\x64\x00\xbd\x00\x00\x00\x00\x00\x97\x00\x41\x01\x00\x00\xe9\xff\x00\x00\x00\x00\xd6\x00\x00\x00\x64\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x08\x00\x00\x00\x00\x00\x00\x00\x03\x01\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x01\x00\x00\x57\x01\x00\x00\x00\x00\xed\x01\x8f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x01\x00\x00\xfb\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd3\x02\x10\x00\x66\x01\x00\x00\x00\x00\x00\x00\x00\x00\x22\x02\x31\x02\x00\x00\xfa\x02\x88\x01\x00\x00\x00\x00\x08\x03\x58\x02\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x63\x01\x75\x01\x00\x00\x67\x02\x00\x00\x00\x00\x8e\x02\x00\x00\x45\x05\x07\x05\x15\x05\x37\x05\xb0\x04\xd7\x04\xe5\x04\x11\x04\x38\x04\x46\x04\x6d\x04\x7b\x04\xa2\x04\xce\x03\x00\x00\xdc\x03\x03\x04\x8d\x01\x2f\x03\x00\x00\x00\x00\x00\x00\xf9\x00\x00\x00\x00\x00\xca\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x02\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\x00\x00\x00\x00\x00\x00\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x01\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x01\x3d\x03\x00\x00\x00\x00\x00\x00\x58\x00\x76\x01\x00\x00\x00\x00\x09\x00\x00\x00\x15\x00\x7a\x01\x00\x00\x00\x00\xc4\x02\x00\x00\x00\x00\x00\x00\x00\x00\x64\x03\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x03\x00\x00\x00\x00\x78\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x99\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x01\xa7\x03\x00\x00\x00\x00\x00\x00\xdb\x00\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xba\xff\x00\x00\xfe\xff\x00\x00\xa4\xff\xfa\xff\xdc\xff\xdb\xff\xdf\xff\x00\x00\xf5\xff\xee\xff\xea\xff\xe6\xff\xd8\xff\xe0\xff\xdd\xff\xe4\xff\xb9\xff\xca\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa4\xff\xda\xff\xa5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9e\xff\x00\x00\x8f\xff\x00\x00\xfd\xff\xfc\xff\x00\x00\x00\x00\xfa\xff\xdf\xff\xd0\xff\x9d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xc0\xff\x00\x00\x00\x00\xb7\xff\xb5\xff\x00\x00\xb6\xff\xfa\xff\x00\x00\xa0\xff\x00\x00\x9b\xff\x00\x00\x98\xff\xba\xff\xe1\xff\x00\x00\xc8\xff\x00\x00\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xb0\xff\x00\x00\xc6\xff\xc7\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\xf8\xff\xf6\xff\xf7\xff\xf2\xff\xf0\xff\xf4\xff\xf1\xff\xef\xff\xf3\xff\xeb\xff\xec\xff\xed\xff\xe8\xff\xe9\xff\xe7\xff\xe5\xff\x00\x00\x00\x00\xd7\xff\x00\x00\xa2\xff\xa4\xff\x00\x00\x00\x00\x91\xff\x00\x00\xa6\xff\x9c\xff\x00\x00\xd9\xff\x00\x00\xb8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\x00\x00\x00\x00\xc1\xff\x94\xff\x93\xff\x00\x00\x96\xff\x97\xff\x95\xff\x00\x00\xa4\xff\xe2\xff\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\xd2\xff\xd4\xff\xa4\xff\x00\x00\xbe\xff\xc2\xff\xb4\xff\x00\x00\xaf\xff\x9f\xff\xa7\xff\x00\x00\x9a\xff\x00\x00\xba\xff\xbc\xff\xa3\xff\x00\x00\x00\x00\xc3\xff\xc4\xff\xb3\xff\x00\x00\xac\xff\x00\x00\x00\x00\x00\x00\xad\xff\xb1\xff\x00\x00\xc5\xff\xd6\xff\xa4\xff\x00\x00\x90\xff\xa1\xff\x00\x00\xce\xff\xca\xff\x00\x00\xde\xff\x00\x00\xbd\xff\x92\xff\xcb\xff\xd3\xff\xd1\xff\xa4\xff\x00\x00\x99\xff\xa8\xff\xb2\xff\xaa\xff\xae\xff\x00\x00\xab\xff\xa9\xff\x00\x00\xcd\xff\xa4\xff\xcc\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x04\x00\x01\x00\x00\x00\x06\x00\x06\x00\x05\x00\x1d\x00\x1f\x00\x00\x00\x32\x00\x03\x00\x1a\x00\x03\x00\x11\x00\x26\x00\x00\x00\x07\x00\x00\x00\x21\x00\x2b\x00\x00\x00\x35\x00\x00\x00\x0e\x00\x1c\x00\x19\x00\x32\x00\x32\x00\x15\x00\x1d\x00\x1e\x00\x1b\x00\x20\x00\x21\x00\x01\x00\x23\x00\x24\x00\x25\x00\x05\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x29\x00\x30\x00\x2d\x00\x2e\x00\x32\x00\x32\x00\x29\x00\x32\x00\x33\x00\x34\x00\x35\x00\x27\x00\x28\x00\x27\x00\x28\x00\x19\x00\x27\x00\x28\x00\x27\x00\x1d\x00\x1e\x00\x11\x00\x20\x00\x21\x00\x01\x00\x23\x00\x24\x00\x25\x00\x05\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x11\x00\x2d\x00\x2e\x00\x05\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x04\x00\x00\x00\x06\x00\x04\x00\x1a\x00\x19\x00\x10\x00\x11\x00\x09\x00\x1d\x00\x1e\x00\x21\x00\x20\x00\x21\x00\x01\x00\x23\x00\x24\x00\x25\x00\x05\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x05\x00\x1c\x00\x2d\x00\x2e\x00\x1c\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x2c\x00\x27\x00\x2f\x00\x05\x00\x06\x00\x19\x00\x01\x00\x27\x00\x05\x00\x1d\x00\x1e\x00\x30\x00\x20\x00\x21\x00\x30\x00\x23\x00\x24\x00\x25\x00\x11\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x05\x00\x2d\x00\x2e\x00\x14\x00\x00\x00\x04\x00\x32\x00\x33\x00\x34\x00\x23\x00\x27\x00\x04\x00\x04\x00\x27\x00\x05\x00\x06\x00\x2a\x00\x0f\x00\x11\x00\x2d\x00\x19\x00\x0d\x00\x17\x00\x18\x00\x32\x00\x33\x00\x34\x00\x17\x00\x18\x00\x1c\x00\x23\x00\x11\x00\x05\x00\x0f\x00\x27\x00\x1c\x00\x1c\x00\x2a\x00\x14\x00\x04\x00\x2d\x00\x06\x00\x05\x00\x05\x00\x23\x00\x32\x00\x33\x00\x34\x00\x27\x00\x30\x00\x05\x00\x2a\x00\x19\x00\x32\x00\x2d\x00\x30\x00\x30\x00\x04\x00\x00\x00\x32\x00\x33\x00\x34\x00\x23\x00\x1c\x00\x05\x00\x04\x00\x27\x00\x06\x00\x00\x00\x2a\x00\x11\x00\x11\x00\x2d\x00\x11\x00\x04\x00\x00\x00\x06\x00\x32\x00\x33\x00\x34\x00\x23\x00\x1c\x00\x00\x00\x30\x00\x27\x00\x05\x00\x0c\x00\x2a\x00\x22\x00\x1c\x00\x2d\x00\x00\x00\x22\x00\x23\x00\x14\x00\x32\x00\x33\x00\x34\x00\x1c\x00\x05\x00\x05\x00\x30\x00\x22\x00\x23\x00\x1d\x00\x1e\x00\x0f\x00\x10\x00\x11\x00\x30\x00\x1c\x00\x1d\x00\x1e\x00\x0c\x00\x2f\x00\x00\x00\x01\x00\x02\x00\x30\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x1a\x00\x0a\x00\x1b\x00\x32\x00\x13\x00\x14\x00\x1f\x00\x16\x00\x09\x00\x01\x00\x19\x00\x09\x00\x25\x00\x05\x00\x0b\x00\x18\x00\x1f\x00\x20\x00\x06\x00\x32\x00\x14\x00\x0f\x00\x25\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x01\x00\x2f\x00\x1b\x00\x06\x00\x05\x00\x13\x00\x14\x00\x11\x00\x16\x00\x09\x00\x2f\x00\x19\x00\x06\x00\x0f\x00\x0f\x00\x10\x00\x32\x00\x1f\x00\x20\x00\x11\x00\x0f\x00\x32\x00\x06\x00\x25\x00\x09\x00\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1a\x00\x06\x00\x11\x00\x14\x00\x31\x00\x13\x00\x14\x00\x09\x00\x16\x00\x01\x00\x11\x00\x19\x00\x32\x00\x31\x00\x14\x00\x32\x00\x0f\x00\x1f\x00\x20\x00\x11\x00\x32\x00\x21\x00\x1a\x00\x25\x00\x20\x00\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x21\x00\x00\x00\x26\x00\x15\x00\x13\x00\x14\x00\x00\x00\x16\x00\x15\x00\x1a\x00\x19\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\x12\x00\xff\xff\x22\x00\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x00\x00\x01\x00\x02\x00\x12\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x00\x00\x01\x00\x02\x00\x12\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\x1f\x00\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x14\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1f\x00\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\x26\x00\x25\x00\xff\xff\xff\xff\xff\xff\x2b\x00\x2a\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\x32\x00\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\xff\xff\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x02\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\x12\x00\x13\x00\x25\x00\x15\x00\x16\x00\x17\x00\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\xff\xff\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\x19\x00\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\x1f\x00\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\x26\x00\xff\xff\xff\xff\xff\xff\x25\x00\x2b\x00\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\xff\xff\x32\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\xff\xff\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\x00\x00\x01\x00\x02\x00\x2a\x00\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x25\x00\x04\x00\xff\xff\xff\xff\xff\xff\x2a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x25\x00\x04\x00\xff\xff\xff\xff\xff\xff\x2a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x5c\x00\x1b\x00\x84\x00\x87\x00\x45\x00\x1c\x00\x40\x00\x99\x00\x84\x00\x03\x00\x03\x00\x61\x00\x50\x00\x5d\x00\x9a\x00\x95\x00\x51\x00\x95\x00\x62\x00\x9b\x00\x95\x00\xff\xff\x95\x00\x52\x00\x5e\x00\x1d\x00\x03\x00\x03\x00\x04\x00\x1e\x00\x1f\x00\x4f\x00\x20\x00\x21\x00\x1b\x00\x22\x00\x23\x00\x24\x00\x1c\x00\x25\x00\x26\x00\x27\x00\x28\x00\x85\x00\x5f\x00\x29\x00\x2a\x00\x03\x00\x03\x00\xc5\x00\x03\x00\x2b\x00\x2c\x00\xfb\xff\x96\x00\x97\x00\x96\x00\xce\x00\x1d\x00\x96\x00\xc4\x00\xb8\x00\x1e\x00\x1f\x00\x4e\x00\x20\x00\x21\x00\x1b\x00\x22\x00\x23\x00\x24\x00\x1c\x00\x25\x00\x26\x00\x27\x00\x28\x00\xa1\x00\x4b\x00\x29\x00\x2a\x00\x68\x00\x95\x00\xb3\x00\x03\x00\x2b\x00\x2c\x00\x5c\x00\x95\x00\x8a\x00\x5c\x00\x8a\x00\x1d\x00\xa3\x00\xd1\x00\xb5\x00\x1e\x00\x1f\x00\x62\x00\x20\x00\x21\x00\x1b\x00\x22\x00\x23\x00\x24\x00\x1c\x00\x25\x00\x26\x00\x27\x00\x28\x00\x4a\x00\x5e\x00\x29\x00\x2a\x00\x5e\x00\x95\x00\xd6\x00\x03\x00\x2b\x00\x2c\x00\x69\x00\xaf\x00\x48\x00\x1c\x00\x45\x00\x1d\x00\x1b\x00\xc6\x00\x46\x00\x1e\x00\x1f\x00\x5f\x00\x20\x00\x21\x00\x5f\x00\x22\x00\x23\x00\x24\x00\x3a\x00\x25\x00\x26\x00\x27\x00\x28\x00\x3c\x00\x1c\x00\x29\x00\x2a\x00\x3c\x00\x3c\x00\x5c\x00\x03\x00\x2b\x00\x2c\x00\x22\x00\xda\x00\x5c\x00\x5c\x00\x25\x00\x1c\x00\x81\x00\x28\x00\x3b\x00\xb8\x00\x29\x00\x34\x00\xd4\x00\x3d\x00\x3e\x00\x03\x00\x2b\x00\x2c\x00\x3d\x00\xa9\x00\x5e\x00\x22\x00\x39\x00\x1c\x00\x4c\x00\x25\x00\x5e\x00\x5e\x00\x28\x00\x4d\x00\x5c\x00\x29\x00\xa6\x00\x38\x00\x37\x00\x22\x00\x03\x00\x2b\x00\x2c\x00\x25\x00\x5f\x00\x1c\x00\x28\x00\x34\x00\x03\x00\x29\x00\x5f\x00\x5f\x00\x5c\x00\xab\x00\x03\x00\x2b\x00\x2c\x00\x22\x00\x5e\x00\x35\x00\x5c\x00\x25\x00\x9d\x00\xab\x00\x28\x00\xa8\x00\x4e\x00\x29\x00\x95\x00\x5c\x00\xbb\x00\xdf\x00\x03\x00\x2b\x00\x2c\x00\x22\x00\x5e\x00\xbb\x00\x5f\x00\x25\x00\x2e\x00\x60\x00\x28\x00\xd3\x00\x5e\x00\x29\x00\xa1\x00\x42\x00\x43\x00\x61\x00\x03\x00\x2b\x00\x2c\x00\x5e\x00\x2d\x00\x4a\x00\x5f\x00\x42\x00\xac\x00\xbd\x00\xdb\x00\xa2\x00\xa3\x00\xa4\x00\x5f\x00\xbc\x00\xbd\x00\xbe\x00\x60\x00\x92\x00\x05\x00\x06\x00\x07\x00\x5f\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x53\x00\x8c\x00\x54\x00\x65\x00\x03\x00\x12\x00\x13\x00\x66\x00\x14\x00\x8d\x00\x64\x00\x15\x00\x89\x00\x18\x00\x4a\x00\x84\x00\x55\x00\x16\x00\x17\x00\x88\x00\x03\x00\x7e\x00\x6a\x00\x18\x00\xaf\xff\x05\x00\x06\x00\x07\x00\x19\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x64\x00\xbb\x00\x4f\x00\xb4\x00\x4a\x00\xcf\x00\x13\x00\xb7\x00\x14\x00\xaf\x00\xb2\x00\x15\x00\xae\x00\xab\x00\xaf\xff\x65\x00\x03\x00\x16\x00\x17\x00\xa9\x00\xa7\x00\x03\x00\xa1\x00\x18\x00\x9f\x00\x05\x00\x06\x00\x07\x00\x19\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xa0\x00\x9e\x00\xce\x00\xcd\x00\xcc\x00\xc8\x00\xc9\x00\xcb\x00\x14\x00\x64\x00\xc2\x00\x15\x00\x03\x00\xd9\x00\xc1\x00\x03\x00\xda\x00\x16\x00\x17\x00\xd8\x00\x03\x00\x48\x00\xd5\x00\x18\x00\x46\x00\x05\x00\x06\x00\x07\x00\x19\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x35\x00\x62\x00\x8f\x00\x82\x00\x81\x00\x12\x00\x13\x00\x6b\x00\x14\x00\xc3\x00\x8a\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x05\x00\x06\x00\x07\x00\x19\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\x13\x00\x00\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x05\x00\x06\x00\x07\x00\x19\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\x00\x13\x00\x00\x00\x14\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x40\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x30\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x31\x00\x00\x00\x42\x00\x43\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x7e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x93\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x7e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x92\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x30\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x8d\x00\x2f\x00\x7e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x7f\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x30\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x7c\x00\x2f\x00\x30\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x16\x00\x00\x00\xb5\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x7e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xc2\x00\x2e\x00\x06\x00\x07\x00\xba\x00\x2f\x00\x9b\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x99\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x00\x18\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x19\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x90\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x8e\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x6a\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\xc7\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\xbf\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\xd6\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\xd0\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\xdc\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x00\x00\x6e\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x56\x00\x6d\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x57\x00\x58\x00\x18\x00\x59\x00\x5a\x00\x5b\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x00\x00\x6c\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x74\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x99\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\x18\x00\x9b\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x00\x00\x03\x00\x73\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x72\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x00\x00\x00\x00\x71\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x70\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x00\x00\x00\x00\x6f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x77\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x19\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x76\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x75\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x18\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x19\x00\x7a\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x2e\x00\x06\x00\x07\x00\x18\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x19\x00\x78\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x2e\x00\x06\x00\x07\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 113) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113)
	]

happy_n_terms = 54 :: Int
happy_n_nonterms = 43 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((Just (tokenLineCol happy_var_1), Ident (prToken happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((Just (tokenLineCol happy_var_1), prToken happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((fst happy_var_1, AbsLanguage.Prog (fst happy_var_1)(reverse (snd happy_var_1)))
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ((fst happy_var_1, AbsLanguage.LValueVar (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_6 = happySpecReduce_3  4# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 ((fst happy_var_1, AbsLanguage.LValueMemb (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_7 = happySpecReduce_3  5# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ((fst happy_var_1, AbsLanguage.ELor (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_8 = happySpecReduce_3  5# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ((fst happy_var_1, AbsLanguage.ELand (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_9 = happySpecReduce_3  5# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ((fst happy_var_1, AbsLanguage.EXor (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_10 = happySpecReduce_1  5# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_11 = happySpecReduce_3  6# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((fst happy_var_1, AbsLanguage.EEq (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_12 = happySpecReduce_3  6# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((fst happy_var_1, AbsLanguage.ENeq (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_13 = happySpecReduce_3  6# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((fst happy_var_1, AbsLanguage.EGeq (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_14 = happySpecReduce_3  6# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((fst happy_var_1, AbsLanguage.ELeq (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_15 = happySpecReduce_3  6# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((fst happy_var_1, AbsLanguage.EGt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_16 = happySpecReduce_3  6# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ((fst happy_var_1, AbsLanguage.ELt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_17 = happySpecReduce_1  6# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_18 = happySpecReduce_3  7# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((fst happy_var_1, AbsLanguage.EPlus (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_19 = happySpecReduce_3  7# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((fst happy_var_1, AbsLanguage.EMinus (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_20 = happySpecReduce_3  7# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((fst happy_var_1, AbsLanguage.ECat (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_22 = happySpecReduce_3  8# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 ((fst happy_var_1, AbsLanguage.ETimes (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_23 = happySpecReduce_3  8# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 ((fst happy_var_1, AbsLanguage.EDiv (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_24 = happySpecReduce_3  8# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 ((fst happy_var_1, AbsLanguage.EMod (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_25 = happySpecReduce_1  8# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_26 = happySpecReduce_3  9# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 ((fst happy_var_1, AbsLanguage.EPow (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_27 = happySpecReduce_1  9# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_28 = happySpecReduce_2  10# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 ((fst happy_var_1, AbsLanguage.EFnCall (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_29 = happyReduce 4# 10# happyReduction_29
happyReduction_29 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.EScan (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_30 = happySpecReduce_2  10# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 ((fst happy_var_1, AbsLanguage.EIife (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_31 = happySpecReduce_1  10# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_32 = happySpecReduce_1  11# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((fst happy_var_1, AbsLanguage.ELValue (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_33 = happyReduce 5# 11# happyReduction_33
happyReduction_33 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_4 of { happy_var_4 -> 
	happyIn15
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.ENew (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_34 = happySpecReduce_1  11# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_35 = happySpecReduce_1  12# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((fst happy_var_1, AbsLanguage.EString (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_36 = happySpecReduce_1  12# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((fst happy_var_1, AbsLanguage.EInt (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_37 = happySpecReduce_1  12# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((fst happy_var_1, AbsLanguage.EBool (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_38 = happySpecReduce_3  12# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 ((Just (tokenLineCol happy_var_1), snd happy_var_2)
	)}}

happyReduce_39 = happySpecReduce_1  13# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_40 = happySpecReduce_1  14# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_41 = happySpecReduce_3  14# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_42 = happySpecReduce_0  15# happyReduction_42
happyReduction_42  =  happyIn19
		 ((Nothing, AbsLanguage.AFEmpty Nothing)
	)

happyReduce_43 = happySpecReduce_1  15# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, AbsLanguage.AFList (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_44 = happySpecReduce_3  16# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 ((fst happy_var_1, AbsLanguage.NFADefault (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_45 = happySpecReduce_1  17# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_46 = happySpecReduce_3  17# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_47 = happySpecReduce_1  18# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((fst happy_var_1, AbsLanguage.EOTRegular (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_48 = happySpecReduce_3  18# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.EOTTuple (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_49 = happyReduce 5# 19# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut23 happy_x_5 of { happy_var_5 -> 
	happyIn23
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SIf (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_50 = happyReduce 7# 19# happyReduction_50
happyReduction_50 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut24 happy_x_5 of { happy_var_5 -> 
	case happyOut23 happy_x_7 of { happy_var_7 -> 
	happyIn23
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SIfElse (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5)(snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_51 = happyReduce 9# 19# happyReduction_51
happyReduction_51 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_5 of { happy_var_5 -> 
	case happyOut9 happy_x_7 of { happy_var_7 -> 
	case happyOut23 happy_x_9 of { happy_var_9 -> 
	happyIn23
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SFor (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5)(snd happy_var_7)(snd happy_var_9))
	) `HappyStk` happyRest}}}}}

happyReduce_52 = happyReduce 5# 19# happyReduction_52
happyReduction_52 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut23 happy_x_5 of { happy_var_5 -> 
	happyIn23
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SWhile (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_53 = happySpecReduce_1  19# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_54 = happySpecReduce_2  20# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.SExpr (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_55 = happySpecReduce_2  20# happyReduction_55
happyReduction_55 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.SVDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_56 = happySpecReduce_3  20# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.SFDecl (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_57 = happySpecReduce_3  20# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.SSDecl (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_58 = happyReduce 5# 20# happyReduction_58
happyReduction_58 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_4 of { happy_var_4 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.STDecl (fst happy_var_1)(snd happy_var_1)(snd happy_var_4))
	) `HappyStk` happyRest}}

happyReduce_59 = happyReduce 4# 20# happyReduction_59
happyReduction_59 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.SAssign (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_60 = happyReduce 4# 20# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.STAssign (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_61 = happyReduce 4# 20# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SIgnore (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_62 = happySpecReduce_3  20# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SReturn (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_63 = happySpecReduce_2  20# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SBreak (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_64 = happySpecReduce_2  20# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SCont (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_65 = happyReduce 4# 20# happyReduction_65
happyReduction_65 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SAssert (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_66 = happyReduce 5# 20# happyReduction_66
happyReduction_66 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SPrint (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_67 = happyReduce 4# 20# happyReduction_67
happyReduction_67 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsLanguage.SBlock (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_3)))
	) `HappyStk` happyRest}}

happyReduce_68 = happySpecReduce_2  20# happyReduction_68
happyReduction_68 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_69 = happySpecReduce_0  21# happyReduction_69
happyReduction_69  =  happyIn25
		 ((Nothing, [])
	)

happyReduce_70 = happySpecReduce_2  21# happyReduction_70
happyReduction_70 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_71 = happySpecReduce_3  22# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn26
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.TTar (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_72 = happySpecReduce_1  23# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((fst happy_var_1, AbsLanguage.IOIIdent (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_73 = happySpecReduce_1  23# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.IOIIgnore (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_74 = happySpecReduce_1  24# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_75 = happySpecReduce_3  24# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_76 = happyReduce 4# 25# happyReduction_76
happyReduction_76 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn29
		 ((fst happy_var_1, AbsLanguage.DVDecl (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_77 = happyReduce 6# 25# happyReduction_77
happyReduction_77 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOut9 happy_x_6 of { happy_var_6 -> 
	happyIn29
		 ((fst happy_var_1, AbsLanguage.DVDeclAsgn (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_4)(snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_78 = happyReduce 5# 25# happyReduction_78
happyReduction_78 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut9 happy_x_5 of { happy_var_5 -> 
	happyIn29
		 ((fst happy_var_1, AbsLanguage.DVDeclDeduce (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_79 = happySpecReduce_1  26# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.VSReadOnly (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_80 = happySpecReduce_0  26# happyReduction_80
happyReduction_80  =  happyIn30
		 ((Nothing, AbsLanguage.VSNone Nothing)
	)

happyReduce_81 = happyReduce 4# 27# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.SDDefault (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_82 = happySpecReduce_1  28# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 ((fst happy_var_1, AbsLanguage.SMDefault (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_83 = happySpecReduce_0  28# happyReduction_83
happyReduction_83  =  happyIn32
		 ((Nothing, AbsLanguage.SMEmpty Nothing)
	)

happyReduce_84 = happySpecReduce_3  29# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 ((fst happy_var_1, AbsLanguage.DStrMem (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_85 = happySpecReduce_2  30# happyReduction_85
happyReduction_85 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_86 = happySpecReduce_3  30# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_87 = happyReduce 6# 31# happyReduction_87
happyReduction_87 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_5 of { happy_var_5 -> 
	happyIn35
		 ((fst happy_var_1, AbsLanguage.FDDefault (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_3)(reverse (snd happy_var_5)))
	) `HappyStk` happyRest}}}}

happyReduce_88 = happyReduce 4# 32# happyReduction_88
happyReduction_88 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.BdDefault (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_89 = happySpecReduce_3  32# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.BdPure (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_90 = happySpecReduce_1  32# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.BdPureAlt (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_91 = happySpecReduce_0  32# happyReduction_91
happyReduction_91  =  happyIn36
		 ((Nothing, AbsLanguage.BdNone Nothing)
	)

happyReduce_92 = happySpecReduce_3  33# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.IELDefault (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_93 = happySpecReduce_2  33# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.IELEmpty (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_94 = happyReduce 4# 34# happyReduction_94
happyReduction_94 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	happyIn38
		 ((fst happy_var_1, AbsLanguage.DDeclBasic (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_95 = happySpecReduce_1  35# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_96 = happySpecReduce_3  35# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_97 = happySpecReduce_0  36# happyReduction_97
happyReduction_97  =  happyIn40
		 ((Nothing, AbsLanguage.RExNone Nothing)
	)

happyReduce_98 = happySpecReduce_1  36# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((fst happy_var_1, AbsLanguage.RExRegular (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_99 = happySpecReduce_3  37# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.FPList (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_100 = happySpecReduce_2  37# happyReduction_100
happyReduction_100 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.FPEmpty (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_101 = happySpecReduce_2  38# happyReduction_101
happyReduction_101 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn42
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.FRTSingle (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_102 = happyReduce 4# 38# happyReduction_102
happyReduction_102 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.FRTTuple (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_103 = happySpecReduce_0  38# happyReduction_103
happyReduction_103  =  happyIn42
		 ((Nothing, AbsLanguage.FRTEmpty Nothing)
	)

happyReduce_104 = happySpecReduce_1  39# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.TInt (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_105 = happySpecReduce_1  39# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.TBool (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_106 = happySpecReduce_1  39# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.TString (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_107 = happySpecReduce_1  39# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((fst happy_var_1, AbsLanguage.TUser (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_108 = happySpecReduce_1  40# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_109 = happySpecReduce_3  40# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_110 = happySpecReduce_1  41# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_111 = happySpecReduce_3  41# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_112 = happySpecReduce_1  42# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn46
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.BTrue (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_113 = happySpecReduce_1  42# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn46
		 ((Just (tokenLineCol happy_var_1), AbsLanguage.BFalse (Just (tokenLineCol happy_var_1)))
	)}

happyNewToken action sts stk [] =
	happyDoAction 53# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TV _) -> cont 50#;
	PT _ (TL _) -> cont 51#;
	PT _ (TI _) -> cont 52#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 53# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut7 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/local/lib/ghc-8.6.4/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/root/tmp/ghc11947_0/ghc_2.h" #-}


























































































































































































































































































































































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
