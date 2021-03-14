module Kempe.Asm.Arm.ControlFlow ( mkControlFlow
                                 , ControlAnn (..)
                                 ) where

import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Semigroup             ((<>))
import           Kempe.Asm.Arm.Type
import           Kempe.Asm.Type

-- map of labels by node
type FreshM = State (Int, M.Map Label Int) -- TODO: map int to asm

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty)

mkControlFlow :: [Arm AbsReg ()] -> [Arm AbsReg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

singleton :: AbsReg -> IS.IntSet
singleton = maybe IS.empty IS.singleton . toInt

-- | Can't be called on abstract registers i.e. 'DataPointer'
-- This is kinda sus but it allows us to use an 'IntSet' for liveness analysis.
toInt :: AbsReg -> Maybe Int
toInt (AllocReg i) = Just i
toInt _            = Nothing

fromList :: [AbsReg] -> IS.IntSet
fromList = foldMap singleton

addrRegs :: Addr AbsReg -> IS.IntSet
addrRegs (Reg r)          = singleton r
addrRegs (AddRRPlus r r') = fromList [r, r']
addrRegs (AddRCPlus r _)  = singleton r

-- | Annotate instructions with a unique node name and a list of all possible
-- destinations.
addControlFlow :: [Arm AbsReg ()] -> FreshM [Arm AbsReg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f []) IS.empty IS.empty) l : asms')
    }
addControlFlow ((BranchCond _ l c):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l -- TODO: is this what's wanted?
    ; pure (BranchCond (ControlAnn i (f [l_i]) IS.empty IS.empty) l c : asms')
    }
addControlFlow ((BranchZero _ r l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (BranchZero (ControlAnn i (f [l_i]) IS.empty IS.empty) r l : asms')
    }
addControlFlow ((BranchLink _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (BranchLink (ControlAnn i [l_i] IS.empty IS.empty) l : nextAsms)
    }
addControlFlow (Ret{}:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; pure (Ret (ControlAnn i [] IS.empty IS.empty) : nextAsms)
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f []) (uses asm) (defs asm)) : asms')
    }

uses :: Arm AbsReg ann -> IS.IntSet
uses (MovRR _ _ r)            = singleton r
uses (AddRR _ _ r r')         = fromList [r, r']
uses (SubRR _ _ r r')         = fromList [r, r']
uses (SubRC _ _ r _)          = singleton r
uses (LShiftLRR _ _ r r')     = fromList [r, r']
uses (LShiftRRR _ _ r r')     = fromList [r, r']
uses (BranchZero _ r _)       = singleton r
uses (AddRC _ _ r _)          = singleton r
uses (MulRR _ _ r r')         = fromList [r, r']
uses (AndRR _ _ r r')         = fromList [r, r']
uses (SignedDivRR _ _ r r')   = fromList [r, r']
uses (UnsignedDivRR _ _ r r') = fromList [r, r']
uses (CmpRR _ r r')           = fromList [r, r']
uses (Load _ _ a)             = addrRegs a
uses (Store _ r a)            = singleton r <> addrRegs a
uses (StoreByte _ r a)        = singleton r <> addrRegs a
uses Branch{}                 = mempty
uses BranchLink{}             = mempty
uses BranchCond{}             = mempty
uses MovRC{}                  = mempty
uses MovRWord{}               = mempty
uses LoadLabel{}              = mempty
uses Label{}                  = mempty
uses BSLabel{}                = mempty
uses GnuMacro{}               = mempty
uses CSet{}                   = mempty
uses Ret{}                    = mempty

defs :: Arm AbsReg ann -> IS.IntSet
defs (MovRR _ r _)           = singleton r
defs (MovRC _ r _)           = singleton r
defs (MovRWord _ r _)        = singleton r
defs (AddRR _ r _ _)         = singleton r
defs (SubRR _ r _ _)         = singleton r
defs (AddRC _ r _ _)         = singleton r
defs (SubRC _ r _ _)         = singleton r
defs (LShiftRRR _ r _ _)     = singleton r
defs (LShiftLRR _ r _ _)     = singleton r
defs (AndRR _ r _ _)         = singleton r
defs (MulRR _ r _ _)         = singleton r
defs (Load _ r _)            = singleton r
defs (SignedDivRR _ r _ _)   = singleton r
defs (UnsignedDivRR _ r _ _) = singleton r
defs (LoadLabel _ r _)       = singleton r
defs (CSet _ r _)            = singleton r
defs Branch{}                = mempty
defs Label{}                 = mempty
defs BSLabel{}               = mempty
defs GnuMacro{}              = mempty
defs Ret{}                   = mempty
defs BranchLink{}            = mempty
defs BranchCond{}            = mempty
defs BranchZero{}            = mempty
defs Store{}                 = mempty
defs StoreByte{}             = mempty
defs CmpRR{}                 = mempty

next :: [Arm AbsReg ()] -> FreshM ([Int] -> [Int], [Arm AbsReg ControlAnn])
next asms = do
    nextAsms <- addControlFlow asms
    case nextAsms of
        []      -> pure (id, [])
        (asm:_) -> pure ((node (ann asm) :), nextAsms)

-- | Construct map assigning labels to their node name.
broadcasts :: [Arm reg ()] -> FreshM [Arm reg ()]
broadcasts [] = pure []
broadcasts (asm@(Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (asm :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
