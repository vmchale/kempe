module Kempe.Asm.X86.ControlFlow ( mkControlFlow
                                 , ControlAnn (..)
                                 ) where

-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Kempe.Asm.X86.Type

-- map of labels by node (maybe backwards?)
type FreshM = State (Int, M.Map Label Int) -- TODO: map int to asm

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty)

-- FIXME: split on RET before?
mkControlFlow :: [X86 AbsReg ()] -> [X86 AbsReg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

addrRegs :: Ord reg => Addr reg -> S.Set reg
addrRegs (Reg r)              = S.singleton r
addrRegs (AddrRRPlus r r')    = S.fromList [r, r']
addrRegs (AddrRCPlus r _)     = S.singleton r
addrRegs (AddrRCMinus r _)    = S.singleton r
addrRegs (AddrRRScale r r' _) = S.fromList [r, r']

-- | Annotate instructions with a unique node name and a list of all possible
-- destinations.
addControlFlow :: [X86 AbsReg ()] -> FreshM [X86 AbsReg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f []) S.empty S.empty) l : asms')
    }
addControlFlow ((Je _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l -- TODO: is this what's wanted?
    ; pure (Je (ControlAnn i (f [l_i]) S.empty S.empty) l : asms')
    }
addControlFlow ((Jump _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (Jump (ControlAnn i [l_i] S.empty S.empty) l : nextAsms)
    }
addControlFlow ((Call _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (Call (ControlAnn i [l_i] S.empty S.empty) l : nextAsms)
    }
addControlFlow (Ret{}:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; pure (Ret (ControlAnn i [] S.empty S.empty) : nextAsms)
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f []) (uses asm) (defs asm)) : asms')
    }

uses :: Ord reg => X86 reg ann -> S.Set reg
uses (PushReg _ r)       = S.singleton r
uses (PushMem _ a)       = addrRegs a
uses (PopMem _ a)        = addrRegs a
uses (MovRA _ _ a)       = addrRegs a
uses (MovAR _ a r)       = S.singleton r <> addrRegs a
uses (MovRR _ _ r)       = S.singleton r
uses (AddRR _ r r')      = S.fromList [r, r']
uses (SubRR _ r r')      = S.fromList [r, r']
uses (ImulRR _ r r')     = S.fromList [r, r']
uses (AddRC _ r _)       = S.singleton r
uses (SubRC _ r _)       = S.singleton r
uses (AddAC _ a _)       = addrRegs a
uses (MovABool _ a _)    = addrRegs a
uses (XorRR _ r r')      = S.fromList [r, r']
uses (CmpAddrReg _ a r)  = S.singleton r <> addrRegs a
uses (CmpRegReg _ r r')  = S.fromList [r, r']
uses (CmpAddrBool _ a _) = addrRegs a
uses (ShiftLRR _ r r')   = S.fromList [r, r']
uses (ShiftRRR _ r r')   = S.fromList [r, r']
uses (MovRCi8 _ r _)     = S.singleton r
uses (MovACi8 _ a _)     = addrRegs a
uses (IdivR _ r)         = S.singleton r
uses _                   = S.empty

defs :: X86 reg ann -> S.Set reg
defs (MovRA _ r _)     = S.singleton r
defs (MovRR _ r _)     = S.singleton r
defs (MovRC _ r _)     = S.singleton r
defs (MovRCBool _ r _) = S.singleton r
defs (MovRCi8 _ r _)   = S.singleton r
defs (MovRWord _ r _)  = S.singleton r
defs (AddRR _ r _)     = S.singleton r
defs (SubRR _ r _)     = S.singleton r
defs (ImulRR _ r _)    = S.singleton r
defs (AddRC _ r _)     = S.singleton r
defs (SubRC _ r _)     = S.singleton r
defs (XorRR _ r _)     = S.singleton r
defs (MovRL _ r _)     = S.singleton r
defs (ShiftRRR _ r _)  = S.singleton r
defs (PopReg _ r)      = S.singleton r
defs (ShiftLRR _ r _)  = S.singleton r
defs _                 = S.empty

next :: [X86 AbsReg ()] -> FreshM ([Int] -> [Int], [X86 AbsReg ControlAnn])
next asms = do
    nextAsms <- addControlFlow asms
    case nextAsms of
        []      -> pure (id, [])
        (asm:_) -> pure ((node (ann asm) :), nextAsms)

-- | Construct map assigning labels to their node name.
broadcasts :: [X86 reg ()] -> FreshM [X86 reg ()]
broadcasts [] = pure []
broadcasts (asm@(Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (asm :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
