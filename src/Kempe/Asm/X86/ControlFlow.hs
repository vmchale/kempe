module Kempe.Asm.X86.ControlFlow ( mkControlFlow
                                 , ControlAnn (..)
                                 ) where

-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Semigroup             ((<>))
import           Kempe.Asm.Type
import           Kempe.Asm.X86.Type

-- map of labels by node
type FreshM = State (Int, M.Map Label Int) -- TODO: map int to asm

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty)

mkControlFlow :: [X86 AbsReg ()] -> [X86 AbsReg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd)

allLabels :: FreshM [Int]
allLabels = gets (M.elems . snd)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

singleton :: AbsReg -> IS.IntSet
singleton = maybe IS.empty IS.singleton . toInt

-- | Make sure 8-bit and 64-bit registers have no overlap.
--
-- Also can't be called on abstract registers i.e. 'DataPointer' or 'CArg1'.
-- This is kinda sus but it allows us to use an 'IntSet' for liveness analysis.
toInt :: AbsReg -> Maybe Int
toInt (AllocReg64 i) = Just i
toInt (AllocReg8 i)  = Just i
toInt _              = Nothing

fromList :: [AbsReg] -> IS.IntSet
fromList = foldMap singleton

addrRegs :: Addr AbsReg -> IS.IntSet
addrRegs (Reg r)              = singleton r
addrRegs (AddrRRPlus r r')    = fromList [r, r']
addrRegs (AddrRCPlus r _)     = singleton r
addrRegs (AddrRCMinus r _)    = singleton r
addrRegs (AddrRRScale r r' _) = fromList [r, r']

-- | Annotate instructions with a unique node name and a list of all possible
-- destinations.
addControlFlow :: [X86 AbsReg ()] -> FreshM [X86 AbsReg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f []) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Je _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l -- TODO: is this what's wanted?
    ; pure (Je (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jl _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jl (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jle _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jle (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jne _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jne (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jge _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jge (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jg _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jg (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jump _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (Jump (ControlAnn i [l_i] IS.empty IS.empty) l : nextAsms)
    }
addControlFlow ((Call _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (Call (ControlAnn i [l_i] IS.empty IS.empty) l : nextAsms)
    }
addControlFlow (Ret{}:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; pure (Ret (ControlAnn i [] IS.empty IS.empty) : nextAsms)
    }
addControlFlow ((JumpReg _ r):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; ls <- allLabels
    ; pure (JumpReg (ControlAnn i ls (singleton r) IS.empty) r : nextAsms)
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f []) (uses asm) (defs asm)) : asms')
    }

uses :: X86 AbsReg ann -> IS.IntSet
uses (JumpReg _ r)       = singleton r
uses (PushReg _ r)       = singleton r
uses (PushMem _ a)       = addrRegs a
uses (PopMem _ a)        = addrRegs a
uses (MovRA _ _ a)       = addrRegs a
uses (MovAR _ a r)       = singleton r <> addrRegs a
uses (MovALK _ a _)      = addrRegs a
uses (MovRR _ _ r)       = singleton r
uses (MovRRLower _ _ r)  = singleton r
uses (AddRR _ r r')      = fromList [r, r']
uses (SubRR _ r r')      = fromList [r, r']
uses (ImulRR _ r r')     = fromList [r, r']
uses (AddRC _ r _)       = singleton r
uses (SubRC _ r _)       = singleton r
uses (AddAC _ a _)       = addrRegs a
uses (MovABool _ a _)    = addrRegs a
uses (MovAC  _ a _)      = addrRegs a
uses (MovACi8 _ a _)     = addrRegs a
uses (XorRR _ r r')      = fromList [r, r']
uses (CmpAddrReg _ a r)  = singleton r <> addrRegs a
uses (CmpRegReg _ r r')  = fromList [r, r']
uses (CmpRegBool _ r _)  = singleton r
uses (CmpAddrBool _ a _) = addrRegs a
uses (LShiftLRR _ r r')  = fromList [r, r']
uses (LShiftRRR _ r r')  = fromList [r, r']
uses (AShiftRRR _ r r')  = fromList [r, r']
uses (MovRCi8 _ r _)     = singleton r
uses (MovACTag _ a _)    = addrRegs a
uses (IdivR _ r)         = singleton r
uses (DivR _ r)          = singleton r
uses Cqo{}               = IS.empty -- TODO?
uses (AndRR _ r r')      = fromList [r, r']
uses (OrRR _ r r')       = fromList [r, r']
uses (PopcountRR _ _ r') = singleton r'
uses (NegR _ r)          = singleton r
uses _                   = IS.empty

defs :: X86 AbsReg ann -> IS.IntSet
defs (MovRA _ r _)      = singleton r
defs (MovRR _ r _)      = singleton r
defs (MovRRLower _ r _) = singleton r
defs (MovRC _ r _)      = singleton r
defs (MovRCBool _ r _)  = singleton r
defs (MovRCi8 _ r _)    = singleton r
defs (MovRWord _ r _)   = singleton r
defs (AddRR _ r _)      = singleton r
defs (SubRR _ r _)      = singleton r
defs (ImulRR _ r _)     = singleton r
defs (AddRC _ r _)      = singleton r
defs (SubRC _ r _)      = singleton r
defs (XorRR _ r _)      = singleton r
defs (MovRL _ r _)      = singleton r
defs (MovRLK _ r _)     = singleton r
defs (LShiftRRR _ r _)  = singleton r
defs (PopReg _ r)       = singleton r
defs (LShiftLRR _ r _)  = singleton r
defs (AShiftRRR _ r _)  = singleton r
defs (AndRR _ r _)      = singleton r
defs (OrRR _ r _)       = singleton r
defs (PopcountRR _ r _) = singleton r
defs (NegR _ r)         = singleton r
defs (MovRCTag _ r _)   = singleton r
-- defs for IdivR &c.?
defs _                  = IS.empty

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
