{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- FIXME: this module is slow

-- | Based on the Appel book.
module Kempe.Asm.X86.Liveness ( mkLiveness
                              , Liveness
                              , LivenessMap
                              ) where

import           Control.Arrow       ((***))
import           Control.Composition (thread)
import           Control.DeepSeq     (NFData)
-- this seems to be faster
import qualified Data.IntMap.Lazy    as IM
import           Data.Semigroup      ((<>))
import qualified Data.Set            as S
import           GHC.Generics        (Generic)
import           Kempe.Asm.X86.Type

data Liveness = Liveness { ins :: S.Set AbsReg, out :: S.Set AbsReg }
    deriving (Eq, Generic, NFData)

emptyLiveness :: Liveness
emptyLiveness = Liveness S.empty S.empty

-- need: succ for a node

initLiveness :: [X86 reg ControlAnn] -> LivenessMap
initLiveness = IM.fromList . fmap (\asm -> let x = ann asm in (node x, (x, emptyLiveness)))

type LivenessMap = IM.IntMap (ControlAnn, Liveness)

-- i :: X86 AbsReg (ControlAnn, Liveness) -> Int
-- i = node . fst . ann

-- | All program points accessible from some node.
succNode :: ControlAnn -- ^ 'ControlAnn' associated w/ node @n@
         -> LivenessMap
         -> [Liveness] -- ^ 'Liveness' associated with 'succNode' @n@
succNode x ns =
    let conns = conn x
        in fmap (snd . flip lookupNode ns) conns

lookupNode :: Int -> LivenessMap -> (ControlAnn, Liveness)
lookupNode = IM.findWithDefault (error "Internal error: failed to look up instruction")

done :: LivenessMap -> LivenessMap -> Bool
done n0 n1 = {-# SCC "done" #-} and $ IM.map (uncurry (==) . (snd *** snd)) $ IM.intersectionWith (,) n0 n1

-- order in which to inspect nodes during liveness analysis
inspectOrder :: [X86 reg ControlAnn] -> [Int]
inspectOrder = fmap (node . ann) -- don't reverse because thread goes in opposite order

mkLiveness :: [X86 reg ControlAnn] -> LivenessMap
mkLiveness asms = liveness is (initLiveness asms)
    where is = inspectOrder asms

liveness :: [Int] -> LivenessMap -> LivenessMap
liveness is nSt =
    if done nSt nSt'
        then nSt
        else liveness is nSt'
    where nSt' = iterNodes is nSt

iterNodes :: [Int] -> LivenessMap -> LivenessMap
iterNodes is = thread (fmap stepNode is)

stepNode :: Int -> LivenessMap -> LivenessMap
stepNode n ns = {-# SCC "stepNode" #-} IM.insert n (c, Liveness ins' out') ns
    where (c, l) = lookupNode n ns
          ins' = usesNode c <> (out l S.\\ defsNode c)
          out' = S.unions (fmap ins (succNode c ns))
