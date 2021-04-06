-- FIXME: this module is slow

-- | Based on the Appel book.
module Kempe.Asm.Liveness ( Liveness
                          , reconstruct
                          ) where

import           Control.Composition (thread)
import           Data.Copointed
-- this seems to be faster
import qualified Data.IntMap.Lazy    as IM
import qualified Data.IntSet         as IS
import           Data.Semigroup      ((<>))
import           Kempe.Asm.Type

emptyLiveness :: Liveness
emptyLiveness = Liveness IS.empty IS.empty

initLiveness :: Copointed p => [p ControlAnn] -> LivenessMap
initLiveness = IM.fromList . fmap (\asm -> let x = copoint asm in (node x, (x, emptyLiveness)))

type LivenessMap = IM.IntMap (ControlAnn, Liveness)

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
done n0 n1 = {-# SCC "done" #-} and $ zipWith (\(_, l) (_, l') -> l == l') (IM.elems n0) (IM.elems n1) -- should be safe b/c n0, n1 must have same length

-- order in which to inspect nodes during liveness analysis
inspectOrder :: Copointed p => [p ControlAnn] -> [Int]
inspectOrder = fmap (node . copoint) -- don't need to reverse because thread goes in opposite order

reconstruct :: (Copointed p, Functor p) => [p ControlAnn] -> [p Liveness]
reconstruct asms = {-# SCC "reconstructL" #-} fmap (fmap lookupL) asms
    where l = {-# SCC "mkLiveness" #-} mkLiveness asms
          lookupL x = snd $ lookupNode (node x) l

mkLiveness :: Copointed p => [p ControlAnn] -> LivenessMap
mkLiveness asms = liveness is (initLiveness asms)
    where is = inspectOrder asms

liveness :: [Int] -> LivenessMap -> LivenessMap
liveness is nSt =
    if done nSt nSt'
        then nSt
        else liveness is nSt'
    where nSt' = {-# SCC "iterNodes" #-} iterNodes is nSt

iterNodes :: [Int] -> LivenessMap -> LivenessMap
-- this is fickle, thread will seemingly thunk leak (??) if optimizations aren't on
iterNodes is = thread (fmap stepNode is)

stepNode :: Int -> LivenessMap -> LivenessMap
stepNode n ns = {-# SCC "stepNode" #-} IM.insert n (c, Liveness ins' out') ns
    where (c, l) = lookupNode n ns
          ins' = usesNode c <> (out l IS.\\ defsNode c)
          out' = IS.unions (fmap ins (succNode c ns))
