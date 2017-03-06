{-# LANGUAGE GeneralizedNewtypeDeriving, UnboxedTuples, RecordWildCards, MultiParamTypeClasses, FlexibleContexts, NamedFieldPuns, MagicHash #-}
module Codec.JVM.ASM.Code.Instr where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.List(scanl')
import Data.Maybe(fromMaybe)
import GHC.Base

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow, Stack, VerifType(..), Stack(..))
import Codec.JVM.ASM.Code.Types
import Codec.JVM.Const (Const)
import Codec.JVM.Internal (packI16, packI32)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Types

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

data InstrState =
  InstrState { isByteCode      :: !ByteString
             , isStackMapTable :: StackMapTable
             , isOffset        :: !Offset
             , isCtrlFlow      :: CtrlFlow
             , isLabelTable    :: LabelTable
             , isLastGoto      :: Maybe Int
             , isLastReturn    :: Maybe Int }

newtype InstrM a = InstrM { runInstrM :: ConstPool -> InstrState -> (# a, InstrState #) }

newtype Instr = Instr { unInstr :: InstrM () }

instance Functor InstrM where
  fmap = liftM

instance Applicative InstrM where
  pure = return
  (<*>) = ap

instance Monad InstrM where
  return x = InstrM $ \_ s -> (# x, s #)
  (InstrM m) >>= f =
    InstrM $ \e s ->
      case m e s of
        (# x, s' #) ->
          case runInstrM (f x) e s' of
            (# x', s'' #) -> (# x', s'' #)

instance MonadIO InstrM where
  liftIO (IO io) = InstrM $ \_ s ->
    case io realWorld# of
      (# _, a #) -> (# a, s #)

instance MonadState InstrState InstrM where
  get = InstrM $ \_ s -> (# s, s #)
  put s' = InstrM $ \_ _ -> (# (), s' #)

instance MonadReader ConstPool InstrM where
  ask = InstrM $ \e s -> (# e, s #)
  local f (InstrM m) = InstrM $ \e s -> m (f e) s

instance Monoid Instr where
  mempty = Instr $ return ()
  mappend (Instr rws0) (Instr rws1) = Instr $ do
    rws0
    rws1

instance Show Instr where
  show _ = "Instructions"

withOffset :: (Int -> Instr) -> Instr
withOffset f = Instr $ do
  InstrState { isOffset = Offset offset } <- get
  unInstr $ f offset

emptyInstrState :: InstrState
emptyInstrState =
  InstrState { isByteCode = mempty
             , isStackMapTable = mempty
             , isOffset = 0
             , isCtrlFlow = CF.empty
             , isLabelTable = mempty
             , isLastGoto = Nothing
             , isLastReturn = Nothing }

getBCS :: InstrState -> (ByteString, CtrlFlow, StackMapTable)
getBCS InstrState{..} = (isByteCode, isCtrlFlow, isStackMapTable)

runInstr :: Instr -> ConstPool -> InstrState
runInstr instr cp = multiPass 3 emptyInstrState
  where multiPass :: Int -> InstrState -> InstrState
        multiPass 0 s = s
        multiPass n InstrState { isLabelTable = lt } =
          case runInstr' instr cp $ emptyInstrState { isLabelTable = lt } of
            s' -> multiPass (n - 1) s'

runInstrBCS :: Instr -> ConstPool -> (ByteString, CtrlFlow, StackMapTable)
runInstrBCS instr cp = getBCS $ runInstr instr cp

runInstrWithLabels :: Instr -> ConstPool -> Offset -> CtrlFlow -> LabelTable -> InstrState
runInstrWithLabels instr cp offset cf lt = runInstr' instr cp s
  where s = emptyInstrState { isOffset = offset
                            , isCtrlFlow = cf
                            , isLabelTable = lt }

runInstrWithLabelsBCS :: Instr -> ConstPool -> Offset -> CtrlFlow -> LabelTable
                      -> (ByteString, CtrlFlow, StackMapTable)
runInstrWithLabelsBCS instr cp offset cf lt = getBCS $ runInstrWithLabels instr cp offset cf lt


runInstr' :: Instr -> ConstPool -> InstrState -> InstrState
runInstr' (Instr m) e s@InstrState { isOffset = Offset off } =
  case runInstrM m e s of
    (# (), s'@InstrState { isLastReturn, isLastGoto} #)
      -> s' { isLastReturn = fmap subInitOffset isLastReturn
            , isLastGoto   = fmap subInitOffset isLastGoto }
  where subInitOffset x = x - off

runInstrBCS' :: Instr -> ConstPool -> InstrState -> (ByteString, CtrlFlow, StackMapTable)
runInstrBCS' instr e s = getBCS $ runInstr' instr e s

recordGoto :: InstrM ()
recordGoto = do
  off <- getOffset
  modify' $ \s -> s { isLastGoto = Just off }

recordReturn :: InstrM ()
recordReturn = do
  off <- getOffset
  modify' $ \s -> s { isLastReturn = Just off }

gotoInstr :: InstrM ()
gotoInstr = do
  recordGoto
  op' OP.goto

gotoWInstr :: InstrM ()
gotoWInstr = do
  recordGoto
  op' OP.goto_w

gotoInstrGen :: Int -> InstrM ()
gotoInstrGen offset = do
  if outsideGotoRange offset
  then do
    gotoWInstr
    writeBytes . packI32 $ offset
  else do
    gotoInstr
    writeBytes . packI16 $ offset

returnInstr :: Opcode -> Instr
returnInstr opc = Instr $ do
  recordReturn
  op' opc

modifyStack' :: (Stack -> Stack) -> InstrM ()
modifyStack' f = ctrlFlow' $ CF.mapStack f

modifyStack :: (Stack -> Stack) -> Instr
modifyStack = Instr . modifyStack'

gbranch :: (FieldType -> Stack -> Stack)
        -> FieldType -> Opcode -> Instr -> Instr -> Instr
gbranch f ft oc ok ko = Instr $ do
  lengthOp <- writeInstr ifop
  InstrState { isCtrlFlow = cf } <- get
  branches False cf lengthOp ok ko
  where ifop = op oc <> modifyStack (f ft)

-- TODO: This function fails for huge methods, must make it safe
--       when goto offset is outside of −32,768 to 32,767
--       which isn't likely to happen.
branches :: Bool -> CtrlFlow -> Int -> Instr -> Instr -> InstrM ()
branches useGotoW cf lengthOp ok ko = do
  InstrState { isByteCode = koBytes
             , isCtrlFlow = koCF
             , isLabelTable = koLabels
             , isStackMapTable = koFrames
             , isLastGoto = mGoto
             , isLastReturn = mReturn } <- pad lengthOperand ko
  let hasGoto = ifLastBranch mGoto mReturn koBytes
      lengthJumpOK
        | hasGoto = 0
        | useGotoW = 5
        | otherwise = 3
      jumpOffset = BS.length koBytes + lengthJumpOK + lengthOp + lengthOperand
  InstrState { isByteCode = okBytes
             , isCtrlFlow = okCF
             , isLabelTable = okLabels
             , isStackMapTable = okFrames } <- pad (jumpOffset - lengthOp) ok
  let koJumpOffset = BS.length okBytes + lengthJumpOK
  if not hasGoto &&  outsideGotoRange koJumpOffset && not useGotoW
  then branches True cf lengthOp ok ko
  else do
    writeBytes . packI16 $ jumpOffset
    write koBytes koFrames
    unless hasGoto $ do
      op' (if useGotoW then OP.goto_w else OP.goto)
      writeBytes . (if useGotoW then packI32 else packI16)
                 $ BS.length okBytes + lengthJumpOK
    writeStackMapFrame
    write okBytes okFrames
    putCtrlFlow' $ CF.merge cf [okCF, koCF]
    mergeLabels [koLabels, okLabels]
    writeStackMapFrame
  where pad padding instr = do
          cp <- ask
          InstrState { isOffset = Offset offset
                     , isCtrlFlow = cf'
                     , isLabelTable = lt } <- get
          return $ runInstrWithLabels instr cp (Offset $ offset + padding) cf' lt
        lengthOperand = 2 -- Size of offset for ok branch

bytes :: ByteString -> Instr
bytes = Instr . writeBytes

ix :: Const -> Instr
ix c = Instr $ do
  cp <- ask
  writeBytes . packI16 $ CP.ix $ CP.unsafeIndex c cp

op :: Opcode -> Instr
op = Instr . op'

op' :: Opcode -> InstrM ()
op' = writeBytes . BS.singleton . opcode

ctrlFlow' :: (CtrlFlow -> CtrlFlow) -> InstrM ()
ctrlFlow' f = modify' $ \s@InstrState { isCtrlFlow = cf }  -> s { isCtrlFlow = f cf }

ctrlFlow :: (CtrlFlow -> CtrlFlow) -> Instr
ctrlFlow = Instr . ctrlFlow'

withStack :: ([VerifType] -> [VerifType]) -> Instr
withStack f = modifyStack (\s -> let stack'     = f (stackVal s)
                                     stackSize' = length stack'
                                 in s { stackVal  = stack'
                                      , stackMax  = max (stackMax s) stackSize'
                                      , stackSize = stackSize' })

initCtrl :: (CtrlFlow -> CtrlFlow) -> Instr
initCtrl f = Instr $ do
  unInstr $ ctrlFlow f
  writeStackMapFrame

putCtrlFlow :: CtrlFlow -> Instr
putCtrlFlow = Instr . putCtrlFlow'

putCtrlFlow' :: CtrlFlow -> InstrM ()
putCtrlFlow' = ctrlFlow' . const

withCtrlFlowAndLabels :: CtrlFlow -> LabelTable -> InstrM () -> InstrM (CtrlFlow, LabelTable)
withCtrlFlowAndLabels cf lt instr = do
  InstrState { isCtrlFlow = cf', isLabelTable = lt' } <- get
  modify' $ \s -> s { isCtrlFlow = cf, isLabelTable = lt }
  instr
  s' <- get
  modify' $ \s -> s { isCtrlFlow = cf', isLabelTable = lt' }
  return (isCtrlFlow s', isLabelTable s')

incOffset :: Int -> Instr
incOffset = Instr . incOffset'

incOffset' :: Int -> InstrM ()
incOffset' i =
  modify' $ \s@InstrState { isOffset = Offset off } ->
              s { isOffset = Offset $ off + i}

write :: ByteString -> StackMapTable -> InstrM ()
write bs smfs = do
  incOffset' $ BS.length bs
  modify' $ \s@InstrState { isByteCode = bs'
                          , isStackMapTable = smfs' } ->
    s { isByteCode = bs' <> bs
      , isStackMapTable = smfs' <> smfs }

writeBytes :: ByteString -> InstrM ()
writeBytes bs = write bs mempty

writeInstr :: Instr -> InstrM Int
writeInstr (Instr action) = do
  off0 <- getOffset
  action
  off1 <- getOffset
  return $ off1 - off0

markStackMapFrame :: Instr
markStackMapFrame = Instr writeStackMapFrame

writeStackMapFrame :: InstrM ()
writeStackMapFrame = do
  modify' $ \s@InstrState { isOffset = Offset offset
                          , isCtrlFlow = cf
                          , isStackMapTable = smt } ->
    s { isStackMapTable = insertSMT offset cf smt }

getOffset :: InstrM Int
getOffset = do
  Offset offset <- gets isOffset
  return offset

-- TODO: Unify tableswitch with lookupswitch
type BranchMap = IntMap.IntMap Instr

tableswitch :: Int -> Int -> BranchMap -> Maybe Instr -> Instr
tableswitch low high branchMap deflt = Instr $ do
  cp <- ask
  baseOffset <- getOffset
  _ <- writeInstr $ op OP.tableswitch
  modifyStack' $ CF.pop jint
  InstrState { isOffset = Offset offset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  --(Offset offset, cf, lt) <- get
  -- Align to 4-byte boundary
  let padding = (4 - (offset `mod` 4)) `mod` 4
  writeBytes . BS.pack . replicate padding $ 0
  offset' <- getOffset
  let firstOffset = offset' + 4 * (3 + numBranches)
      (offsets, codeInfos) = unzip . tail $ scanl' (computeOffsets cf cp lt) (firstOffset, undefined) [low..high]
      defOffset = last offsets
      defInstr = fromMaybe mempty deflt
      istate@InstrState { isLabelTable = defLt }
        = runInstrWithLabels defInstr cp (Offset defOffset) cf lt
      (defBytes, defCF, defFrames) = getBCS istate
      breakOffset = defOffset + BS.length defBytes
      relOffset x = x - baseOffset
  writeBytes . packI32 $ relOffset defOffset
  writeBytes . packI32 $ low
  writeBytes . packI32 $ high
  forM_ codeInfos $ \(codeOffset, _, _, _, _, _, _) ->
    writeBytes . packI32 $ relOffset codeOffset
  forM_ codeInfos $ \(codeOffset, len, bytes', _, frames, shouldJump, _) -> do
    writeStackMapFrame
    if len == 0 then do
      gotoInstr
      writeBytes . packI16 $ (defOffset - codeOffset)
    else do
      write bytes' frames
      when shouldJump $ do
        op' OP.goto
        writeBytes . packI16 $ (breakOffset - (codeOffset + len))
  writeStackMapFrame
  write defBytes defFrames
  putCtrlFlow' $ CF.merge cf (defCF : map (\(_, _, _, cf', _, _, _) -> cf') codeInfos)
  mergeLabels $ defLt : map (\(_, _, _, _, _, _, lt') -> lt') codeInfos
  writeStackMapFrame
  where computeOffsets cf cp lt (offset, _) i =
          ( offset + bytesLength + lengthJump
          , (offset, bytesLength, bytes', cf', frames, not hasGoto, lt') )
          where istate@InstrState { isLastGoto, isLastReturn, isLabelTable = lt' }
                  = runInstrWithLabels instr cp (Offset offset) cf lt
                (bytes', cf', frames) = getBCS istate
                instr = IntMap.findWithDefault mempty i branchMap
                bytesLength = BS.length bytes'
                hasGoto = ifLastBranch isLastGoto isLastReturn bytes'
                lengthJump = if hasGoto then 0 else 3 -- op goto <> pack16 $ length ko
        numBranches = high - low + 1

lookupswitch :: BranchMap -> Maybe Instr -> Instr
lookupswitch branchMap deflt = Instr $ do
  cp <- ask
  baseOffset <- getOffset
  _ <- writeInstr $ op OP.lookupswitch
  modifyStack' $ CF.pop jint
  InstrState { isOffset = Offset offset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  --(Offset offset, cf, lt) <- get
  -- Align to 4-byte boundary
  let padding = (4 - (offset `mod` 4)) `mod` 4
  writeBytes . BS.pack . replicate padding $ 0
  offset' <- getOffset
  let firstOffset = offset' + 4 * (2 + 2 * numBranches)
      (offsets, codeInfos) = unzip . tail $ scanl' (computeOffsets cf cp lt) (firstOffset, undefined) $ IntMap.toAscList branchMap
      defOffset = last offsets
      defInstr = fromMaybe mempty deflt
      istate@InstrState { isLabelTable = defLt }
        = runInstrWithLabels defInstr cp (Offset defOffset) cf lt
      (defBytes, defCF, defFrames) = getBCS istate
      breakOffset = defOffset + BS.length defBytes
      relOffset x = x - baseOffset
  writeBytes . packI32 $ relOffset defOffset
  writeBytes . packI32 $ length codeInfos
  forM_ codeInfos $ \(codeOffset, _, val, _, _, _, _, _) -> do
    writeBytes . packI32 $ val
    writeBytes . packI32 $ relOffset codeOffset
  forM_ codeInfos $ \(codeOffset, len, _, bytes', _, frames, shouldJump, _) -> do
    writeStackMapFrame
    write bytes' frames
    when shouldJump $ do
      op' OP.goto -- special gotos
      writeBytes . packI16 $ (breakOffset - (codeOffset + len))
  writeStackMapFrame
  write defBytes defFrames
  putCtrlFlow' $
    CF.merge cf (defCF : map (\(_, _, _, _, cf', _, _, _) -> cf') codeInfos)
  mergeLabels $ defLt : map (\(_, _, _, _, _, _, _, lt') -> lt') codeInfos
  writeStackMapFrame
  where computeOffsets cf cp lt (offset, _) (val, instr) =
          ( offset + bytesLength + lengthJump
          , (offset, bytesLength, val, bytes', cf', frames, not hasGoto, lt') )
          where istate@InstrState { isLastGoto, isLastReturn, isLabelTable = lt' }
                  = runInstrWithLabels instr cp (Offset offset) cf lt
                (bytes', cf', frames) = getBCS istate
                bytesLength = BS.length bytes'
                hasGoto = ifLastBranch isLastGoto isLastReturn bytes'
                lengthJump = if hasGoto then 0 else 3 -- op goto <> pack16 $ length ko
        numBranches = IntMap.size branchMap


lookupLabel :: Label -> InstrM Offset
lookupLabel l = do
  InstrState { isLabelTable = lt } <- get
  return $ lookupLT l lt

mergeLabels :: [LabelTable] -> InstrM ()
mergeLabels tables = do
  InstrState { isLabelTable = table } <- get
  modify' $ \s@InstrState { isLabelTable = table } ->
    s { isLabelTable = unionsLT (table : map (`differenceLT` table) tables) }

gotoLabel :: Label -> Instr
gotoLabel label = Instr $ do
  offset <- getOffset
  Offset labelOffset <- lookupLabel label
  gotoInstrGen $ labelOffset - offset

condGoto :: Label -> Instr
condGoto l = Instr $ do
  InstrState { isByteCode = bytes
             , isLastGoto = mGoto
             , isLastReturn = mReturn } <- get
  unless (ifLastBranch mGoto mReturn bytes) $ unInstr (gotoLabel l)

putLabel :: Label -> Instr
putLabel l = Instr $ do
  modify' $ \s@InstrState { isLabelTable = lt
                          , isOffset = off } ->
              s { isLabelTable = insertLT l off lt }
  writeStackMapFrame

addLabels :: [(Label, Offset)] -> InstrM ()
addLabels labelOffsets = modify' f
  where f s@InstrState { isLabelTable = table } =
          s { isLabelTable = table' }
          where table' = table <> toLT labelOffsets

ifLastBranch :: Maybe Int -> Maybe Int -> ByteString -> Bool
ifLastBranch mGoto mReturn bs = maybe False (\x -> x == gotoIndex || x == gotoWIndex) mGoto
                             || maybe False (== returnIndex) mReturn
  where returnIndex = BS.length bs - 1
        gotoIndex = returnIndex - 2
        gotoWIndex = gotoIndex - 2

outsideGotoRange :: Int -> Bool
outsideGotoRange offset = offset > 32767 || offset < -32768
