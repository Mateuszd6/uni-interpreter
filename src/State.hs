{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module State where

import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AbsLanguage
import Common
import Error
import Parser

type VarId = Int
type FunId = Int
type TypeId = Int

type Struct = Map.Map String Var

data Var
  = VEmpty
  | VInt Int
  | VBool Bool
  | VString String
  | VTuple [Var]
  | VStruct { vStructTId :: TypeId, vStructData :: Struct }

data VarInfo = VarInfo
  {
    viScopeN :: Int, -- Scope in which var was declared - prevents redeclaration
    viIsReadOnly :: Bool
  }
  deriving (Show)

instance Show Var where
  show VEmpty = "void"
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show VStruct { } = "Struct" -- Language does not support printing structs and tuples.
  show VTuple { } = "Tuple"

data StructDef = StructDef { strctName :: String,
                             strctFields :: Map.Map String TypeId }
  deriving (Show)

type Param = (String, VarSpec PPos, TypeId)

data FRetT
  = FRetTSinge TypeId
  | FRetTTuple [TypeId]
  deriving (Show)

data Func = Func
  {
    funcId :: FunId,
    funcBody :: Stmt PPos,
    funcRetT :: FRetT,
    funcParams :: [Param],
    funcBind :: Maybe (Set.Set VarId),
    funcScope :: Scope,
    fScopeN :: Int
  }
  deriving (Show)

-- The state contains two separate things: store, which holds variable states by their IDs
-- and State which maps names to IDs. This way we can handle static binding of function
-- parameters, var hiding etc.
data State = State
  {
    scopeCnt :: Int,
    stateStore :: Store,
    stateScope :: Scope,
    bindVars :: [(Int, Set.Set VarId)],
    currRetT :: Maybe
    FRetT
  }
  deriving (Show)

data Store = Store
  {
    storeVars :: Map.Map VarId (Var, VarInfo),
    storeFuncs :: Map.Map FunId Func,
    storeTypes :: Map.Map TypeId StructDef,

    nextVarId :: Int,
    nextFuncId :: Int,
    nextTypeId :: Int
  }
  deriving (Show)

data Scope = Scope
  {
    scopeVars :: Map.Map String VarId,
    scopeFuncs :: Map.Map String FunId,
    scopeTypes :: Map.Map String TypeId
  }
  deriving (Show)

-- Avoid hardcodeing typeids.
voidT :: TypeId
voidT = 0
intT :: TypeId
intT = 1
boolT :: TypeId
boolT = 2
stringT :: TypeId
stringT = 3
tupleT :: TypeId
tupleT = 4
firstFreeNotBulitinT :: TypeId
firstFreeNotBulitinT = 5

-- Commonly used aliases to get state members
varsScope :: State -> Map.Map String VarId
varsScope = scopeVars . stateScope
funcsScope :: State -> Map.Map String FunId
funcsScope = scopeFuncs . stateScope
typesScope :: State -> Map.Map String TypeId
typesScope = scopeTypes . stateScope
varsStore :: State -> Map.Map VarId (Var, VarInfo)
varsStore = storeVars . stateStore
funcsStore :: State -> Map.Map FunId Func
funcsStore = storeFuncs . stateStore
typesStore :: State -> Map.Map TypeId StructDef
typesStore = storeTypes . stateStore

initialState :: State
initialState = State
                 0
                 (Store Map.empty Map.empty Map.empty 1 1 5)
                 (Scope Map.empty Map.empty Map.empty)
                 [(-1, Set.fromList [])]
                 Nothing

-- CtrlT moand, which nests another monad in Error Monad (wrapped so that we can have
-- returns/breaks). Basically used only with IO monad in the entire project...
data ExcType
  = ExBreak PPos
  | ExContinue PPos
  | ExReturn PPos Var
  deriving (Show)

instance Pos ExcType where
  getPos (ExBreak pos) = pos
  getPos (ExContinue pos) = pos
  getPos (ExReturn pos _) = pos

-- There are no exceptions, but return/break etc. statements sort of function like that.
data CtrlFlow a
  = CtrlRegular (Error a)
  | CtrlException ExcType State
  deriving (Show)

instance Functor CtrlFlow where
  fmap f (CtrlRegular a) = CtrlRegular $ f <$> a
  fmap _ (CtrlException r s) = CtrlException r s

newtype CtrlT m a = CtrlT { runCtrlT :: m (CtrlFlow a) }

instance MonadTrans CtrlT where
  lift = CtrlT . fmap (CtrlRegular . Ok)

instance (Monad m) => Monad (CtrlT m) where
  return = CtrlT . return . CtrlRegular . Ok

  x >>= f = CtrlT $ do
    v <- runCtrlT x
    case v of
      CtrlRegular (Fail rs) -> return . CtrlRegular $ Fail rs
      CtrlRegular (Ok w) -> runCtrlT (f w)
      CtrlException r s -> return (CtrlException r s)

instance (Functor m) => Functor (CtrlT m) where
  fmap f = CtrlT . fmap (fmap f) . runCtrlT

instance (Functor m, Monad m) => Applicative (CtrlT m) where
  pure = CtrlT . return . CtrlRegular . Ok

  mf <*> mx = CtrlT $ do
    mb_f <- runCtrlT mf
    case mb_f of
      CtrlException r s -> return (CtrlException r s)
      CtrlRegular (Fail rs) -> return $ CtrlRegular (Fail rs)
      CtrlRegular (Ok f) -> do
        mb_x <- runCtrlT mx
        case mb_x of
          CtrlException r s -> return (CtrlException r s)
          CtrlRegular (Fail rs) -> return $ CtrlRegular (Fail rs)
          CtrlRegular (Ok x) -> return $ CtrlRegular (Ok $ f x)

-- Promote regular Error into CtrlT. This is used a lot because most functions won't
-- change the flow (won't return or something similar), so they return error. But things
-- like statements do, so anything just returning errors is wrapped with this.
toCtrlT :: Monad m => Error a -> CtrlT m a
toCtrlT = CtrlT . return . CtrlRegular

-- Used only once at the end of the program. Basically, don't allow program to
-- be in 'exception thrown' state, but treat is as an error.
ctrlToError :: CtrlFlow a -> Error a
ctrlToError (CtrlRegular err) = err
ctrlToError (CtrlException expType _) = Fail $
  EDUncoughtedExc (show expType) (getPos expType)

-- Create new variable and add it to the state.
createVar :: String -> Bool -> Var -> PPos -> State -> Error (VarId, State)
createVar name rdOnly v p s@(State c
                                   str@(Store vars _ _ next _ _)
                                   scp@(Scope vnames _ _) _ _) =
  let varInfo = VarInfo (scopeCnt s) rdOnly
      storeVars' = Map.insert next (v, varInfo) vars
  in do
    enforceNotAlreadyDefined name (viScopeN . snd) vnames vars c (EDVarAlreadyDeclared p)
    return (next, s{
               stateStore = str{ storeVars = storeVars', nextVarId = next + 1 },
               stateScope = scp{ scopeVars = Map.insert name next vnames } })

createFunc :: String -> Stmt PPos -> [Param] -> FRetT -> Maybe (Set.Set VarId) ->
              PPos -> State -> Error (FunId, State)
createFunc name body params ret bind p st@(State c str@Store{ nextFuncId = next } _ _ _) =
  let scp' = (stateScope st){ scopeFuncs = Map.insert name next (funcsScope st) }
      storeFuncs' = Map.insert next func (funcsStore st)
      func = Func next body ret params bind scp' c
  in do
    enforceNotAlreadyDefined name fScopeN (funcsScope st) (funcsStore st)
                             c (EDFuncAlreadyDeclared p)
    enforceNotParamRepeated (map fst3 params) (flip EDFuncArgRepeated p)
    return (next, st{
               stateStore = str{ storeFuncs = storeFuncs', nextFuncId = next + 1 },
               stateScope = scp' })

createStruct :: String -> PPos -> [(String, TypeId)] -> State -> Error (TypeId, State)
createStruct name p fields st@(State _ str@Store { nextTypeId = next } _ _ _) =
  let storeTypes' = Map.insert next (StructDef name $ Map.fromList fields)(storeTypes str)
      scopeTypes' = Map.insert name next (typesScope st)
  in do
    enforceNotParamRepeated (map fst fields) (flip EDStructArgRepeated p)
    return (next, st{
               stateStore = str{ storeTypes = storeTypes', nextTypeId = next + 1 },
               stateScope = (stateScope st){ scopeTypes = scopeTypes' } })

getVarImpl :: VarId -> String -> PPos -> State -> Error (Var, VarInfo)
getVarImpl vId vname p st = do
  (var, vinfo) <- errorFromMaybe (EDVarNotFound vname p) $
                  Map.lookup vId (varsStore st)
  enforceBind vId (viScopeN vinfo) vname p st
  return (var, vinfo)

-- Get variable by name
getVar :: String -> PPos -> State -> Error (VarId, Var)
getVar vname p st = do
  vId <- errorFromMaybe (EDVarNotFound vname p) $
         Map.lookup vname (varsScope st)

  (var, _) <- getVarImpl vId vname p st
  return (vId, var)

getFunc :: String -> PPos -> State -> Error (FunId, Func)
getFunc fname p st = errorFromMaybe (EDFuncNotFound fname p) $
  do
    fId <- Map.lookup fname (funcsScope st)
    func <- Map.lookup fId (funcsStore st)
    return (fId, func)

parseRetType :: FuncRetT PPos -> State -> Error FRetT
parseRetType (FRTEmpty _) _ = return $ FRetTSinge 0
parseRetType (FRTSingle _ t) st = FRetTSinge <$> getTypeId t st
parseRetType (FRTTuple _ types) st = FRetTTuple <$>
  foldrM (\a b -> flip (:) b <$> getTypeId a st) [] types

funcReturnsVoid :: FRetT -> Bool
funcReturnsVoid (FRetTSinge 0) = True
funcReturnsVoid _ = False

funcToParams :: FunParams PPos -> State -> Error [Param]
funcToParams (FPEmpty _) _ = return []
funcToParams (FPList _ declParams) st =
  mapM (\(DDeclBasic _ (Ident n) spec t) -> (n, spec, ) <$> getTypeId t st) declParams

asgnFieldsToList :: AsgnFields PPos -> [(String, Expr PPos)]
asgnFieldsToList (AFEmpty _) = []
asgnFieldsToList (AFList _ newfieldasgns) = map (\(NFADefault _ (Ident n) e) -> (n, e))
                                                newfieldasgns

getTypeStruct :: String -> PPos -> State -> Error (TypeId, StructDef)
getTypeStruct name p st = errorFromMaybe (EDTypeNotFound name p) $
  do
    tId <- Map.lookup name (typesScope st)
    strct <- Map.lookup tId (typesStore st)
    return (tId, strct)

-- This function does not perform the type check!!
setVar :: VarId -> Var -> PPos -> State -> Error State
setVar vId val p s@(State _ str _ _ _) = do
  -- This should never fail to lookup the var, unless there is a bug. That's why we give a
  -- variable an artificial name.
  (_, info) <- getVarImpl vId ("ID=" ++ show vId) p s
  enforceVarIsNotReadOnly info p
  return s{ stateStore = str{ storeVars = Map.insert vId (val, info) $ storeVars str }}

getTypeId :: Type PPos -> State -> Error TypeId
getTypeId (TInt _) _ = return intT
getTypeId (TBool _) _ = return boolT
getTypeId (TString _) _ = return stringT
getTypeId (TUser p (Ident tname)) st =
  errorFromMaybe (EDTypeNotFound tname p) $
  Map.lookup tname (typesScope st)

getTypeDescr :: TypeId -> PPos -> State -> Error StructDef
getTypeDescr tId p st = errorFromMaybe (EDVarNotStruct p) $
  Map.lookup tId $ storeTypes $ stateStore st

-- Can't get the name from the scope throught reverse lookup, because the type can be
-- hidden, that's why we use strctName field.
getTypeName :: TypeId -> State -> String
getTypeName tId st
  | tId == 0 = "void"
  | tId == intT = "int"
  | tId == boolT = "bool"
  | tId == stringT = "string"
  | tId == tupleT = "tuple"
  | otherwise = strctName $ nofail $ getTypeDescr tId Nothing st

-- Can't fail, if we have a typeId it means there type actually exits.
defaultVarOfType :: State -> TypeId -> Var
defaultVarOfType st tId
  | tId == voidT = VEmpty
  | tId == intT = VInt 0
  | tId == boolT = VBool False
  | tId == stringT = VString ""
  | tId == tupleT = VTuple [] -- Should not happen
  | otherwise = VStruct tId $ Map.map (defaultVarOfType st) $ strctFields $
                        fromJust $ Map.lookup tId (typesStore st)

varTypeId :: Var -> TypeId
varTypeId VEmpty = voidT
varTypeId (VInt _) = intT
varTypeId (VBool _) = boolT
varTypeId (VString _) = stringT
varTypeId (VTuple _) = tupleT -- Tuple variables are only used when returning values.
varTypeId (VStruct sId _) = sId -- Structs know their typeIDs.

getStructFieldType :: TypeId -> String -> PPos -> State -> Error TypeId
getStructFieldType tId field p st = do
  strctDescr <- getTypeDescr tId p st
  errorFromMaybe (EDNoMember p (getTypeName tId st) field) $
                 Map.lookup field (strctFields strctDescr)

getStructFieldImpl :: (TypeId, Struct) -> String -> PPos -> State
                   -> Error (TypeId, Struct)
getStructFieldImpl (tId, struct) field p st = do
  destTypeId <- getStructFieldType tId field p st
  destVar <- errorFromMaybe (EDNoMember p (getTypeName tId st) field) $
             Map.lookup field struct
  (destTypeId, ) . snd <$> asStruct st p destVar

getStructField :: (TypeId, Struct) -> [String] -> PPos -> State -> Error Var
getStructField (tId, struct) [n] p st =
  errorFromMaybe (EDNoMember p (getTypeName tId st) n) $
    Map.lookup n struct

getStructField (tId, struct) (n:ns) p st = do
  (destTypeId, destStruct) <- getStructFieldImpl (tId, struct) n p st
  getStructField (destTypeId, destStruct) ns p st

getStructField _ [] _ _ = undefined -- Would not parse.

getStructMemebers :: StrcMembers PPos -> State -> Error [(String, TypeId)]
getStructMemebers (SMEmpty _) _ = return []
getStructMemebers (SMDefault _ members) st =
  foldrM (\(DStrMem _ (Ident name) tp) acc -> flip (:) acc . (name, ) <$> getTypeId tp st)
         [] members

-- First member is a list of accessed fields, second is a variable.
lvalueMem :: LValue PPos -> State -> Error ([String], (VarId, Var))
lvalueMem lv st = (vmemb, ) <$> getVar vname p st
  where
    lvalueMemImpl :: LValue PPos -> [String] -> ([String], PPos, String)
    lvalueMemImpl (LValueVar p0 (Ident name)) fs = (fs, p0, name)
    lvalueMemImpl (LValueMemb _ v (Ident name)) fs = lvalueMemImpl v $ name:fs
    (vmemb, p, vname) = lvalueMemImpl lv []

-- To avoid code duplication in scope and scope2.
scopeA :: Monad m =>
          (a -> State) -> (a -> State -> a) ->
          (State -> m a) -> Maybe FRetT ->
          Maybe (Set.Set VarId) -> State -> m a
scopeA getS setS fun fret bind st =
  let newBind = case bind of
                  Nothing -> bindVars st
                  Just set -> (scopeCnt st + 1, set) : bindVars st
      nextFRetT = (fret Control.Applicative.<|> currRetT st)
  in do
    retv <- fun st{ scopeCnt = scopeCnt st + 1, bindVars = newBind, currRetT = nextFRetT }
    return $ setS retv (getS retv){ scopeCnt = scopeCnt st,
                                    stateScope = stateScope st,
                                    bindVars = bindVars st,
                                    currRetT = currRetT st }
    -- Rollbacks scope and bind vars after leaving the scope.

scope :: (State -> CtrlT IO State) -> Maybe FRetT -> Maybe (Set.Set VarId) -> State
      -> CtrlT IO State
scope = scopeA id (\_ x -> x)

scope2 :: (State -> CtrlT IO (a, State)) -> Maybe FRetT -> Maybe (Set.Set VarId) -> State
       -> CtrlT IO (a, State)
scope2 = scopeA snd (\(x, _) z -> (x, z))

-- Convert vars to desired type or return a type error.
asInt :: State -> PPos -> Var -> Error Int
asInt _ _ (VInt v) = return v
asInt st p var = Fail $ EDTypeError "int" (getTypeName (varTypeId var) st) p

asBool :: State -> PPos -> Var -> Error Bool
asBool _ _ (VBool v) = return v
asBool st p var = Fail $ EDTypeError "bool" (getTypeName (varTypeId var) st) p

asString :: State -> PPos -> Var -> Error String
asString _ _ (VString v) = return v
asString st p var = Fail $ EDTypeError "string" (getTypeName (varTypeId var) st) p

asTuple :: State -> PPos -> Var -> Error [Var]
asTuple _ _ (VTuple v) = return v
asTuple st p var = Fail $ EDTypeError "tuple" (getTypeName (varTypeId var) st) p

asStruct :: State -> PPos -> Var -> Error (TypeId, Struct)
asStruct _ _ (VStruct tId str) = return (tId, str)
asStruct _ p _ = Fail $ EDVarNotStruct p

-- Fucntions to catch and throw CtrlException:
throw :: Monad m => ExcType -> State -> CtrlT m a
throw v st = CtrlT . return $ CtrlException v st

catch :: Monad m => (CtrlFlow a -> CtrlFlow b) -> CtrlT m a -> CtrlT m b
catch hndl ctrl = do
  curr <- lift $ runCtrlT ctrl
  CtrlT . return $ hndl curr

-- Since using catch directly isn't really handy, define some aux funcs.
catchBreak :: Monad m => CtrlT m State -> CtrlT m State
catchBreak = catch (\ctrl -> case ctrl of
    CtrlException (ExBreak _) s -> CtrlRegular $ Ok s
    _ -> ctrl)

catchContinue :: Monad m => CtrlT m State -> CtrlT m State
catchContinue = catch (\ctrl -> case ctrl of
    CtrlException (ExContinue _) s -> CtrlRegular $ Ok s
    _ -> ctrl)

expectReturnValue :: Monad m => FRetT -> PPos -> CtrlT m State -> CtrlT m (Var, State)
expectReturnValue retT p = catch (\case
    CtrlException (ExReturn p0 VEmpty) _ -> CtrlRegular $ Fail $ EDReturnVoid p0
    CtrlException (ExReturn p0 v) st' -> CtrlRegular $ enforceRetType v retT p0 st'
                                         >> return (v, st')
    CtrlRegular (Fail r) -> CtrlRegular $ Fail r
    _ -> CtrlRegular $ Fail $ EDNoReturn p)

-- Not the same as expectReturnValue, because we allow void functions, not to
-- return at all.
catchReturnVoid :: Monad m => PPos -> CtrlT m State -> CtrlT m (Var, State)
catchReturnVoid _ = catch (\case
    CtrlException (ExReturn _ VEmpty) st' -> CtrlRegular $ Ok (VEmpty, st')
    CtrlException (ExReturn p _) _ -> CtrlRegular $ Fail $ EDNoReturnNonVoid p
    CtrlRegular (Ok st') -> CtrlRegular $ Ok (VEmpty, st')
    CtrlException x y -> CtrlException x y
    CtrlRegular (Fail r) -> CtrlRegular $ Fail r)

dontAllowBreakContinue :: Monad m => CtrlT m a -> CtrlT m a
dontAllowBreakContinue = catch (\ctrl -> case ctrl of
    CtrlException (ExBreak p) _ -> CtrlRegular $ Fail $ EDUnexpectedBreak p
    CtrlException (ExContinue p) _ -> CtrlRegular $ Fail $ EDUnexpectedContinue p
    _ -> ctrl)

dontAllowReturn :: Monad m => CtrlT m a -> CtrlT m a
dontAllowReturn = catch (\ctrl -> case ctrl of
    CtrlException (ExReturn p _) _ -> CtrlRegular $ Fail $ EDUnexpectedReturn p
    _ -> ctrl)

-- Functions used to enforce some commonly changed conditions in the Error monad.
enforceParamLengthEqual :: PPos -> Int -> Int -> Error ()
enforceParamLengthEqual p expected got =
  when (expected /= got) $ Fail $ EDInvalidNumParams p expected got

-- Make sure the var is of the desired type or fail with a TypeError.
enforceType :: Var -> TypeId -> PPos -> State -> Error ()
enforceType v tId p st = when (varTypeId v /= tId) $ Fail $
  EDTypeError (getTypeName tId st) (getTypeName (varTypeId v) st) p

enforceReturnIsCorret :: Var -> FRetT -> PPos -> State -> Error ()
enforceReturnIsCorret v retT p st =
  case (v, retT) of
    (VTuple _, FRetTSinge _) -> Fail $ EDTupleReturned p
    (VTuple _, FRetTTuple _) -> enforceRetType v retT p st
    (_, FRetTTuple _) -> Fail $ EDValueReturned p
    (VEmpty, FRetTSinge tId) -> when (tId /= voidT) (Fail $ EDReturnVoid p)
    (_, FRetTSinge 0) -> Fail $ EDNoReturnNonVoid p
    _ -> enforceRetType v retT p st

enforceRetType :: Var -> FRetT -> PPos -> State -> Error ()
enforceRetType (VTuple _) (FRetTSinge _) p _ = Fail $ EDTupleReturned p
enforceRetType v (FRetTSinge tId) p st = enforceType v tId p st
enforceRetType (VTuple vars) (FRetTTuple types) p st = do
  zipped <- errorFromMaybe (EDTupleNumbersDontMatch p (length types) (length vars))
            $ tryZip types vars
  mapM_ (\(t, v) -> enforceType v t p st) zipped

enforceRetType _ (FRetTTuple _) p _ = Fail $ EDValueReturned p

enforce0div :: Int -> Int -> PPos -> Error ()
enforce0div _ d p = when (d == 0) $ Fail $ EDDivideByZero p

enforceVarIsNotVoid :: Var -> PPos -> Error ()
enforceVarIsNotVoid VEmpty p = Fail $ EDVarIsVoid p
enforceVarIsNotVoid _ _  = return ()

enforceIsScannable :: Type PPos -> Error ()
enforceIsScannable (TInt _) = return ()
enforceIsScannable (TBool _) = return ()
enforceIsScannable (TString _) = return ()
enforceIsScannable (TUser p (Ident n)) = Fail $ EDScanError n p

enforceBindedVarsAreInBlock :: String -> Func -> PPos -> State -> Error ()
enforceBindedVarsAreInBlock fname func p st = do
  bind <- errorFromMaybe (EDCantUseWiderBind fname p) $ funcBind func
  unless (setContainsAll (snd $ head $ bindVars st) bind) $
         Fail $ EDCantUseWiderBind fname p
  where
    setContainsAll :: Set.Set VarId -> Set.Set VarId -> Bool
    setContainsAll wider = foldr (\vId acc -> acc && Set.member vId wider) True

enforceFnCallBindRules :: String -> Func -> PPos -> State -> Error ()
enforceFnCallBindRules fname func p st =
  when (definedAt < lastBindAt) $ enforceBindedVarsAreInBlock fname func p st
  where
    definedAt = fScopeN func
    lastBindAt = fst $ head $ bindVars st

-- Check if var/func/type is aready defined for the scope.
enforceNotAlreadyDefined :: String -> (a -> Int) ->
                         Map.Map String Int -> Map.Map Int a -> Int ->
                         ErrorDetail -> Error ()
enforceNotAlreadyDefined name scopeN sscope sstore scCnt ed =
  when ((== Just True) maybeDefined) $ Fail ed
  where
    maybeDefined = do
      eId <- Map.lookup name sscope
      eScn <- scopeN <$> Map.lookup eId sstore
      return $ eScn == scCnt

-- Can be used to check func params or struct fields, it just requires an function that
-- can build an error objct from the name of a repeated thing.
enforceNotParamRepeated :: [String] -> (String -> ErrorDetail) -> Error ()
enforceNotParamRepeated params ed =
  let isParamRepeatedImpl :: [String] -> Maybe String
      isParamRepeatedImpl [] = Nothing
      isParamRepeatedImpl [_] = Nothing
      isParamRepeatedImpl (x:y:xys)
          | x == y = Just x
          | otherwise = isParamRepeatedImpl xys
  in case isParamRepeatedImpl $ sort params of
       Just rep -> Fail $ ed rep
       _ -> return ()

enforceBind :: VarId -> Int -> String -> PPos -> State -> Error ()
enforceBind vId scopeN n p (State _ _ _ ((i, set):_) _)
  | scopeN >= i = return () -- Variable declared insinde last bind block.
  | Set.member vId set = return () -- Variable binded in the curr bind scope.
  | otherwise = Fail $ EDBind n p
enforceBind _ _ _ _ (State _ _ _ [] _) = undefined -- We always have at least one bind

enforceVarIsNotReadOnly :: VarInfo -> PPos -> Error ()
enforceVarIsNotReadOnly info p = when (viIsReadOnly info) $ Fail $ EDVarReadOnly p
