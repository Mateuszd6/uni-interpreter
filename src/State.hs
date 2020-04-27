module State where -- TODO: rename to runtime?

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

-- Could use Either, but this is much more readable in pattern matching.
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
    bindVars :: [(Int, Set.Set VarId)]
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
initialState = State 0
                     (Store Map.empty Map.empty Map.empty 1 1 5)
                     (Scope Map.empty Map.empty Map.empty)
                     [(-1, Set.fromList [])]

-- Create new variable and add it to the state.
createVar :: String -> Bool -> Var -> PPos -> State -> Error (VarId, State)
createVar name rdOnly v p s@(State c str@(Store vars _ _ next _ _) scp@(Scope vnames _ _) _) =
  let varInfo = VarInfo (scopeCnt s) rdOnly
  in do
    enforceNotAlreadyDefined name (viScopeN . snd) vnames vars c (EDVarAlreadyDeclared p)
    return (next, s{
               stateStore = str{ storeVars = Map.insert next (v, varInfo) vars,
                                 nextVarId = next + 1 },
               stateScope = scp{ scopeVars = Map.insert name next vnames } })

-- TODO: Decide if this is better, or below is better and refactor.
createFunc :: String -> Stmt PPos ->
              [Param] -> FRetT ->
              Maybe (Set.Set VarId) -> -- Bind
              PPos -> State ->
              Error (FunId, State)
createFunc name body params ret bind p st@(State c str@(Store _ _ _  _ next _) _ _) =
  let scp' = (stateScope st){ scopeFuncs = Map.insert name next (funcsScope st) } -- Allow recursion
      func = Func next body ret params bind scp' c
  in do
    enforceNotAlreadyDefined name fScopeN (funcsScope st) (funcsStore st) c (EDFuncAlreadyDeclared p)
    enforceNotParamRepeated (map (\(x, _, _) -> x) params) (flip EDFuncArgRepeated p)
    return (next, st{
               stateStore = str{ storeFuncs = Map.insert next func (funcsStore st),
                                 nextFuncId = next + 1 },
               stateScope = scp' })

createStruct :: String -> PPos -> [(String, TypeId)] -> State -> Error (TypeId, State)
createStruct name p fields s@(State _ str@(Store _ _ types _ _ next) scp@(Scope _ _ tnames) _) = do
  enforceNotParamRepeated (map fst fields) (flip EDStructArgRepeated p)
  return (next, s{
      stateStore = str{ storeTypes = Map.insert next (StructDef name $ Map.fromList fields) types,
                        nextTypeId = next + 1 },
      stateScope = scp{ scopeTypes = Map.insert name next tnames } })

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

getTypeStruct :: String -> PPos -> State -> Error (TypeId, StructDef)
getTypeStruct name p st = errorFromMaybe (EDTypeNotFound name p) $
  do
    tId <- Map.lookup name (typesScope st)
    strct <- Map.lookup tId (typesStore st)
    return (tId, strct)

-- This function does not perform the type check!!
setVar :: VarId -> Var -> PPos -> State -> Error State
setVar vId val p s@(State _ str _ _) = do
  -- This should never fail to lookup the var, unless there is a bug. That's why we give a
  -- variable an artificial name.
  (_, info) <- getVarImpl vId ("ID=" ++ show vId) p s
  enforceVarIsNotReadOnly info p
  return s{ stateStore = str{ storeVars = Map.insert vId (val, info) $ storeVars str }}

getTypeId :: Type PPos -> State -> Error TypeId
getTypeId (TInt _) _ = return 1
getTypeId (TBool _) _ = return 2
getTypeId (TString _) _ = return 3
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
  | tId == 1 = "int"
  | tId == 2 = "bool"
  | tId == 3 = "string"
  | tId == 4 = "tuple"
  | otherwise = strctName $ nofail $ getTypeDescr tId Nothing st

defaultVarOfType :: State -> TypeId -> Var
defaultVarOfType _ 0 = VEmpty -- Should not happen
defaultVarOfType _ 1 = VInt 0
defaultVarOfType _ 2 = VBool False
defaultVarOfType _ 3 = VString ""
defaultVarOfType _ 4 = VTuple [] -- Should no happen
defaultVarOfType st sId = VStruct sId $ Map.map (defaultVarOfType st) $ strctFields $
                          fromJust $ Map.lookup sId (typesStore st)

varTypeId :: Var -> Int
varTypeId VEmpty = 0
varTypeId (VInt _) = 1
varTypeId (VBool _) = 2
varTypeId (VString _) = 3
varTypeId (VTuple _) = 4 -- Tuple variables are only used when returning values.
varTypeId (VStruct sId _) = sId -- Structs know their typeIDs.

-- To avoid code duplication in scope and scope2.
scopeA :: (a -> State) -> (a -> State -> a) ->
         (State -> CtrlT IO a) ->
         Maybe (Set.Set VarId) -> State -> CtrlT IO a
scopeA getS setS fun bind st =
  let newBind = case bind of
                  Nothing -> bindVars st
                  Just set -> (scopeCnt st + 1, set) : bindVars st
  in do
    retv <- fun st { scopeCnt = scopeCnt st + 1, bindVars = newBind }
    return $ setS retv (getS retv){ scopeCnt = scopeCnt st,
                                    stateScope = stateScope st,
                                    bindVars = bindVars st }
    -- Rollbacks scope and bind vars after leaving the scope.

scope :: (State -> CtrlT IO State) -> Maybe (Set.Set VarId) -> State
      -> CtrlT IO State
scope = scopeA id (\_ x -> x)

scope2 :: (State -> CtrlT IO (a, State)) -> Maybe (Set.Set VarId) -> State
       -> CtrlT IO (a, State)
scope2 = scopeA snd (\(x, _) z -> (x, z))

nofail :: Show a => Error a -> a
nofail (Ok x) = x
nofail e = error $ "Unexpected error: " ++ show e

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
ctrlToError (CtrlException expType _) = Fail $ EDUncoughtedExc (show expType) (getPos expType)

catchBreak :: Monad m => CtrlT m State -> CtrlT m State
catchBreak st = do
  err_ <- lift $ runCtrlT st
  CtrlT . return $ case err_ of
    CtrlException (ExBreak _) s -> CtrlRegular $ Ok s
    _ -> err_

catchContinue :: Monad m => CtrlT m State -> CtrlT m State
catchContinue st = do
  err_ <- lift $ runCtrlT st
  CtrlT . return $ case err_ of
    CtrlException (ExContinue _) s -> CtrlRegular $ Ok s
    _ -> err_

catchReturnVoid :: Monad m => PPos -> CtrlT m State -> CtrlT m (Var, State)
catchReturnVoid _ st = do
  err_ <- lift $ runCtrlT st
  CtrlT . return $ case err_ of
    CtrlException (ExReturn _ VEmpty) st' -> CtrlRegular $ Ok (VEmpty, st')
    CtrlException (ExReturn p _) _ -> CtrlRegular $ Fail $ EDNoReturnNonVoid p
    CtrlException x y -> CtrlException x y
    CtrlRegular (Ok st') -> CtrlRegular $ Ok (VEmpty, st')
    CtrlRegular (Fail r) -> CtrlRegular $ Fail r

expectReturnValue :: Monad m => FRetT -> PPos -> CtrlT m State -> CtrlT m (Var, State)
expectReturnValue retT p st = do
  err_ <- lift $ runCtrlT st
  CtrlT . return $ case err_ of
    CtrlException (ExReturn p0 VEmpty) _ -> CtrlRegular $ Fail $ EDReturnVoid p0
    CtrlException (ExReturn p0 v) st' -> CtrlRegular $ enforceRetType v retT p0 st' >> return (v, st')
    CtrlRegular (Fail r) -> CtrlRegular $ Fail r
    _ -> CtrlRegular $ Fail $ EDNoReturn p

dontAllowBreakContinue :: Monad m => CtrlT m a -> CtrlT m a
dontAllowBreakContinue st = do
  err_ <- lift $ runCtrlT st
  CtrlT . return $ case err_ of
    CtrlException (ExBreak p) _ -> CtrlRegular $ Fail $ EDUnexpectedBreak p
    CtrlException (ExContinue p) _ -> CtrlRegular $ Fail $ EDUnexpectedContinue p
    _ -> err_

dontAllowReturn :: Monad m => CtrlT m a -> CtrlT m a
dontAllowReturn st = do
  err_ <- lift $ runCtrlT st
  CtrlT . return $ case err_ of
    CtrlException (ExReturn p _) _ -> CtrlRegular $ Fail $ EDUnexpectedReturn p
    _ -> err_

throw :: Monad m => ExcType -> State -> CtrlT m a
throw v st = CtrlT . return $ CtrlException v st


-- Functions used to enforce some commonly changed conditions in the Error monad.
enforceParamLengthEqual :: PPos -> Int -> Int -> Error ()
enforceParamLengthEqual p expected got =
  when (expected /= got) $ Fail $ EDInvalidNumParams p expected got

-- Make sure the var is of the desired type or fail with a TypeError.
enforceType :: Var -> TypeId -> PPos -> State -> Error ()
enforceType v tId p st = when (varTypeId v /= tId) $ Fail $
  EDTypeError (getTypeName tId st) (getTypeName (varTypeId v) st) p

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

enforceIsBultinType :: Type PPos -> Error ()
enforceIsBultinType (TInt _) = return ()
enforceIsBultinType (TBool _) = return ()
enforceIsBultinType (TString _) = return ()
enforceIsBultinType (TUser p (Ident n)) = Fail $ EDScanError n p

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
enforceBind vId scopeN n p (State _ _ _ ((i, set):_))
  | scopeN >= i = return () -- Variable declared insinde last bind block.
  | Set.member vId set = return () -- Variable binded in the curr bind scope.
  | otherwise = Fail $ EDBind n p
enforceBind _ _ _ _ (State _ _ _ []) = undefined -- We always have at least one bind rule
                                                 -- in the scope.

enforceVarIsNotReadOnly :: VarInfo -> PPos -> Error ()
enforceVarIsNotReadOnly info p = when (viIsReadOnly info) $ Fail $ EDVarReadOnly p
