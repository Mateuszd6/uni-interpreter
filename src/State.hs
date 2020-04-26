module State where -- TODO: rename to runtime?

import Control.Monad (when)
import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Data.List (find, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AbsLanguage

type PPos = Maybe (Int, Int)

type VarId = Int
type FunId = Int
type TypeId = Int

type Struct = Map.Map String Var

data Var
  = VUninitialized { vType :: TypeId } -- Unitinialized, but knows its type.
  | VEmpty
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
  show VStruct { } = "??" -- TODO: This is possible...
  show VTuple { } = "??" -- TODO: This shoudl't be possible to do.
  show VUninitialized { } = "??" -- TODO: Kill unitinialized

data StructDef = StructDef { strctName :: String, strctFields :: Map.Map String TypeId }
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

-- The state contains two separate things: store, which holds variable
-- states by their IDs and State which maps names to IDs. This way we
-- can handle static binding of function parameters, var hiding etc.
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

-- TODO: Kill when not necesarry
dumpState :: State -> IO ()
dumpState s = do
  putStrLn "Dumping state:"
  putStrLn $ "Currect scope:" ++ show (scopeCnt s)
  putStrLn "  Store:"
  putStrLn "    Vars:"
  putMap $ storeVars $ stateStore s
  putStrLn "    Funcs:"
  putMap $ funcsStore s
  putStrLn "    Types:"
  putMap $ storeTypes $ stateStore s
  -- putStrLn "    Types:"
  -- putMap $ storeTypes $ stateStore s
  putStrLn "  Scope:"
  putStrLn "    Vars:"
  putMap $ scopeVars $ stateScope s
  putStrLn "    Funcs:"
  putMap $ scopeFuncs $ stateScope s
  putStrLn "    Types:"
  putMap $ scopeTypes $ stateScope s
  putStrLn "BIND:"
  putListOfSets
  where
    putMap :: (Show a, Show b) => Map.Map a b -> IO ()
    putMap =
      mapM_ (putStrLn . (\(x, y) -> "      " ++ show x ++ " -> " ++ show y))
      . Map.toList
    putListOfSets =
      mapM_ (\(x, y) -> putStr ("      " ++ show x ++ " ->") >> putSet y >> putStrLn "") $ bindVars s
    putSet :: (Show a) => Set.Set a -> IO ()
    putSet =
      mapM_ (putStr . (\x -> " " ++ show x))
      . Set.toList


-- Check if var/func/type is aready defined for the scope.
checkIfAlreadyDefined :: String -> -- Name
                         (a -> Int) -> -- Get scope num from a
                         Map.Map String Int -> -- Scope
                         Map.Map Int a -> -- Store
                         Int -> -- Curr scope count.
                         ErrorDetail -> -- Error detailed returned on error.
                         Error ()
checkIfAlreadyDefined name scopeN sscope sstore scCnt ed =
  let b = do
        eId <- Map.lookup name sscope
        eScn <- scopeN <$> Map.lookup eId sstore
        return $ eScn == scCnt
  in case b of
       Just True -> Fail ed
       _ -> return ()

-- Can be used to check func params or struct fields, it just requires an
-- function that can build an error objct from the name of a repeated thing.
checkIfParamRepeated :: [String] -> (String -> ErrorDetail) -> Error ()
checkIfParamRepeated params ed =
  let isParamRepeatedImpl :: [String] -> Maybe String
      isParamRepeatedImpl [] = Nothing
      isParamRepeatedImpl [_] = Nothing
      isParamRepeatedImpl (x:y:xys)
          | x == y = Just x
          | otherwise = isParamRepeatedImpl xys
  in case isParamRepeatedImpl $ sort params of
       Just rep -> Fail $ ed rep
       _ -> return ()

checkBind :: VarId -> Int -> String -> PPos -> State -> Error ()
checkBind vId scopeN n p (State _ _ _ ((i, set):_))
  | scopeN >= i = return () -- Variable declared insinde last bind block.
  | Set.member vId set = return () -- Variable binded in the curr bind scope.
  | otherwise = Fail $ EDBind n p

-- We always have at least one bind rule in the scope:
checkBind _ _ _ _ (State _ _ _ []) = undefined

checkIfVarIsReadOnly :: VarInfo -> PPos -> Error ()
checkIfVarIsReadOnly info p = when (viIsReadOnly info) $ Fail $ EDVariableReadOnly p

-- Create new variable and add it to the state.
createVar :: String -> Bool -> Var -> PPos -> State -> Error (VarId, State)
createVar name rdOnly v p s@(State c str@(Store vars _ _ next _ _) scp@(Scope vnames _ _) _) =
  let varInfo = VarInfo (scopeCnt s) rdOnly
  in do
    checkIfAlreadyDefined name (viScopeN . snd) vnames vars c (EDVarAlreadyDeclared p)
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
    checkIfAlreadyDefined name fScopeN (funcsScope st) (funcsStore st) c (EDFuncAlreadyDeclared p)
    checkIfParamRepeated (map (\(x, _, _) -> x) params) (flip EDFuncArgRepeated p)
    return (next, st{
               stateStore = str{ storeFuncs = Map.insert next func (funcsStore st),
                                 nextFuncId = next + 1 },
               stateScope = scp' })

createStruct :: String -> PPos -> [(String, TypeId)] -> State -> Error (TypeId, State)
createStruct name p fields s@(State _ str@(Store _ _ types _ _ next) scp@(Scope _ _ tnames) _) = do
  checkIfParamRepeated (map fst fields) (flip EDStructArgRepeated p)
  return (next, s{
      stateStore = str{ storeTypes = Map.insert next (StructDef name $ Map.fromList fields) types,
                        nextTypeId = next + 1 },
      stateScope = scp{ scopeTypes = Map.insert name next tnames } })

getVarImpl :: VarId -> String -> PPos -> State -> Error (Var, VarInfo)
getVarImpl vId vname p st = do
  (var, vinfo) <- errorFromMaybe (EDVarNotFound vname p) $
                  Map.lookup vId (varsStore st)
  checkBind vId (viScopeN vinfo) vname p st
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

-- TODO: Provide for empty and uninitialized and tuple or don't use it.
getTypeStruct :: String -> PPos -> State -> Error (TypeId, StructDef)
getTypeStruct name p st = errorFromMaybe (EDTypeNotFound name p) $
  do
    tId <- Map.lookup name (typesScope st)
    strct <- Map.lookup tId (typesStore st)
    return (tId, strct)

enforceIsBultinType :: Type PPos -> Error ()
enforceIsBultinType (TInt _) = return ()
enforceIsBultinType (TBool _) = return ()
enforceIsBultinType (TString _) = return ()
enforceIsBultinType (TUser p (Ident n)) = Fail $ EDScanError n p

-- This function does not perform the type check!!
setVar :: VarId -> Var -> PPos -> State -> Error State
setVar vId val p s@(State _ str _ _) = do
  -- This should never fail to lookup the var, unless there is a bug. That's why
  -- we give a variable an artificial name.
  (_, info) <- getVarImpl vId ("ID=" ++ show vId) p s
  checkIfVarIsReadOnly info p
  return s{ stateStore = str{ storeVars = Map.insert vId (val, info) $ storeVars str }}

-- TODO: Provide for empty and uninitialized and tuple?
getTypeId :: Type PPos -> State -> Error TypeId
getTypeId (TInt _) _ = return 1
getTypeId (TBool _) _ = return 2
getTypeId (TString _) _ = return 3
getTypeId (TUser p (Ident tname)) st =
  errorFromMaybe (EDTypeNotFound tname p) $
  Map.lookup tname (typesScope st)

getTypeDescr :: TypeId -> PPos -> State -> Error StructDef
getTypeDescr tId p st = errorFromMaybe (EDVariableNotStruct p) $
  Map.lookup tId $ storeTypes $ stateStore st

-- This is reverse map lookup, which is slow, but is done only once, when
-- reporting the type error, in case when program is exploding anyway.
getTypeName :: TypeId -> State -> String
getTypeName tId st
  | tId == 0 = "void"
  | tId == 1 = "int"
  | tId == 2 = "bool"
  | tId == 3 = "string"
  | tId == 4 = "tuple" -- TODO: describe the trick
  | otherwise = maybe ("*unknown* (typeId = " ++ show tId ++ ")") fst $
                find ((tId ==) . snd) (Map.toList $ typesScope st)

varTypeId :: Var -> Int
varTypeId (VUninitialized tid) = tid -- TODO: Kill uninitialzied
varTypeId VEmpty = 0
varTypeId (VInt _) = 1
varTypeId (VBool _) = 2
varTypeId (VString _) = 3
varTypeId (VTuple _) = 4 -- Tuple variables are only used when returning values.
varTypeId (VStruct sId _) = sId -- Structs know their typeIDs.

scopeA :: (a -> State) -> (a -> State -> a) ->
         (State -> ErrorT IO a) ->
         Maybe (Set.Set VarId) -> State -> ErrorT IO a
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

scope :: (State -> ErrorT IO State) -> Maybe (Set.Set VarId) -> State -> ErrorT IO State
scope = scopeA id (\_ x -> x)

-- Handy when evaluating two things in a scoped block, like evaluating a
-- function with return value.
scope2 :: (State -> ErrorT IO (a, State)) -> Maybe (Set.Set VarId) -> State -> ErrorT IO (a, State)
scope2 = scopeA snd (\(x, _) z -> (x, z))

nofail :: Show a => Error a -> a
nofail (Ok x) = x
nofail e = error $ "Unexpected error: " ++ show e

data ErrorDetail
  = EDVarNotFound String PPos
  | EDVarNotInitialized PPos
  | EDFuncNotFound String PPos
  | EDParsingError String
  | EDTypeError String String PPos
  | EDTypeNotFound String PPos
  | EDUnexpectedBreak PPos
  | EDUnexpectedContinue PPos
  | EDUnexpectedReturn PPos
  | EDNoReturn PPos
  | EDNoReturnNonVoid PPos
  | EDReturnVoid PPos
  | EDInvalidNumParams PPos Int Int
  | EDTupleNumbersDontMatch PPos Int Int
  | EDTupleReturned PPos
  | EDValueReturned PPos
  | EDCantBePrimitiveType String PPos
  | EDVariableNotStruct PPos
  | EDNoMember PPos String String
  | EDCantCompare PPos String String
  | EDAssertFail PPos
  | EDTupleNotAllowed PPos
  | EDDivideByZero PPos
  | EDVariableReadOnly PPos
  | EDVarAlreadyDeclared PPos
  | EDFuncAlreadyDeclared PPos
  | EDTypeAlreadyDeclared PPos
  | EDFuncArgRepeated String PPos
  | EDStructArgRepeated String PPos
  | EDBind String PPos
  | EDCantUseWiderBind String PPos
  | EDScanError String PPos -- Rename to not builtin type.

  deriving (Show)

showFCol :: PPos -> String -> String
showFCol (Just (l, c)) fname = fname ++ ":" ++ show l ++ ":" ++ show c ++ ": "
showFCol Nothing fname = fname ++ ": "

errorMsg :: String -> ErrorDetail -> String
errorMsg fname (EDVarNotFound name p) = showFCol p fname ++ "Variable `" ++ name ++ "' not in scope."
errorMsg fname (EDVarNotInitialized p) = showFCol p fname ++ "Variable was not initialized."
errorMsg fname (EDFuncNotFound name p) = showFCol p fname ++ "Function `" ++ name ++ "' not in scope."
errorMsg fname (EDParsingError str) = showFCol Nothing fname ++ "Parsing error: " ++ str ++ "."
errorMsg fname (EDTypeError expected got p) = showFCol p fname ++ "Type error: "
                                    ++ "expected `" ++ expected ++ "'"
                                    ++ ", got `" ++ got ++ "'."
errorMsg fname (EDTypeNotFound tname p) = showFCol p fname ++ "Type `" ++ tname ++ "' not in scope."
errorMsg fname (EDUnexpectedBreak p) = showFCol p fname ++ "`break' is not allowed here."
errorMsg fname (EDUnexpectedContinue p) = showFCol p fname ++ "`continue' is not allowed here."
errorMsg fname (EDUnexpectedReturn p) = showFCol p fname ++ "`return' is not allowed here."
errorMsg fname (EDNoReturn p) = showFCol p fname ++ "Function that returns a value didn't return."
errorMsg fname (EDNoReturnNonVoid p) = showFCol p fname ++ "Void function cannot return a value."
errorMsg fname (EDReturnVoid p) = showFCol p fname ++ "Return without a value when value was expected."
errorMsg fname (EDInvalidNumParams p expected got) = showFCol p fname ++
  "Invalid number of parameters. Expected " ++ show expected ++
  ", but got " ++ show got ++ "."
errorMsg fname (EDTupleNumbersDontMatch p l r) = showFCol p fname ++
  "Numbers of elements in asigned tuples don't " ++
  "much: left has " ++ show l ++ ", but right has " ++ show r ++ "."
errorMsg fname (EDTupleReturned p) = showFCol p fname ++ "Tuple returned, when single variable expected."
errorMsg fname (EDValueReturned p) = showFCol p fname ++ "Single variable returned, when tuple was expected."
errorMsg fname (EDCantBePrimitiveType t p) = showFCol p fname ++
                                   "Type must be a struct, not a primitive type." ++
                                   " (Was: `" ++ t ++ "').";
errorMsg fname (EDVariableNotStruct p) = showFCol p fname ++ "Variable or member is not a struct."
errorMsg fname (EDNoMember p tname memb) = showFCol p fname ++ "Struct `" ++ tname ++ "'" ++
                                 " has no member " ++ memb ++ "."
errorMsg fname (EDCantCompare p l r) = showFCol p fname ++
                                       "Can't compare types `" ++ l ++ "' and `" ++ r ++ "'. " ++
                                       "Only builtin types with matching type can be compared."

errorMsg fname (EDAssertFail p) = showFCol p fname ++ "Assertion failed."
errorMsg fname (EDTupleNotAllowed p) = showFCol p fname ++ "Tuple is not allowed here."
errorMsg fname (EDDivideByZero p) = showFCol p fname ++ "Divide by zero."
errorMsg fname (EDVariableReadOnly p) = showFCol p fname ++ "Variable is read only."
errorMsg fname (EDVarAlreadyDeclared p) = showFCol p fname ++ "Variable is already declared in the current scope."
errorMsg fname (EDFuncAlreadyDeclared p) = showFCol p fname ++ "Function is already declared in the current scope."
errorMsg fname (EDTypeAlreadyDeclared p) = showFCol p fname ++ "Struct is already declared in the current scope."
errorMsg fname (EDFuncArgRepeated name p) = showFCol p fname ++ "Function argument named `" ++ name ++ "' is repeated more than once."
errorMsg fname (EDStructArgRepeated name p) = showFCol p fname ++ "Struct member named `" ++ name ++ "' is repeated more than once."
errorMsg fname (EDBind n p) = showFCol p fname ++ "Variable `" ++ n ++ "' is used, but not binded."
errorMsg fname (EDCantUseWiderBind n p) = showFCol p fname ++ "Can't call function `" ++ n ++ "' because it refers to the wider scope than the current binded block does."
errorMsg fname (EDScanError n p) = showFCol p fname ++ "Can't scan `" ++ n ++ "'. Not a builtin type."

data FlowReason
  = FRBreak PPos
  | FRContinue PPos
  | FRReturn PPos Var
  deriving (Show)

data Error a
  = Ok a
  | Flow FlowReason State
  | Fail ErrorDetail
  deriving (Show)

instance Functor Error  where
  fmap _ (Fail rs) = Fail rs
  fmap _ (Flow r s) = Flow r s
  fmap f (Ok a) = Ok (f a)

instance Applicative Error where
  pure = Ok

  Ok f <*> m = fmap f m
  Flow r s <*> _m = Flow r s
  Fail rs <*> _m = Fail rs

  Ok _m1 *> m2 = m2
  Flow r s *> _m2 = Flow r s
  Fail rs *> _m2 = Fail rs

instance Monad Error where
  (Ok x) >>= k = k x
  Flow r s >>= _ = Flow r s
  Fail rs >>= _ = Fail rs

  (>>) = (*>)

newtype ErrorT m a = ErrorT { runErrorT :: m (Error a) }

instance MonadTrans ErrorT where
  lift = ErrorT . fmap Ok

instance (Monad m) => Monad (ErrorT m) where
  return = ErrorT . return . Ok

  x >>= f = ErrorT $ do
      v <- runErrorT x
      case v of
          Fail rs -> return $ Fail rs
          Flow r s -> return $ Flow r s
          Ok w -> runErrorT (f w)

instance (Functor m) => Functor (ErrorT m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Functor m, Monad m) => Applicative (ErrorT m) where
  pure = ErrorT . return . Ok

  mf <*> mx = ErrorT $ do
      mb_f <- runErrorT mf
      case mb_f of
          Fail rs -> return $ Fail rs
          Flow r s -> return $ Flow r s
          Ok f -> do
              mb_x <- runErrorT mx
              case mb_x of
                  Fail rs -> return $ Fail rs
                  Flow r s -> return $ Flow r s
                  Ok x  -> return (Ok (f x))

  m *> k = m >> k

-- Convert Maybe a to Error a. If value is Nothing return an error with
-- provided description.
errorFromMaybe :: ErrorDetail -> Maybe a -> Error a
errorFromMaybe _ (Just x) = return x
errorFromMaybe det Nothing = Fail det

-- Promote regular Error into ErrorT with any wrapped monad.
toErrorT :: Monad m => Error a -> ErrorT m a
toErrorT = ErrorT . return
