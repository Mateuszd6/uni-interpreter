module State where -- TODO: rename to runtime?

import Control.Monad.Trans.Class (lift, MonadTrans(..))
import qualified Data.Map.Strict as Map -- TODO: Explain why strict instead of lazy.
import qualified Data.Set as Set
import Data.List (find, sort)

import AbsLanguage

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
  show VEmpty = "void "
  show (VInt i) = show i ++ " "
  show (VBool b) = show b ++ " "
  show (VString s) = s ++ " "
  show VStruct { } = "??" -- TODO: This is possible...
  show VTuple { } = "??" -- TODO: This shoudl't be possible to do.
  show VUninitialized { } = "??" -- TODO: Kill unitinialized

-- TODO: So that it is clear, that it is a _definition_ of a struct.
data Strct = Strct { strctName :: String, strctFields :: Map.Map String TypeId }
  deriving (Show)

type Param = (String, TypeId)

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

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

data Store = Store
  {
    storeVars :: Map.Map VarId (Var, VarInfo),
    -- TODO: Instead maybe should be PPos but can't include parser.
    --       Include this in parser?
    storeFuncs :: Map.Map FunId Func,
    storeTypes :: Map.Map TypeId Strct,

    nextVarId :: Int,
    nextFuncId :: Int,
    nextTypeId :: Int
  }
  deriving (Show)

-- TODO: Explain!!
data Scope = Scope
  {
    scopeVars :: Map.Map String VarId,
    scopeFuncs :: Map.Map String FunId,
    scopeTypes :: Map.Map String TypeId
  }
  deriving (Show)

data State = State
  {
    scopeCnt :: Int,
    stateStore :: Store,
    stateScope :: Scope,
    bindVars :: [(Int, Set.Set VarId)]
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
typesStore :: State -> Map.Map TypeId Strct
typesStore = storeTypes . stateStore

-- TODO Kill temps
tempDefaultScope :: Scope
tempDefaultScope = Scope Map.empty Map.empty Map.empty
tempDefaultStore :: Store
tempDefaultStore = Store Map.empty Map.empty Map.empty 1 1 5 -- TODO: Make sure id don't bind reserved onces.

tempDefaultState :: State
tempDefaultState = State 0 tempDefaultStore tempDefaultScope [(-1, Set.fromList [])]

-- TODO: Move to dumping
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

createFunc :: String -> Stmt PPos -> [Param] -> FRetT -> Maybe (Set.Set VarId) -> PPos -> State -> Error (FunId, State)
createFunc name body params ret bind p s@(State c str@(Store _ funcs _  _ next _) scp@(Scope _ fnames _) _) =
  let scp' = scp{ scopeFuncs = Map.insert name next fnames } -- Allow recursion
      func = Func next body ret params bind scp' c
  in do
    checkIfAlreadyDefined name fScopeN fnames funcs c (EDFuncAlreadyDeclared p)
    checkIfParamRepeated (map fst params) (flip EDFuncArgRepeated p)
    return (next, s{
               stateStore = str{ storeFuncs = Map.insert next func funcs,
                                 nextFuncId = next + 1 },
               stateScope = scp' })

createStruct :: String -> PPos -> [(String, TypeId)] -> State -> Error (TypeId, State)
createStruct name p fields s@(State _ str@(Store _ _ types _ _ next) scp@(Scope _ _ tnames) _) = do
  checkIfParamRepeated (map fst fields) (flip EDStructArgRepeated p)
  return (next, s{
      stateStore = str{ storeTypes = Map.insert next (Strct name $ Map.fromList fields) types,
                        nextTypeId = next + 1 },
      stateScope = scp{ scopeTypes = Map.insert name next tnames } })

checkBind :: VarId -> Int -> String -> PPos -> State -> Error ()
checkBind vId scopeN n p (State _ _ _ ((i, set):_))
  | scopeN >= i = Ok () -- TODO: make sure it is >= not > ! -- Variable declared insinde last bind block.
  | Set.member vId set = Ok () -- Variable binded in the curr bind scope.
  | otherwise = Fail $ EDBind n p

-- TODO: Rename
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
getTypeStruct :: String -> PPos -> State -> Error (TypeId, Strct)
getTypeStruct name p st = errorFromMaybe (EDTypeNotFound name p) $
  do
    tId <- Map.lookup name (typesScope st)
    strct <- Map.lookup tId (typesStore st)
    return (tId, strct)

checkIfVarIsReadOnly :: VarInfo -> PPos -> Error ()
checkIfVarIsReadOnly info p = if viIsReadOnly info
                              then Fail $ EDVariableReadOnly p
                              else Ok ()

-- TODO: I guess this should never happen, because to set a variable
--       we have to get it first. Also PPos?
-- This function does not perform the type check!!
setVar :: VarId -> Var -> PPos -> State -> Error State
setVar vId val p s@(State _ str _ _) = do
  -- This should never fail unless there is a bug. That's why we give a variable
  -- an artificial name
  (_, info) <- getVarImpl vId ("ID=" ++ show vId) p s
  checkIfVarIsReadOnly info p
  Ok s{ stateStore = str{ storeVars = Map.insert vId (val, info) $ storeVars str }}

-- TODO: Provide for empty and uninitialized and tuple?
getTypeId :: Type PPos -> State -> Error TypeId
getTypeId (TInt _) _ = Ok 1
getTypeId (TBool _) _ = Ok 2
getTypeId (TString _) _ = Ok 3
getTypeId (TUser p (Ident tname)) st =
  errorFromMaybe (EDTypeNotFound tname p) $
  Map.lookup tname $ typesScope st

getTypeDescr :: TypeId -> PPos -> State -> Error Strct
getTypeDescr tId p st = errorFromMaybe (EDVariableNotStruct p) $
  Map.lookup tId $ storeTypes $ stateStore st

-- This is reverse map lookup, which is slow, but is done only once, when
-- reporting the type error, in case when program is exploding anyway.
getTypeNameForED :: TypeId -> State -> String
getTypeNameForED tId (State _ _ scp _)
  | tId == 0 = "void"
  | tId == 1 = "int"
  | tId == 2 = "bool"
  | tId == 3 = "string"
  | tId == 4 = "tuple" -- TODO: describe the trick
  | otherwise = maybe ("*unknown* (typeId = " ++ show tId ++ ")") fst $
                find ((tId ==) . snd) (Map.toList $ scopeTypes scp)

-- typeId is used to determine variable type. We can't use name becasue
-- TODO: explain and decide whether it is used or not.
varTypeId :: Var -> Int
varTypeId (VUninitialized tid) = tid -- Permitive types have constant typeids.
varTypeId VEmpty = 0
varTypeId (VInt _) = 1 -- Permitive types have constant typeids.
varTypeId (VBool _) = 2
varTypeId (VString _) = 3
varTypeId (VTuple _) = 4 -- TODO: Describe the trick
varTypeId (VStruct sId _) = sId -- TODO: Structs know their typeids??

scope :: (State -> ErrorT IO State) -> Maybe (Set.Set VarId) -> State -> ErrorT IO State
scope fun bind st =
  let newBind = case bind of
                  Nothing -> bindVars st
                  Just set -> (scopeCnt st + 1, set) : bindVars st
  in do
    lift $ putStrLn $ "Starting a new scope: " ++ show (scopeCnt st + 1)
    -- lift $ dumpState $ st { scopeCnt = scopeCnt st + 1, bindVars = newBind}

    st' <- fun st { scopeCnt = scopeCnt st + 1, bindVars = newBind}

    lift $ putStrLn $ "Closing a scope: " ++ show (scopeCnt st + 1)
    return st'{ scopeCnt = scopeCnt st, stateScope = stateScope st, bindVars = bindVars st }
    -- Rollbacks scope and bind vars after leaving the scope.

-- Handy when evaluating two things in a scoped block, like evaluating a
-- function with return value.
scope2 :: (State -> ErrorT IO (a, State)) -> Maybe (Set.Set VarId) -> State -> ErrorT IO (a, State)
scope2 fun bind st =
  let newBind = case bind of
                  Nothing -> bindVars st
                  Just set -> (scopeCnt st + 1, set) : bindVars st
  in do
    (x, st') <- fun st { scopeCnt = scopeCnt st + 1, bindVars = newBind}
    return (x, st'{ scopeCnt = scopeCnt st, stateScope = stateScope st, bindVars = bindVars st })
  -- (x, st') <- fun st { scopeCnt = scopeCnt st + 1 }
  -- return (x, st'{ stateScope = stateScope st })

-- TODO: read this and decide if it stays.
-- Generalized Error Monad (including Monad Transform) to combine error
-- handling with with IO. This features an ErrorDetail type which allows us to
-- give user many detailed information about an error. This is laregly taken
-- from MaybeT implementation in the Haskell standard library.

type PPos = Maybe (Int, Int) -- TODO: Move it upper.

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
  | EDNoReturnNonVoid PPos -- TODO: rename
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
  fmap _ (Fail reason) = Fail reason
  fmap _ (Flow r s) = Flow r s
  fmap f (Ok a) = Ok (f a)

instance Applicative Error where
  pure = Ok

  Ok f <*> m = fmap f m
  Flow r s <*> _m = Flow r s
  Fail reason <*> _m = Fail reason

  Ok _m1 *> m2 = m2
  Flow r s *> _m2 = Flow r s
  Fail reason *> _m2 = Fail reason -- TODO(MD): Invesitgate

instance Monad Error where
  (Ok x) >>= k = k x
  Flow r s >>= _ = Flow r s
  Fail reason >>= _ = Fail reason

  (>>) = (*>) -- TODO(MD): Invesitgate

newtype ErrorT m a = ErrorT { runErrorT :: m (Error a) }

instance MonadTrans ErrorT where
  lift = ErrorT . fmap Ok

instance (Monad m) => Monad (ErrorT m) where
  return = ErrorT . return . Ok

  x >>= f = ErrorT $ do
      v <- runErrorT x
      case v of
          Fail reason -> return $ Fail reason
          Flow r s -> return $ Flow r s
          Ok w -> runErrorT (f w)

instance (Functor m) => Functor (ErrorT m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Functor m, Monad m) => Applicative (ErrorT m) where
  pure = ErrorT . return . Ok

  mf <*> mx = ErrorT $ do
      mb_f <- runErrorT mf
      case mb_f of
          Fail reason -> return $ Fail reason
          Flow r s -> return $ Flow r s
          Ok f -> do
              mb_x <- runErrorT mx
              case mb_x of
                  Fail reason -> return $ Fail reason
                  Flow r s -> return $ Flow r s
                  Ok x  -> return (Ok (f x))

  m *> k = m >> k -- TODO(MD): Invesitgate

-- Convert Maybe a to Error a. If value is Nothing return an error with
-- provided description.
errorFromMaybe :: ErrorDetail -> Maybe a -> Error a
errorFromMaybe _ (Just x) = Ok x
errorFromMaybe det Nothing = Fail det

-- Promote regular Error into ErrorT with any wrapped monad.
toErrorT :: Monad m => Error a -> ErrorT m a
toErrorT = ErrorT . return
