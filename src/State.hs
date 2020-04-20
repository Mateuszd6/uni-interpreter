module State where -- TODO: rename to runtime?

import Control.Monad.Trans.Class (lift, MonadTrans(..))
import qualified Data.Map.Strict as Map -- TODO: Explain why strict instead of lazy.
import Data.List (find)

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
  deriving (Show)

-- TODO: So that it is clear, that it is a _definition_ of a struct.
data Strct = Strct { strctName :: String, strctFields :: Map.Map String TypeId }
  deriving (Show)

type Param = (String, TypeId)

-- Could use Either, but this is much more readable in pattern matching.
data FRetT
  = FRetTSinge TypeId
  | FRetTTuple [TypeId]
  deriving (Show)

data Func = Func { funcId :: FunId,
                   funcBody :: Stmt PPos,
                   funcRetT :: FRetT,
                   funcParams :: [Param],
                   funcScope :: Scope }
  deriving (Show)

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

data Store = Store
  {
    storeVars :: Map.Map VarId Var,
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
    counter :: Int,
    stateStore :: Store,
    stateScope :: Scope
  }
  deriving (Show)

-- TODO Kill temps
tempDefaultScope :: Scope
tempDefaultScope = Scope Map.empty Map.empty Map.empty
tempDefaultStore :: Store
tempDefaultStore = Store Map.empty Map.empty Map.empty 1 1 5 -- TODO: Make sure id don't bind reserved onces.

tempDefaultState :: State
tempDefaultState =
  -- dummyStmt is a hack, we will never evaluate it, but undefined causes some
  -- problems because we use strict map which forces an evaluation.
  let st = State 0 tempDefaultStore tempDefaultScope
      dummyStmt = SBreak Nothing
  in
    -- TODO: HARDOCDES! 2 - tstring, 0 - tvoid!
    (snd . createFunc "assert" dummyStmt [("val", 2)] (FRetTSinge 0)) $
    (snd . createFunc "die" dummyStmt [("val", 3)] (FRetTSinge 0)) $
    (snd . createFunc "printString" dummyStmt [("val", 3)] (FRetTSinge 0)) $
    (snd . createFunc "printBool" dummyStmt [("val", 2)] (FRetTSinge 0)) $
    (snd . createFunc "printInt" dummyStmt [("val", 1)] (FRetTSinge 0))
    st

-- TODO: Move to dumping
dumpState :: State -> IO ()
dumpState s = do
  putStrLn "Dumping state:"
  putStrLn "  Store:"
  putStrLn "    Vars:"
  putMap $ storeVars $ stateStore s
  putStrLn "    Funcs:"
  putMap $ storeFuncs $ stateStore s
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
  where
    putMap :: (Show a, Show b) => Map.Map a b -> IO ()
    putMap =
      mapM_ (putStrLn . (\(x, y) -> "      " ++ show x ++ " -> " ++ show y))
      . Map.toList

-- | Create new variable and add it to the state.
createVar :: String -> Var -> State -> (VarId, State)
createVar name v s@(State _ str@(Store vars _ _ next _ _) scp@(Scope vnames _ _)) =
  (next, s{
      stateStore = str{ storeVars = Map.insert next v vars,
                        nextVarId = next + 1 },
      stateScope = scp{ scopeVars = Map.insert name next vnames } })

createFunc :: String -> Stmt PPos -> [Param] -> FRetT -> State -> (FunId, State)
createFunc name body params ret s@(State _ str@(Store _ funcs _  _ next _) scp@(Scope _ fnames _)) =
  let scp' = scp{ scopeFuncs = Map.insert name next fnames } -- Allows recursion
  in
    (next, s{
        stateStore = str{ storeFuncs = Map.insert next (Func next body ret params scp') funcs,
                          nextFuncId = next + 1 },
        stateScope = scp' })

createStruct :: String -> [(String, TypeId)] -> State -> (TypeId, State)
createStruct name fields s@(State _ str@(Store _ _ types _ _ next) scp@(Scope _ _ tnames)) =
  (next, s{
      stateStore = str{ storeTypes = Map.insert next (Strct name $ Map.fromList fields) types,
                        nextTypeId = next + 1 },
      stateScope = scp{ scopeTypes = Map.insert name next tnames } })

-- TODO: This won't be used probably
-- getVar :: VarId -> State -> Error Var
-- getVar vId (State _ store _) =
  -- errorFromMaybe VarNotFoundError $ Map.lookup vId $ storeVars store

-- | Get variable by name
getVar :: String -> PPos -> State -> Error (VarId, Var)
getVar vname p (State _ str scp) =
  errorFromMaybe (EDVarNotFound vname p) $ do
  vId <- Map.lookup vname $ scopeVars scp -- Scope lookup.
  var <- Map.lookup vId $ storeVars str -- Store lookup, should not fail.
  return (vId, var)

getFunc :: String -> PPos -> State -> Error (FunId, Func)
getFunc fname p (State _ str scp) =
  errorFromMaybe (EDFuncNotFound fname p) $ do
  fId <- Map.lookup fname $ scopeFuncs scp -- Scope lookup.
  func <- Map.lookup fId $ storeFuncs str -- Store lookup, should not fail.
  return (fId, func)

-- TODO: Provide for empty and uninitialized and tuple or don't use it.
getTypeStruct :: String -> PPos -> State -> Error (TypeId, Strct)
getTypeStruct name p st =
  errorFromMaybe (EDTypeNotFound name p) $ do
  tId <- Map.lookup name $ scopeTypes $ stateScope st -- Scope lookup.
  strct <- Map.lookup tId $ storeTypes $ stateStore st -- Store lookup, should not fail.
  return (tId, strct)

-- TODO: I guess this should never happen, because to set a variable
--       we have to get it first. Also PPos?
-- This function does not perform the type check!!
setVar :: VarId -> Var -> State -> Error State
setVar vId val s@(State _ str _) = Ok $
  s{ stateStore = str{ storeVars = Map.insert vId val $ storeVars str }}

-- TODO: Provide for empty and uninitialized and tuple?
getTypeId :: Type PPos -> State -> Error TypeId
getTypeId (TInt _) _ = Ok 1
getTypeId (TBool _) _ = Ok 2
getTypeId (TString _) _ = Ok 3
getTypeId (TUser p (Ident tname)) (State _ _ scp) =
  errorFromMaybe (EDTypeNotFound tname p)
  $ Map.lookup tname $ scopeTypes scp

getTypeDescr :: TypeId -> PPos -> State -> Error Strct
getTypeDescr tId p st = errorFromMaybe (EDVariableNotStruct p) $
  Map.lookup tId $ storeTypes $ stateStore st

-- | This is reverse map lookup, which is slow, but is done only once, when
--   reporting the type error, in case when program is exploding anyway.
getTypeNameForED :: TypeId -> State -> String
getTypeNameForED tId (State _ _ scp)
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

scope :: (State -> ErrorT IO State) -> State -> ErrorT IO State
scope fun st = do
  st' <- fun st
  return st'{ stateScope = stateScope st }

-- Handy when evaluating two things in a scoped block, like evaluating a
-- function with return value.
scope2 :: (State -> ErrorT IO (a, State)) -> State -> ErrorT IO (a, State)
scope2 fun st = do
  (x, st') <- fun st
  return (x, st'{ stateScope = stateScope st })

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

data FlowReason
  = FRBreak PPos
  | FRContinue PPos
  | FRReturn PPos Var -- Return a regular variable
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

-- | Convert Maybe a to Error a. If value is Nothing return an error with
--   provided description.
errorFromMaybe :: ErrorDetail -> Maybe a -> Error a
errorFromMaybe _ (Just x) = Ok x
errorFromMaybe det Nothing = Fail det

-- | Promote regular Error into ErrorT with any wrapped monad.
toErrorT :: Monad m => Error a -> ErrorT m a
toErrorT = ErrorT . return
