-- | It's not a monad, sorry! -- TODO!
-- | All program-state related classes and functions.
module State where -- TODO: rename to runtime?

import qualified Data.Map.Strict as Map -- TODO: Explain why strict instead of lazy.
import qualified Data.Maybe as Maybe
import Data.List (find)

import AbsLanguage

import Parser
import Error

type VarId = Int
type FunId = Int
type TypeId = Int

type Struct = Map.Map String Var

data Var
  = VUninitialized { vType :: TypeId } -- Unitinialized, but knows its type.
  | VInt Int
  | VBool Bool
  | VString String
  | VStruct { vStructTId :: Int, vStructVars :: Struct }
  deriving (Show)

data Func = Func { funcBody :: Stmt PPos, funcRetT :: TypeId, funcScope :: Scope }
  deriving (Show)

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

data Store = Store
  {
    storeVars :: Map.Map VarId Var,
    -- TODO: Instead maybe should be PPos but can't include parser.
    --       Include this in parser?
    -- TODO: make a type instead of using tuple?
    storeFuncs :: Map.Map FunId Func,
    -- storeTypes :: Map.Map TypeId () -- TODO!

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
tempDefaultStore = Store Map.empty Map.empty 1 1 4 -- TODO: Make sure id don't bind reserved onces.

tempDefaultState :: State
tempDefaultState = State 0 tempDefaultStore tempDefaultScope

-- TODO: Move to dumping
dumpState :: State -> IO ()
dumpState s = do
  putStrLn "Dumping state:"
  putStrLn "  Store:"
  putStrLn "    Vars:"
  putMap $ storeVars $ stateStore s
  putStrLn "    Funcs:"
  putMap $ storeFuncs $ stateStore s
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
createVar name v s@(State _ str@(Store vars _ next _ _) scp@(Scope vnames _ _)) =
  (next, s{
      stateStore = str{ storeVars = Map.insert next v vars,
                        nextVarId = next + 1 },
      stateScope = scp{ scopeVars = Map.insert name next vnames } })

createFunc :: String -> Stmt (Maybe (Int, Int)) -> TypeId -> State -> (FunId, State)
createFunc name body ret s@(State _ str@(Store _ funcs _ next _) scp@(Scope _ fnames _)) =
  (next, s{
      stateStore = str{ storeFuncs = Map.insert next (Func body ret scp) funcs,
                        nextFuncId = next + 1 },
      stateScope = scp{ scopeFuncs = Map.insert name next fnames } })

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


-- TODO: I guess this should never happen, because to set a variable
--       we have to get it first. Also PPos?
-- This function does not perform the type check!!
setVar :: VarId -> Var -> State -> Error State
setVar vId val s@(State _ str _) = Ok $
  s{ stateStore = str{ storeVars = Map.insert vId val $ storeVars str }}

getTypeId :: Type PPos -> State -> Error TypeId
getTypeId (TInt _) _ = Ok 1
getTypeId (TBool _) _ = Ok 2
getTypeId (TString _) _ = Ok 3
getTypeId (TUser p (Ident tname)) (State _ _ scp) =
  errorFromMaybe (EDTypeNotFound tname p)
  $ Map.lookup tname $ scopeTypes scp

-- | This is reverse map lookup, which is slow, but is done only once, when
--   reporting the type error, in case when program is exploding anyway.
getTypeNameForED :: TypeId -> State -> String
getTypeNameForED tId (State _ _ scp)
  | tId == 0 = "void" -- TODO: KILL IT!
  | tId == 1 = "int"
  | tId == 2 = "bool"
  | tId == 3 = "string"
  | otherwise = Maybe.fromMaybe "*unknown*" $ do
      pair <- find ((tId ==) . snd) $ Map.toList $ scopeTypes scp
      return $ fst pair -- Should never hit the *unknown* case, but just for safety.

-- typeId is used to determine variable type. We can't use name becasue
-- TODO: explain and decide whether it is used or not.
varTypeId :: Var -> Int
-- varTypeId VEmpty = 0 -- TODO: possibly use Maybe instead?
varTypeId (VUninitialized tid) = tid -- Permitive types have constant typeids.
varTypeId (VInt _) = 1 -- Permitive types have constant typeids.
varTypeId (VBool _) = 2
varTypeId (VString _) = 3
varTypeId (VStruct sId _) = sId -- TODO: Structs know their typeids??

scope :: (State -> ErrorT IO State) -> State -> ErrorT IO State
scope fun st = do
  st' <- fun st
  return st'{ stateScope = stateScope st }
