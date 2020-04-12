-- | It's not a monad, sorry! -- TODO!
-- | All program-state related classes and functions.
module State where -- TODO: rename to runtime?

import qualified Data.Map.Strict as Map -- TODO: Explain why strict instead of lazy.

import AbsLanguage

import Parser
import Error

type VarId = Int
type FunId = Int
type TypeId = Int

type Struct = Map.Map String Var

data Var
  = VEmpty
  | VInt Int
  | VBool Bool
  | VString String
  | VStruct { vStructTId :: Int, vStructVars :: Struct }
  deriving (Show)

-- TODO: User can return a struct from a scope which defines it!!!!
-- Probably check in return?

-- TODO: Explain!!
data Scope = Scope
  {
    scopeVars :: Map.Map String VarId,
    scopeFuncs :: Map.Map String FunId,
    scopeTypes :: Map.Map String TypeId
  }
  deriving (Show)

data Store = Store
  {
    storeVars :: Map.Map VarId Var,
    -- TODO: Instead maybe should be PPos but can't include parser.
    --       Include this in parser?
    storeFuncs :: Map.Map FunId (Stmt (Maybe (Int, Int)), Scope), -- TODO: make a type instead of using pair?
    -- storeTypes :: Map.Map TypeId () -- TODO!

    nextVarId :: Int,
    nextFuncId :: Int,
    nextTypeId :: Int
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

-- TODO: This won't be used probably
-- getVar :: VarId -> State -> Error Var
-- getVar vId (State _ store _) =
  -- errorFromMaybe VarNotFoundError $ Map.lookup vId $ storeVars store

-- | Get variable by name
getVar :: String -> PPos -> State -> Error Var
getVar vname p (State _ store scp) =
  errorFromMaybe (EDVarNotFound vname p) $ do
  vId <- Map.lookup vname $ scopeVars scp -- Scope lookup.
  Map.lookup vId $ storeVars store -- Store lookup, should not fail.
