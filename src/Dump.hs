-- TODO: Move dump functions to separate file and move nTabs and indent funcs outside?
data DumpState = DumpState
  {
    dSIndent :: Int,
    dSOpened :: [Int]
  }

dumpStateInitial :: DumpState
dumpStateInitial = DumpState 0 []

astDumpIdent :: DumpState -> Ident -> String
astDumpIdent state@(DumpState tabs opened) (Ident str) =
  indent ++ "[\"" ++ str ++ "\"]\n"
  where
    advState :: DumpState
    advState = state{ dSIndent = tabs + 1, dSOpened = tabs : opened }

    advState' :: DumpState
    advState' = state{ dSIndent = tabs + 1, dSOpened = opened }

    indentImpl :: Int -> String
    indentImpl i = (foldr (\i acc-> (if i `mod` 4 == 0 && (elem (i `div` 4) opened)
                                     then '|' else ' '): acc)
                     "" [0 .. (4 * (i - 1) - 1)])
                   ++ (if i > 0 then branchCh ++ "───" else "")

    indent :: String
    indent = indentImpl tabs

    indent' :: String
    indent' = indentImpl $ tabs + 1

    branchCh :: String
    branchCh = if elem (tabs - 1) opened then "├" else "└"

    nl :: String
    nl = "\n"

    fileLinum :: ParsingPos -> String
    fileLinum pPos = " ./tests.txt:" ++ showLinCol pPos

astDumpExpr :: DumpState -> Expr ParsingPos -> String
astDumpExpr state@(DumpState tabs opened) expr = case expr of
  _ -> indent ++ "[unknown expression]\n"
  where
    advState :: DumpState
    advState = state{ dSIndent = tabs + 1, dSOpened = tabs : opened }

    advState' :: DumpState
    advState' = state{ dSIndent = tabs + 1, dSOpened = opened }

    indentImpl :: Int -> String
    indentImpl i = (foldr (\i acc-> (if i `mod` 4 == 0 && (elem (i `div` 4) opened)
                                     then '|'
                                     else ' '): acc)
                     "" [0 .. (4 * (i - 1) - 1)])
                   ++ (if i > 0 then branchCh ++ "───" else "")

    indent :: String
    indent = indentImpl tabs

    indent' :: String
    indent' = indentImpl $ tabs + 1

    branchCh :: String
    branchCh = if elem (tabs - 1) opened then "├" else "└"

    nl :: String
    nl = "\n"

    fileLinum :: ParsingPos -> String
    fileLinum pPos = " ./tests.txt:" ++ showLinCol pPos

astDumpStmt :: DumpState -> Stmt ParsingPos -> String
astDumpStmt state@(DumpState tabs opened) stmt = case stmt of
  SIf pPos cExpr cStmt ->
    indent ++ "[IF]" ++ (fileLinum pPos) ++ nl ++
    astDumpExpr advState cExpr ++
    astDumpStmt advState' cStmt
  SIfElse pPos cExpr cStmt lStmt ->
    indent ++ "[IF-ELSE]" ++ (fileLinum pPos) ++ nl ++
    astDumpExpr advState cExpr ++
    astDumpStmt advState cStmt ++
    astDumpStmt advState' lStmt
  SFor pPos varName bExpr eExpr lstmt ->
    indent ++ "[FOR]" ++ (fileLinum pPos) ++ nl ++
    astDumpIdent advState varName ++
    astDumpExpr advState bExpr ++
    astDumpExpr advState eExpr ++
    astDumpStmt advState' lstmt
  SWhile pPos cExpr lstmt ->
    indent ++ "[WHILE]" ++ (fileLinum pPos) ++ nl ++
    astDumpExpr advState cExpr ++
    astDumpStmt advState' lstmt
  SExpr pPos expr ->
    indent ++ "[EXPR]" ++ (fileLinum pPos) ++ nl ++
    astDumpExpr advState' expr
  SVDecl pPos varDecl -> indent ++ "TODO\n"
  SFDecl pPos name funDecl -> indent ++ "TODO\n"
  SSDecl pPos name strcDecl -> indent ++ "TODO\n"
  STDecl pPos tupleTarget exprOrTuple -> indent ++ "TODO\n"
  _ -> indent ++ "[unknown statement]\n"
  where
    advState :: DumpState
    advState = state{ dSIndent = tabs + 1, dSOpened = tabs : opened }

    advState' :: DumpState
    advState' = state{ dSIndent = tabs + 1, dSOpened = opened }

    indentImpl :: Int -> String
    indentImpl i = (foldr (\i acc-> (if i `mod` 4 == 0 && (elem (i `div` 4) opened)
                                     then '|'
                                     else ' '): acc)
                     "" [0 .. (4 * (i - 1) - 1)])
                   ++ (if i > 0 then branchCh ++ "───" else "")

    indent :: String
    indent = indentImpl tabs

    indent' :: String
    indent' = indentImpl $ tabs + 1

    branchCh :: String
    branchCh = if elem (tabs - 1) opened then "├" else "└"

    nl :: String
    nl = "\n"

    fileLinum :: ParsingPos -> String
    fileLinum pPos = " ./tests.txt:" ++ showLinCol pPos
