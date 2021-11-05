{-|
    Pretty-printing of a Core program (Section 1.5, p.21-)

    Exercise 1.1 and 1.4 are omitted because Haskell does not support counting steps.
-}
module Language.Core.Exercises.Pprint (pprint) where
import Language.Core.Defs

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-- | Abstract data type for structured string
data Iseq
    = INil
    | IStr String
    | IAppend Iseq Iseq
    | IIndent Iseq
    | INewline

-- | An object that represents an empty string
iNil :: Iseq
iNil = INil

-- | An object that wraps the given string
iStr :: String -> Iseq
iStr = IStr

-- | Append two objects (rewritten: Exercise 1.5)
iAppend :: Iseq -> Iseq -> Iseq
iAppend INil y = y
iAppend x INil = x
iAppend x y = IAppend x y

infixr 5 +++
(+++) = iAppend

-- | Newline object
iNewline :: Iseq
iNewline = INewline

-- | Indent the entire argument to the current column
iIndent :: Iseq -> Iseq
iIndent = IIndent

-- | Convert the constructed object to a string
iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq, 0)]

-- | Inner routine for iDisplay (Exercise 1.6, 1.7)
flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IStr s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs) = '\n' : replicate indent ' ' ++ flatten indent seqs
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)

-- | Concatenate a list of Iseq objects (Exercise 1.2)
iConcat :: [Iseq] -> Iseq
iConcat = foldr (+++) iNil

-- | Concatenate Iseq objects, inserting @sep@ in between (Exercise 1.2)
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep [] = iNil
iInterleave sep iseqs = foldr1 (\x y -> x +++ sep +++ y) iseqs

-- | Conversion function for Expr (Exercise 1.3)
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr $ show n
pprExpr (EConstr tag arity) = iConcat $ map iStr [ "Pack{", show tag, ", ", show arity, "}" ]
pprExpr (EAp e1 e2) = pprExpr e1 +++ iStr " " +++ pprAExpr e2
pprExpr (ELet isrec defns expr) = iConcat
    [ iStr keyword, iNewline
    , iStr "  ", iIndent (pprDefns defns), iNewline
    , iStr "in ", pprExpr expr
    ] where
    keyword = if isrec then "letrec" else "let"
pprExpr (ECase e alts) = iConcat
    [ iStr "case ", pprExpr e, iStr " of", iNewline
    , iStr "  ", iIndent (pprAlts alts)
    ]
pprExpr (ELam vars e) = iConcat
    [ iStr "\\ ", iInterleave (iStr " ") (map iStr vars)
    , iStr " . ", pprExpr e
    ]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e = if atomic e then pprExpr e else iConcat [ iStr "(", pprExpr e, iStr ")" ] where
    atomic (EVar _) = True
    atomic (ENum _) = True
    atomic (EConstr _ _) = True
    atomic _ = False

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns) where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat
    [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprAlts :: [Alter Name] -> Iseq
pprAlts alts = iInterleave sep (map pprAlt alts) where
    sep = iConcat [ iStr ";", iNewline ]

pprAlt :: Alter Name -> Iseq
pprAlt (tag, vars, expr) = iConcat
    [ iStr "<", iStr (show tag), iStr "> "
    , iInterleave (iStr " ") (map iStr vars)
    , iStr " -> ", pprExpr expr
    ]

-- | Conversion function for Program (Exercise 1.3)
pprProgram :: CoreProgram -> Iseq
pprProgram pgm = iInterleave iNewline (map pprScDefn pgm)

pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, vars, expr) = iConcat
    [ iInterleave (iStr " ") (iStr name : map iStr vars)
    , iStr " = ", pprExpr expr
    ]