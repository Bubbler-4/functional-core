{-|
    Common definitions for the Core language (Section 1.3, p.17-20)

    TODO: add prelude (Section 1.4, p.21) when the code is ready to test real programs.
-}
module Language.Core.Defs where

-- | An expression of the Core language
data Expr a
    = EVar Name             -- ^ Variable
    | ENum Int              -- ^ Number
    | EConstr Int Int       -- ^ Constructor (tag, arity)
    | EAp (Expr a) (Expr a) -- ^ Application
    | ELet                  -- ^ Let(rec) expr
        IsRec               -- ^ True if recursive
        [(a, Expr a)]       -- ^ Local defs
        (Expr a)            -- ^ Body
    | ECase                 -- ^ Case expr
        (Expr a)            -- ^ Expr to analyze cases
        [Alter a]           -- ^ Alternatives
    | ELam [a] (Expr a)     -- ^ Lambda abstraction
    deriving (Show)

type CoreExpr = Expr Name
type Name = String

-- | Let(rec) indicator
type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

-- | A case alternative (tag, bound vars, expr body)
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- | A supercombinator definition (name, args, expr body)
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- | A Core program as a list of supercombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name
