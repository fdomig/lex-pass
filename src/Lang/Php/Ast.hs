{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast (
  module Lang.Php.Ast.Common,
  module Lang.Php.Ast.ExprTypes,
  module Lang.Php.Ast.StmtTypes,
  Ast
  ) where

import Common
import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Char
import Lang.Php.Ast.Common
import Lang.Php.Ast.Expr
import Lang.Php.Ast.ExprTypes
import Lang.Php.Ast.Lex
import Lang.Php.Ast.Stmt
import Lang.Php.Ast.StmtTypes
import qualified Data.ByteString as BS
import qualified Data.Intercal as IC

data Ast = Ast TopLevel StmtList
  deriving (Eq, Show, Typeable, Data)

instance Unparse Ast where
  unparse (Ast t s) = unparse t ++ unparse s

instance Parse Ast where
  parse = liftM2 Ast parse stmtListParser

$(derive makeBinary ''Ast)
