{-# LANGUAGE OverloadedStrings #-}

import HindleyMilner
import HindleyMilner.Eval
import HindleyMilner.Infer
import HindleyMilner.Parse

import Control.Monad.Except (runExcept)
import Control.Monad.RWS.Strict (evalRWST)
import Data.Bifunctor (bimap, first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Test.Hspec
import Test.Hspec.QuickCheck

parseExpr :: Text -> Either String Expr
parseExpr = first P.errorBundlePretty . P.parse parser "" where
  parser = do
    expr <- exprParser
    P.eof
    pure expr

inferType :: Text -> Either String Text
inferType txt =
  parseExpr txt >>= bimap (T.unpack . displayTypeError) displayType . flip infer mempty

main :: IO ()
main = hspec $ do
  describe "Parse" $ do
    it "should parse a number" $ parseExpr "1" `shouldBe` Right (LitE (DoubleL 1))
    it "should parse a boolean" $ parseExpr "True" `shouldBe` Right (LitE (BoolL True))
    it "should parse a string" $ parseExpr "\"foo\"" `shouldBe` Right (LitE (StringL "foo"))
    it "should parse the identity function" $ do
      let txt = "let f x = x in f"
      parseExpr txt `shouldBe` Right (LetE (Binding "f" ["x"] (LambdaE "x" (VarE "x"))) (VarE "f"))
  describe "Infer" $ do
    it "should fail when parsing an unbound variable" $
      inferType "let a = b in a" `shouldBe` Left "identifier b was not declared"
    it "should infer a number's type" $ inferType "1" `shouldBe` Right "Double"
    it "should infer a boolean's binding's type" $
      inferType "let b = False in b" `shouldBe` Right "Bool"
    it "should infer the identity function's type" $
      inferType "let f x = x in f" `shouldBe` Right "$1 -> $1"
    it "should infer the identity function's type" $
      inferType "let f x = x in f \"foo\"" `shouldBe` Right "String"
    it "should infer the type of a higher order function" $
      inferType "let f g = g \"foobar\" in f" `shouldBe` Right "(String -> $1) -> $1"
    it "should fail when parsing an unbound variable" $
      inferType "let a = b in a" `shouldBe` Left "identifier b was not declared"
    it "should infer a number's type" $ inferType "1" `shouldBe` Right "Double"
    it "should infer a boolean's binding's type" $
      inferType "let b = False in b" `shouldBe` Right "Bool"
    it "should infer the identity function's type" $
      inferType "let f x = x in f" `shouldBe` Right "$1 -> $1"
    it "should infer the identity function's type particularized to string" $
      inferType "let f x = x in f \"foo\"" `shouldBe` Right "String"
    it "should infer the type of a higher order function" $
      inferType "let f g = g \"foobar\" in f" `shouldBe` Right "(String -> $1) -> $1"
    it "should infer the type of fix" $
      inferType "let fix f = let a = f a in a in fix" `shouldBe` Right "($1 -> $1) -> $1"
