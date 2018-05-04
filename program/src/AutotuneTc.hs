--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

{-# Language PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses #-}

module AutotuneTc where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH.Syntax hiding (Extension(..))
import Language.Haskell.Meta.Syntax.Translate
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.Exts.Parser hiding (parseExp, parseType, parsePat)
import qualified Language.Haskell.Exts.SrcLoc as Hs
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Parser (ParseMode(..), ParseResult(..))

autotune_tc :: QuasiQuoter
autotune_tc = QuasiQuoter { quoteExp  = \x -> returnQ (force_right (parseExp x))
                          , quotePat  = \x -> returnQ (force_right (parsePat x))
                          , quoteType = \x -> returnQ (force_right (parseType x))
                          , quoteDec  = \x -> returnQ (force_right (parseDecs x)) }

force_right :: (Show a) => Either String a -> a
force_right (Left err) = unsafePerformIO $ fail $ show err
force_right (Right x)  = x

{-# LANGUAGE CPP #-}
{- |
  Module      :  Language.Haskell.Meta.Parse
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}


parsePat :: String -> Either String Pat
parsePat = either Left (Right . toPat) . parseHsPat

parseExp :: String -> Either String Exp
parseExp = either Left (Right . toExp) . parseHsExp

parseType :: String -> Either String Type
parseType = either Left (Right . toType) . parseHsType

parseDecs :: String -> Either String [Dec]
parseDecs  = either Left (Right . toDecs) . parseHsDecls

parseMode :: ParseMode
parseMode = ParseMode
  {parseFilename = []
  ,baseLanguage = Haskell2010
  ,extensions = map EnableExtension myExtensions
  ,ignoreLinePragmas = False
  ,ignoreLanguagePragmas = False
  ,fixities = Nothing
  ,ignoreFunctionArity = False
  }

myExtensions :: [KnownExtension]
myExtensions = [PostfixOperators
                      ,QuasiQuotes
                      ,UnicodeSyntax
                      ,PatternSignatures
                      ,MagicHash
                      ,ForeignFunctionInterface
                      ,TemplateHaskell
                      ,RankNTypes
                      ,RecursiveDo
                      ,TypeFamilies
                      ,ScopedTypeVariables
                      ,DeriveDataTypeable
                      ,TypeSynonymInstances
                      ,FlexibleInstances]

parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk a) = Right a
parseResultToEither (ParseFailed loc e)
  = let line = Hs.srcLine loc - 1
    in Left (unlines [show line,show loc,e])

parseHsModule :: String -> Either String (Hs.Module Hs.SrcSpanInfo)
parseHsModule = parseResultToEither . parseModuleWithMode parseMode

parseHsDecls :: String -> Either String [Hs.Decl Hs.SrcSpanInfo]
parseHsDecls = either Left (Right . moduleDecls)
  . parseResultToEither . parseModuleWithMode parseMode


parseHsType :: String -> Either String (Hs.Type Hs.SrcSpanInfo)
parseHsType = parseResultToEither . parseTypeWithMode parseMode


parseHsExp :: String -> Either String (Hs.Exp Hs.SrcSpanInfo)
parseHsExp = parseResultToEither . parseExpWithMode parseMode

parseHsPat :: String -> Either String (Hs.Pat Hs.SrcSpanInfo)
parseHsPat = parseResultToEither . parsePatWithMode parseMode

pprHsModule :: Hs.Module Hs.SrcSpanInfo -> String
pprHsModule = prettyPrint


moduleDecls :: Hs.Module Hs.SrcSpanInfo -> [Hs.Decl Hs.SrcSpanInfo]
moduleDecls (Hs.Module _ _ _ _ x) = x

-- mkModule :: String -> Hs.Module
-- mkModule s = Hs.Module undefined (Hs.ModuleName s) Nothing [] []

emptyHsModule :: String -> Hs.Module Hs.SrcSpanInfo
emptyHsModule n =
    (Hs.Module
        noSrcSpanInfo
        (Just (Hs.ModuleHead noSrcSpanInfo (Hs.ModuleName noSrcSpanInfo n) Nothing Nothing))
        []
        []
        [])

noSrcSpanInfo = Hs.noInfoSpan (Hs.mkSrcSpan Hs.noLoc Hs.noLoc)
