{-# LANGUAGE CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

{- |
  Module      :  Language.Haskell.Meta.Syntax.Translate
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.Meta.Syntax.Translate (
    module Language.Haskell.Meta.Syntax.Translate
) where

import Data.Char (ord)
import Data.Typeable
import Data.List (foldl', nub, (\\))
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.Exts.SrcLoc as Hs
#if MIN_VERSION_haskell_src_exts(1,18,0)
import qualified Language.Haskell.Exts.Syntax as Hs
#else
import qualified Language.Haskell.Exts.Annotated.Syntax as Hs
#endif

-----------------------------------------------------------------------------


class ToName a where toName :: a -> Name
class ToNames a where toNames :: a -> [Name]
class ToLit  a where toLit  :: a -> Lit
class ToType a where toType :: a -> Type
class ToPat  a where toPat  :: a -> Pat
class ToExp  a where toExp  :: a -> Exp
class ToDecs a where toDecs :: a -> [Dec]
class ToDec  a where toDec  :: a -> Dec
class ToStmt a where toStmt :: a -> Stmt
class ToLoc  a where toLoc  :: a -> Loc
class ToCxt  a where toCxt  :: a -> Cxt
class ToPred a where toPred :: a -> Pred
class ToTyVars a where toTyVars :: a -> [TyVarBndr]
#if MIN_VERSION_haskell_src_exts(1,18,0)
class ToMaybeKind a where toMaybeKind :: a -> Maybe Kind
#endif
#if MIN_VERSION_template_haskell(2,11,0)
class ToInjectivityAnn a where toInjectivityAnn :: a -> InjectivityAnn
#endif

#if MIN_VERSION_template_haskell(2,12,0)
#elif MIN_VERSION_template_haskell(2,11,0)
type DerivClause = Pred
#else
type DerivClause = Name
#endif

class ToDerivClauses a where toDerivClauses :: a -> [DerivClause]

-- for error messages
moduleName = "Language.Haskell.Meta.Syntax.Translate"

-- When to use each of these isn't always clear: prefer 'todo' if unsure.
noTH :: (Functor f, Show (f ())) => String -> f e -> a
noTH fun thing = error . concat $ [moduleName, ".", fun,
  ": template-haskell has no representation for: ", show (fmap (const ()) thing)]

noTHyet :: (Functor f, Show (f ())) => String -> String -> f e -> a
noTHyet fun minVersion thing = error . concat $ [moduleName, ".", fun,
  ": template-haskell-", VERSION_template_haskell, " (< ", minVersion, ")",
  " has no representation for: ", show (fmap (const ()) thing)]

todo :: (Functor f, Show (f ())) => String -> f e -> a
todo fun thing = error . concat $ [moduleName, ".", fun,
  ": not implemented: ", show (fmap (const ()) thing)]

nonsense :: (Functor f, Show (f ())) => String -> String -> f e -> a
nonsense fun inparticular thing = error . concat $ [moduleName, ".", fun,
  ": nonsensical: ", inparticular, ": ", show (fmap (const ()) thing)]

-----------------------------------------------------------------------------


instance ToExp Lit where
  toExp = LitE
instance (ToExp a) => ToExp [a] where
  toExp = ListE . fmap toExp
instance (ToExp a, ToExp b) => ToExp (a,b) where
  toExp (a,b) = TupE [toExp a, toExp b]
instance (ToExp a, ToExp b, ToExp c) => ToExp (a,b,c) where
  toExp (a,b,c) = TupE [toExp a, toExp b, toExp c]
instance (ToExp a, ToExp b, ToExp c, ToExp d) => ToExp (a,b,c,d) where
  toExp (a,b,c,d) = TupE [toExp a, toExp b, toExp c, toExp d]


instance ToPat Lit where
  toPat = LitP
instance (ToPat a) => ToPat [a] where
  toPat = ListP . fmap toPat
instance (ToPat a, ToPat b) => ToPat (a,b) where
  toPat (a,b) = TupP [toPat a, toPat b]
instance (ToPat a, ToPat b, ToPat c) => ToPat (a,b,c) where
  toPat (a,b,c) = TupP [toPat a, toPat b, toPat c]
instance (ToPat a, ToPat b, ToPat c, ToPat d) => ToPat (a,b,c,d) where
  toPat (a,b,c,d) = TupP [toPat a, toPat b, toPat c, toPat d]


instance ToLit Char where
  toLit = CharL
instance ToLit String where
  toLit = StringL
instance ToLit Integer where
  toLit = IntegerL
instance ToLit Int where
  toLit = IntegerL . toInteger
instance ToLit Float where
  toLit = RationalL . toRational
instance ToLit Double where
  toLit = RationalL . toRational


-----------------------------------------------------------------------------


-- * ToName {String,HsName,Module,HsSpecialCon,HsQName}


instance ToName String where
  toName = mkName

instance ToName (Hs.Name l) where
  toName (Hs.Ident _ s) = toName s
  toName (Hs.Symbol _ s) = toName s

instance ToName (Hs.SpecialCon l) where
  toName (Hs.UnitCon _) = '()
  toName (Hs.ListCon _) = '[]
  toName (Hs.FunCon _)  = ''(->)
  toName (Hs.TupleCon _ _ n)
    | n<2 = '()
    | otherwise =
      let x = maybe [] (++".") (nameModule '(,))
      in mkName . concat $ x : ["(",replicate (n-1) ',',")"]
  toName (Hs.Cons _)    = '(:)


instance ToName (Hs.QName l) where
--  toName (Hs.Qual (Hs.Module []) n) = toName n
  toName (Hs.Qual _ (Hs.ModuleName _ []) n) = toName n
  toName (Hs.Qual _ (Hs.ModuleName _ m) n) =
    let m' = show . toName $ m
        n' = show . toName $ n
    in toName . concat $ [m',".",n']
  toName (Hs.UnQual _ n) = toName n
  toName (Hs.Special _ s) = toName s


instance ToName (Hs.Op l) where
  toName (Hs.VarOp _ n) = toName n
  toName (Hs.ConOp _ n) = toName n


-----------------------------------------------------------------------------

-- * ToLit HsLiteral


instance ToLit (Hs.Literal l) where
  toLit (Hs.Char _ a _) = CharL a
  toLit (Hs.String _ a _) = StringL a
  toLit (Hs.Int _ a _) = IntegerL a
  toLit (Hs.Frac _ a _) = RationalL a
  toLit l@Hs.PrimChar{} = noTH "toLit" l
  toLit (Hs.PrimString _ a _) = StringPrimL (map toWord8 a)
   where
    toWord8 = fromIntegral . ord
  toLit (Hs.PrimInt _ a _) = IntPrimL a
  toLit (Hs.PrimFloat _ a _) = FloatPrimL a
  toLit (Hs.PrimDouble _ a _) = DoublePrimL a
  toLit (Hs.PrimWord _ a _) = WordPrimL a


-----------------------------------------------------------------------------

-- * ToPat HsPat


instance Show l => ToPat (Hs.Pat l) where
  toPat (Hs.PVar _ n)
    = VarP (toName n)
  toPat (Hs.PLit _ (Hs.Signless _) l)
    = LitP (toLit l)
  toPat (Hs.PLit _ (Hs.Negative _) l) = LitP $ case toLit l of
    IntegerL z -> IntegerL (negate z)
    RationalL q -> RationalL (negate q)
    IntPrimL z' -> IntPrimL (negate z')
    FloatPrimL r' -> FloatPrimL (negate r')
    DoublePrimL r'' -> DoublePrimL (negate r'')
    _ -> nonsense "toPat" "negating wrong kind of literal" l
  toPat (Hs.PInfixApp _ p n q) = UInfixP (toPat p) (toName n) (toPat q)
  toPat (Hs.PApp _ n ps) = ConP (toName n) (fmap toPat ps)
  toPat (Hs.PTuple _ Hs.Boxed ps) = TupP (fmap toPat ps)
  toPat (Hs.PTuple _ Hs.Unboxed ps) = UnboxedTupP (fmap toPat ps)
  toPat (Hs.PList _ ps) = ListP (fmap toPat ps)
  toPat (Hs.PParen _ p) = ParensP (toPat p)
  toPat (Hs.PRec _ n pfs) = let toFieldPat (Hs.PFieldPat _ n p) = (toName n, toPat p)
                            in RecP (toName n) (fmap toFieldPat pfs)
  toPat (Hs.PAsPat _ n p) = AsP (toName n) (toPat p)
  toPat (Hs.PWildCard _) = WildP
  toPat (Hs.PIrrPat _ p) = TildeP (toPat p)
  toPat (Hs.PatTypeSig _ p t) = SigP (toPat p) (toType t)
  toPat (Hs.PViewPat _ e p) = ViewP (toExp e) (toPat p)
  -- regular pattern
  toPat p@Hs.PRPat{} = noTH "toPat" p
  -- XML stuff
  toPat p@Hs.PXTag{} = noTH "toPat" p
  toPat p@Hs.PXETag{} = noTH "toPat" p
  toPat p@Hs.PXPcdata{} = noTH "toPat" p
  toPat p@Hs.PXPatTag{} = noTH "toPat" p
  toPat (Hs.PBangPat _ p) = BangP (toPat p)
  toPat p = todo "toPat" p

-----------------------------------------------------------------------------

-- * ToExp HsExp

instance Show l => ToExp (Hs.QOp l) where
  toExp (Hs.QVarOp _ n) = VarE (toName n)
  toExp (Hs.QConOp _ n) = ConE (toName n)

toFieldExp :: Show l => Hs.FieldUpdate l -> FieldExp
toFieldExp (Hs.FieldUpdate _ n e) = (toName n, toExp e)




instance Show l => ToExp (Hs.Exp l) where
  toExp (Hs.Var _ n)                 = VarE (toName n)
  toExp e@Hs.IPVar{}               = noTH "toExp" e
  toExp (Hs.Con _ n)                 = ConE (toName n)
  toExp (Hs.Lit _ l)                 = LitE (toLit l)
  toExp (Hs.InfixApp _ e o f)        = UInfixE (toExp e) (toExp o) (toExp f)
  toExp (Hs.App _ e f)               = AppE (toExp e) (toExp f)
  toExp (Hs.NegApp _ e)              = AppE (VarE 'negate) (toExp e)
  toExp (Hs.Lambda _ ps e)         = LamE (fmap toPat ps) (toExp e)
  toExp (Hs.Let _ bs e)              = LetE (toDecs bs) (toExp e)
  toExp (Hs.If _ a b c)              = CondE (toExp a) (toExp b) (toExp c)
  toExp (Hs.MultiIf _ ifs)           = MultiIfE (map toGuard ifs)
  toExp (Hs.Case _ e alts)           = CaseE (toExp e) (map toMatch alts)
  toExp (Hs.Do _ ss)                 = DoE (map toStmt ss)
  toExp e@(Hs.MDo _ _)               = noTH "toExp" e
  toExp (Hs.Tuple _ Hs.Boxed xs)     = TupE (fmap toExp xs)
  toExp (Hs.Tuple _ Hs.Unboxed xs)   = UnboxedTupE (fmap toExp xs)
  toExp e@Hs.TupleSection{}        = noTH "toExp" e
  toExp (Hs.List _ xs)               = ListE (fmap toExp xs)
  toExp (Hs.Paren _ e)               = ParensE (toExp e)
  toExp (Hs.LeftSection _ e o)       = InfixE (Just . toExp $ e) (toExp o) Nothing
  toExp (Hs.RightSection _ o f)      = InfixE Nothing (toExp o) (Just . toExp $ f)
  toExp (Hs.RecConstr _ n xs)        = RecConE (toName n) (fmap toFieldExp xs)
  toExp (Hs.RecUpdate _ e xs)        = RecUpdE (toExp e) (fmap toFieldExp xs)
  toExp (Hs.EnumFrom _ e)            = ArithSeqE $ FromR (toExp e)
  toExp (Hs.EnumFromTo _ e f)        = ArithSeqE $ FromToR (toExp e) (toExp f)
  toExp (Hs.EnumFromThen _ e f)      = ArithSeqE $ FromThenR (toExp e) (toExp f)
  toExp (Hs.EnumFromThenTo _ e f g)  = ArithSeqE $ FromThenToR (toExp e) (toExp f) (toExp g)
  toExp (Hs.ListComp _ e ss)         = CompE $ map convert ss ++ [NoBindS (toExp e)]
   where
    convert (Hs.QualStmt _ st) = toStmt st
    convert s = noTH "toExp ListComp" s
  toExp (Hs.ExpTypeSig _ e t)      = SigE (toExp e) (toType t)
  toExp e = todo "toExp" e


toMatch :: Show l => Hs.Alt l -> Match
toMatch (Hs.Alt _ p rhs ds) = Match (toPat p) (toBody rhs) (toDecs ds)

toBody :: Show l => Hs.Rhs l -> Body
toBody (Hs.UnGuardedRhs _ e) = NormalB $ toExp e
toBody (Hs.GuardedRhss _ rhss) = GuardedB $ map toGuard rhss

toGuard (Hs.GuardedRhs _ stmts e) = (g, toExp e)
  where
    g = case map toStmt stmts of
      [NoBindS x] -> NormalG x
      xs -> PatG xs

instance ToDecs a => ToDecs (Maybe a) where
    toDecs Nothing = []
    toDecs (Just a) = toDecs a

instance Show l => ToDecs (Hs.Binds l) where
  toDecs (Hs.BDecls _ ds)   = toDecs ds
  toDecs a@(Hs.IPBinds {}) = noTH "ToDecs Hs.Binds" a

instance Show l => ToDecs (Hs.ClassDecl l) where
  toDecs (Hs.ClsDecl _ d) = toDecs d
  toDecs x = todo "classDecl" x

-----------------------------------------------------------------------------

-- * ToLoc SrcLoc

instance ToLoc Hs.SrcLoc where
  toLoc (Hs.SrcLoc fn l c) =
    Loc fn [] [] (l,c) (-1,-1)

-----------------------------------------------------------------------------

-- * ToType HsType

instance ToName (Hs.TyVarBind l) where
  toName (Hs.KindedVar _ n _) = toName n
  toName (Hs.UnkindedVar _ n) = toName n

instance ToName Name where
  toName = id

instance ToName TyVarBndr where
  toName (PlainTV n) = n
  toName (KindedTV n _) = n

instance ToType (Hs.Kind l) where
  toType (Hs.KindStar _) = StarT
  toType (Hs.KindFn _ k1 k2) = toType k1 .->. toType k2
  toType (Hs.KindParen _ kp) = toType kp
  toType (Hs.KindVar _ n) = VarT (toName n)

toKind :: Hs.Kind l -> Kind
toKind = toType

toTyVar :: Hs.TyVarBind l -> TyVarBndr
toTyVar (Hs.KindedVar _ n k) = KindedTV (toName n) (toKind k)
toTyVar (Hs.UnkindedVar _ n) = PlainTV (toName n)

instance Show l => ToType (Hs.Type l) where
  toType (Hs.TyForall _ tvbM cxt t) = ForallT (maybe [] (fmap toTyVar) tvbM) (toCxt cxt) (toType t)
  toType (Hs.TyFun _ a b) = toType a .->. toType b
  toType (Hs.TyList _ t) = ListT `AppT` toType t
  toType (Hs.TyTuple _ b ts) = foldAppT (tuple . length $ ts) (fmap toType ts)
   where
    tuple = case b of
      Hs.Boxed -> TupleT
      Hs.Unboxed -> UnboxedTupleT
  toType (Hs.TyApp _ a b) = AppT (toType a) (toType b)
  toType (Hs.TyVar _ n) = VarT (toName n)
  toType (Hs.TyCon _ qn) = ConT (toName qn)
  toType (Hs.TyParen _ t) = toType t
  -- XXX: need to wrap the name in parens!
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toType (Hs.TyInfix _ a (Hs.UnpromotedName _ o) b) =
#else
  toType (Hs.TyInfix _ a o b) =
#endif
    AppT (AppT (ConT (toName o)) (toType a)) (toType b)
  toType (Hs.TyKind _ t k) = SigT (toType t) (toKind k)
  toType t@Hs.TyBang{} =
    nonsense "toType" "type cannot have strictness annotations in this context" t


toStrictType :: Show l => Hs.Type l -> StrictType
#if MIN_VERSION_template_haskell(2,11,0)
toStrictType (Hs.TyBang _ s u t) = (Bang (toUnpack u) (toStrict s), toType t)
    where
      toStrict (Hs.LazyTy _) = SourceLazy
      toStrict (Hs.BangedTy _) = SourceStrict
      toStrict (Hs.NoStrictAnnot _) = NoSourceStrictness
      toUnpack (Hs.Unpack _) = SourceUnpack
      toUnpack (Hs.NoUnpack _) = SourceNoUnpack
      toUnpack (Hs.NoUnpackPragma _) = NoSourceUnpackedness
toStrictType x = (Bang NoSourceUnpackedness NoSourceStrictness, toType x)
#elif MIN_VERSION_haskell_src_exts(1,18,0)
-- TyBang l (BangType l) (Unpackedness l) (Type l)
-- data BangType l = BangedTy l	| LazyTy l | NoStrictAnnot l
-- data Unpackedness l = Unpack l | NoUnpack l | NoUnpackPragma l
toStrictType (Hs.TyBang _ b u t) = (toStrict b u, toType t)
    where
      toStrict :: Hs.BangType l -> Hs.Unpackedness l -> Strict
      toStrict (Hs.BangedTy _) _ = IsStrict
      toStrict _ (Hs.Unpack _) = Unpacked
      toStrict _ _ = NotStrict
toStrictType x = (NotStrict, toType x)
#else
toStrictType t@(Hs.TyBang _ _ Hs.TyBang{}) =
  nonsense "toStrictType" "double strictness annotation" t
toStrictType (Hs.TyBang _ (Hs.BangedTy _) t) = (IsStrict, toType t)
toStrictType (Hs.TyBang _ (Hs.UnpackedTy _) t) = (Unpacked, toType t)
toStrictType t = (NotStrict, toType t)
#endif


(.->.) :: Type -> Type -> Type
a .->. b = AppT (AppT ArrowT a) b

instance Show l => ToPred (Hs.Asst l) where
#if MIN_VERSION_template_haskell(2,10,0)
    toPred (Hs.ClassA _ n ts) = foldl' AppT (ConT (toName n)) (fmap toType ts)
    toPred (Hs.InfixA _ t1 n t2) = foldl' AppT (ConT (toName n)) (fmap toType [t1,t2])
    toPred (Hs.EqualP _ t1 t2) = foldl' AppT EqualityT (fmap toType [t1,t2])
    toPred (Hs.ParenA _ a) = toPred a
#else
    toPred (Hs.ClassA _ n ts) = ClassP (toName n) (fmap toType ts)
    toPred (Hs.InfixA _ t1 n t2) = ClassP (toName n) (fmap toType [t1, t2])
    toPred (Hs.EqualP _ t1 t2) = EqualP (toType t1) (toType t2)
    toPred (Hs.ParenA _ a) = toPred a
#endif
    toPred a@Hs.IParam{} = noTH "toCxt" a
    toPred p = todo ("toPred" ++ show p) p

#if MIN_VERSION_template_haskell(2,12,0)
instance Show l => ToDerivClauses (Hs.Deriving l) where
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toDerivClauses (Hs.Deriving _ strat irules) = [DerivClause (fmap toDerivStrategy strat) (map toType irules)]
#else
  toDerivClauses (Hs.Deriving _ irules) = [DerivClause Nothing (map toType irules)]
#endif
#else
instance ToDerivClauses (Hs.Deriving l) where
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toDerivClauses (Hs.Deriving _ _ irules) =
#else
  toDerivClauses (Hs.Deriving _ irules) =
#endif
#if MIN_VERSION_template_haskell(2,11,0)
    map toType irules
#else
    concatMap toNames irules
#endif
#endif

instance ToDerivClauses a => ToDerivClauses (Maybe a) where
  toDerivClauses Nothing  = []
  toDerivClauses (Just a) = toDerivClauses a

instance ToDerivClauses a => ToDerivClauses [a] where
  toDerivClauses = concatMap toDerivClauses

#if MIN_VERSION_template_haskell(2,12,0) && MIN_VERSION_haskell_src_exts(1,20,0)
toDerivStrategy :: (Hs.DerivStrategy l) -> DerivStrategy
toDerivStrategy (Hs.DerivStock _) = StockStrategy
toDerivStrategy (Hs.DerivAnyclass _) = AnyclassStrategy
toDerivStrategy (Hs.DerivNewtype _) = NewtypeStrategy
#endif

foldAppT :: Type -> [Type] -> Type
foldAppT t ts = foldl' AppT t ts

-----------------------------------------------------------------------------

-- * ToStmt HsStmt

instance Show l => ToStmt (Hs.Stmt l) where
  toStmt (Hs.Generator _ p e)  = BindS (toPat p) (toExp e)
  toStmt (Hs.Qualifier _ e)      = NoBindS (toExp e)
  toStmt a@(Hs.LetStmt _ bnds)   = LetS (toDecs bnds)
  toStmt s@Hs.RecStmt{}        = noTH "toStmt" s


-----------------------------------------------------------------------------

-- * ToDec HsDecl

instance Show l => ToDec (Hs.Decl l) where
  toDec (Hs.TypeDecl _ h t)
    = TySynD (toName h) (toTyVars h) (toType t)

  toDec a@(Hs.DataDecl  _ dOrN cxt h qcds qns)
    = case dOrN of
        Hs.DataType _ -> DataD (toCxt cxt)
                             (toName h)
                             (toTyVars h)
#if MIN_VERSION_template_haskell(2,11,0)
                             Nothing
#endif
                             (fmap qualConDeclToCon qcds)
                             (toDerivClauses qns)
        Hs.NewType _  -> let qcd = case qcds of
                                     [x] -> x
                                     _   -> nonsense "toDec" ("newtype with " ++
                                                              "wrong number of constructors") a
                        in NewtypeD (toCxt cxt)
                                    (toName h)
                                    (toTyVars h)
#if MIN_VERSION_template_haskell(2,11,0)
                                    Nothing
#endif
                                    (qualConDeclToCon qcd)
                                    (toDerivClauses qns)

  -- This type-signature conversion is just wrong.
  -- Type variables need to be dealt with. /Jonas
  toDec a@(Hs.TypeSig _ ns t)
    -- XXXXXXXXXXXXXX: oh crap, we can't return a [Dec] from this class!
    = let xs = fmap (flip SigD (toType t) . toName) ns
      in case xs of x:_ -> x; [] -> error "toDec: malformed TypeSig!"

  toDec (Hs.InlineConlikeSig _ act qn) = PragmaD $
    InlineP (toName qn) Inline ConLike (transAct act)
  toDec (Hs.InlineSig _ b act qn) = PragmaD $
    InlineP (toName qn) inline FunLike (transAct act)
   where
    inline | b = Inline | otherwise = NoInline

#if MIN_VERSION_template_haskell(2,11,0)
  toDec (Hs.TypeFamDecl _ h sig inj)
    = OpenTypeFamilyD $ TypeFamilyHead (toName h)
                                       (toTyVars h)
                                       (maybe NoSig KindSig . toMaybeKind $ sig)
                                       (fmap toInjectivityAnn inj)
  toDec (Hs.DataFamDecl _ _ h sig)
    = DataFamilyD (toName h) (toTyVars h) (toMaybeKind sig)
#elif MIN_VERSION_haskell_src_exts(1,18,0)
  toDec (Hs.TypeFamDecl _ h sig inj)
    = FamilyD TypeFam (toName h) (toTyVars h) (toMaybeKind sig)
  toDec (Hs.DataFamDecl _ _ h sig)
    = FamilyD DataFam (toName h) (toTyVars h) (toMaybeKind sig)
#else
  toDec (Hs.TypeFamDecl _ h k)
    = FamilyD TypeFam (toName h) (toTyVars h) (fmap toKind k)

  -- TODO: do something with context?
  toDec (Hs.DataFamDecl _ _ h k)
    = FamilyD DataFam (toName h) (toTyVars h) (fmap toKind k)
#endif

  toDec a@(Hs.FunBind _ mtchs)                           = hsMatchesToFunD mtchs
  toDec (Hs.PatBind _ p rhs bnds)                      = ValD (toPat p)
                                                              (hsRhsToBody rhs)
                                                              (toDecs bnds)

  toDec i@(Hs.InstDecl _ (Just overlap) _ _) =
    noTH "toDec" (fmap (const ()) overlap, i)

  -- the 'vars' bit seems to be for: instance forall a. C (T a) where ...
  -- TH's own parser seems to flat-out ignore them, and honestly I can't see
  -- that it's obviously wrong to do so.
#if MIN_VERSION_template_haskell(2,11,0)
  toDec (Hs.InstDecl _ Nothing irule ids) = InstanceD
    Nothing
    (toCxt irule)
    (toType irule)
    (toDecs ids)
#else
  toDec (Hs.InstDecl _ Nothing irule ids) = InstanceD
    (toCxt irule)
    (toType irule)
    (toDecs ids)
#endif

  toDec (Hs.ClassDecl _ cxt h fds decls) = ClassD
    (toCxt cxt)
    (toName h)
    (toTyVars h)
    (fmap toFunDep fds)
    (toDecs decls)
   where
    toFunDep (Hs.FunDep _ ls rs) = FunDep (fmap toName ls) (fmap toName rs)

  toDec x = todo "toDec" x

#if MIN_VERSION_haskell_src_exts(1,18,0)
instance ToMaybeKind (Hs.ResultSig l) where
    toMaybeKind (Hs.KindSig _ k) = Just $ toKind k
    toMaybeKind (Hs.TyVarSig _ _) = Nothing

instance ToMaybeKind a => ToMaybeKind (Maybe a) where
    toMaybeKind Nothing = Nothing
    toMaybeKind (Just a) = toMaybeKind a
#endif

#if MIN_VERSION_template_haskell(2,11,0)
instance ToInjectivityAnn (Hs.InjectivityInfo l) where
  toInjectivityAnn (Hs.InjectivityInfo _ n ns) = InjectivityAnn (toName n) (fmap toName ns)
#endif

transAct :: Maybe (Hs.Activation l) -> Phases
transAct Nothing = AllPhases
transAct (Just (Hs.ActiveFrom _ n)) = FromPhase n
transAct (Just (Hs.ActiveUntil _ n)) = BeforePhase n

instance ToName (Hs.DeclHead l) where
  toName (Hs.DHead _ n) = toName n
  toName (Hs.DHInfix _ _ n) = toName n
  toName (Hs.DHParen _ h) = toName h
  toName (Hs.DHApp _ h _) = toName h

instance ToTyVars (Hs.DeclHead l) where
  toTyVars (Hs.DHead _ _) = []
  toTyVars (Hs.DHParen _ h) = toTyVars h
  toTyVars (Hs.DHInfix _ tvb _) = [toTyVar tvb]
  toTyVars (Hs.DHApp _ h tvb) = toTyVars h ++ [toTyVar tvb]

instance ToNames a => ToNames (Maybe a) where
  toNames Nothing = []
  toNames (Just a) = toNames a

instance ToNames (Hs.Deriving l) where
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toNames (Hs.Deriving _ _ irules) =
#else
  toNames (Hs.Deriving _ irules) =
#endif
    concatMap toNames irules
instance ToNames (Hs.InstRule l) where
  toNames (Hs.IParen _ irule) = toNames irule
  toNames (Hs.IRule _ _mtvbs _mcxt mihd) = toNames mihd
instance ToNames (Hs.InstHead l) where
  toNames (Hs.IHCon _ n) = [toName n]
  toNames (Hs.IHInfix _ _ n) = [toName n]
  toNames (Hs.IHParen _ h) = toNames h
  toNames (Hs.IHApp _ h _) = toNames h

instance Show l => ToCxt (Hs.InstRule l) where
  toCxt (Hs.IRule _ _ cxt _) = toCxt cxt
  toCxt (Hs.IParen _ irule) = toCxt irule

instance Show l => ToCxt (Hs.Context l) where
  toCxt x = case x of
              Hs.CxEmpty _ -> []
              Hs.CxSingle _ x' -> [toPred x']
              Hs.CxTuple _ xs -> fmap toPred xs

instance (ToCxt a,Show a) => ToCxt (Maybe a) where
    toCxt Nothing = []
    toCxt (Just a) = toCxt a

instance Show l => ToType (Hs.InstRule l) where
    toType (Hs.IRule _ _ _ h) = toType h
    toType (Hs.IParen _ irule) = toType irule

instance Show l => ToType (Hs.InstHead l) where
    toType (Hs.IHCon _ qn) = toType qn
    toType (Hs.IHInfix _ typ qn) = AppT (toType typ) (toType qn)
    toType (Hs.IHParen _ hd) = toType hd
    toType (Hs.IHApp _ hd typ) = AppT (toType hd) (toType typ)

qualConDeclToCon :: Show l => Hs.QualConDecl l -> Con
qualConDeclToCon (Hs.QualConDecl _ Nothing Nothing cdecl) = conDeclToCon cdecl
qualConDeclToCon (Hs.QualConDecl _ ns cxt cdecl) = ForallC (toTyVars ns)
                                                    (toCxt cxt)
                                                    (conDeclToCon cdecl)

instance ToTyVars a => ToTyVars (Maybe a) where
  toTyVars Nothing = []
  toTyVars (Just a) = toTyVars a

instance ToTyVars a => ToTyVars [a] where
  toTyVars = concatMap toTyVars

instance ToTyVars (Hs.TyVarBind l) where
  toTyVars tvb = [toTyVar tvb]

instance ToType (Hs.QName l) where
    toType = ConT . toName

conDeclToCon :: Show l => Hs.ConDecl l -> Con
conDeclToCon (Hs.ConDecl _ n tys)
  = NormalC (toName n) (map toStrictType tys)
conDeclToCon (Hs.RecDecl _ n fieldDecls)
  = RecC (toName n) (concatMap convField fieldDecls)
  where
    convField :: Show l => Hs.FieldDecl l -> [VarStrictType]
    convField (Hs.FieldDecl _ ns t) =
      let (strict, ty) = toStrictType t
      in map (\n' -> (toName n', strict, ty)) ns


hsMatchesToFunD :: Show l => [Hs.Match l] -> Dec
hsMatchesToFunD [] = FunD (mkName []) []   -- errorish
hsMatchesToFunD xs@(Hs.Match _ n _ _ _ : _) = FunD (toName n) (fmap hsMatchToClause xs)
hsMatchesToFunD xs@(Hs.InfixMatch _ _ n _ _ _ : _) = FunD (toName n) (fmap hsMatchToClause xs)


hsMatchToClause :: Show l => Hs.Match l -> Clause
hsMatchToClause (Hs.Match _ _ ps rhs bnds) = Clause
                                                (fmap toPat ps)
                                                (hsRhsToBody rhs)
                                                (toDecs bnds)
hsMatchToClause (Hs.InfixMatch _ p _ ps rhs bnds) = Clause
                                                        (fmap toPat (p:ps))
                                                        (hsRhsToBody rhs)
                                                        (toDecs bnds)



hsRhsToBody :: Show l => Hs.Rhs l -> Body
hsRhsToBody (Hs.UnGuardedRhs _ e) = NormalB (toExp e)
hsRhsToBody (Hs.GuardedRhss _ hsgrhs) = let fromGuardedB (GuardedB a) = a
                                      in GuardedB . concat
                                          . fmap (fromGuardedB . hsGuardedRhsToBody)
                                              $ hsgrhs



hsGuardedRhsToBody :: Show l => Hs.GuardedRhs l -> Body
hsGuardedRhsToBody (Hs.GuardedRhs _ [] e)  = NormalB (toExp e)
hsGuardedRhsToBody (Hs.GuardedRhs _ [s] e) = GuardedB [(hsStmtToGuard s, toExp e)]
hsGuardedRhsToBody (Hs.GuardedRhs _ ss e)  = let ss' = fmap hsStmtToGuard ss
                                                 (pgs,ngs) = unzip [(p,n)
                                                               | (PatG p) <- ss'
                                                               , n@(NormalG _) <- ss']
                                                 e' = toExp e
                                                 patg = PatG (concat pgs)
                                            in GuardedB $ (patg,e') : zip ngs (repeat e')



hsStmtToGuard :: Show l => Hs.Stmt l -> Guard
hsStmtToGuard (Hs.Generator _ p e) = PatG [BindS (toPat p) (toExp e)]
hsStmtToGuard (Hs.Qualifier _ e)     = NormalG (toExp e)
hsStmtToGuard (Hs.LetStmt _ bs)      = PatG [LetS (toDecs bs)]


-----------------------------------------------------------------------------

-- * ToDecs InstDecl
instance Show l => ToDecs (Hs.InstDecl l) where
  toDecs (Hs.InsDecl _ decl) = toDecs decl
  toDecs d              = todo "toDec" d

-- * ToDecs HsDecl HsBinds

instance Show l => ToDecs (Hs.Decl l) where
  toDecs a@(Hs.TypeSig _ ns t)
    = let xs = fmap (flip SigD (toType t) . toName) ns
       in xs

  toDecs (Hs.InfixDecl l assoc Nothing ops) =
      toDecs (Hs.InfixDecl l assoc (Just 9) ops)
  toDecs (Hs.InfixDecl _ assoc (Just fixity) ops) =
    map (\op -> InfixD (Fixity fixity dir) (toName op)) ops
   where
    dir = case assoc of
      Hs.AssocNone _ -> InfixN
      Hs.AssocLeft _ -> InfixL
      Hs.AssocRight _ -> InfixR

  toDecs a = [toDec a]

instance ToDecs a => ToDecs [a] where
  toDecs a = concatMap toDecs a

-----------------------------------------------------------------------------
