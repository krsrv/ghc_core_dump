module ResolvedCore
where

--------------------------------------------------------------------------------
import Data.String.Utils
import CoreSyn
import DynFlags
import Outputable
import DataCon
import TyCoRep
import Id
import qualified Text.PrettyPrint.HughesPJClass as P
--------------------------------------------------------------------------------

data RBind = RNonRec String RExpr | RRec [(String, RExpr)]
--------------------------------------------------------------------------------

data RExpr = RVar String
            | RLit String
            | RApp RExpr RExpr
            | RLam (String, String) RExpr
            | RLet RBind RExpr
            | RCase RExpr [RAlt]
            | RType String
--------------------------------------------------------------------------------

type RAlt = (RAltCon, [String], RExpr)
--------------------------------------------------------------------------------

data RAltCon = RDataAlt String
              | RLitAlt String
              | RDEFAULT
--------------------------------------------------------------------------------

instance P.Pretty RAltCon where
  pPrint (RDataAlt dataCon) = P.text "RDataAlt" P.<+> P.text dataCon
  pPrint (RLitAlt lit) = P.text "RLitAlt" P.<+> P.text lit
  pPrint RDEFAULT = P.text "RDEFAULT"
--------------------------------------------------------------------------------

instance P.Pretty RExpr where
  pPrint (RVar v) = P.text "RVar" P.<+> P.text v
  pPrint (RLit l) = P.text "RLit" P.<+> P.text l
  pPrint (RApp e1 e2) = P.vcat [ P.text "RApp"
                                , P.nest 2 $ P.pPrint e1
                                , P.nest 2 $ P.pPrint e2
                                ]
  pPrint (RLam (v, t) e) = P.vcat [ P.text "RLam" P.<+> P.text v P.<+> P.text "::" P.<+> P.text t
                                , P.nest 2 $ P.pPrint e
                                ]
  pPrint (RLet b e) = P.vcat [ P.text "RLet"
                            , P.nest 2 $ P.pPrint b
                            , P.nest 2 $ P.pPrint e
                            ]
  pPrint (RCase e alts) = P.vcat $ (P.text "RCase"):(P.nest 2 (P.pPrint e)):(map ((P.nest 2) . P.pPrint) alts)
  pPrint (RType t) = P.text "RType" P.<+> P.text t
--------------------------------------------------------------------------------

instance P.Pretty RBind where
  pPrint (RNonRec v e) = P.vcat [ P.text "RNonRec"
                              , P.nest 2 $ P.text v
                              , P.nest 2 $ P.pPrint e
                              ]
  pPrint (RRec bs) = P.vcat $ (P.text "RRec"):(map ((P.nest 2) . P.pPrint) bs)
--------------------------------------------------------------------------------


toRBind (NonRec v e) = RNonRec (toStr v) $ toRExpr e
toRBind (Rec bs) = RRec $ map (\(n, e) -> (toStr n, toRExpr e)) bs
--------------------------------------------------------------------------------

toRExpr (Var v) = RVar $ toStr v
toRExpr (Lit l) = RLit $ toStr l
toRExpr (App e1 e2) = RApp (toRExpr e1) $ toRExpr e2
toRExpr (Lam v e) = RLam (toStr v, varType v) $ toRExpr e
toRExpr (Let b e) = RLet (toRBind b) $ toRExpr e
toRExpr (Case e _ _ alts) = RCase (toRExpr e) $ map toRAlt alts
toRExpr (Type t) = RType $ toStr t
--------------------------------------------------------------------------------

toRAlt (con, args, e) = (toRAltCon con, map toStr args, toRExpr e)
--------------------------------------------------------------------------------

toRAltCon (DataAlt d) = RDataAlt $ toStr d
toRAltCon (LitAlt l) = RLitAlt $ toStr l
toRAltCon _ = RDEFAULT
--------------------------------------------------------------------------------

toStr v = showSDoc unsafeGlobalDynFlags $ ppr v
--------------------------------------------------------------------------------

varType = (showSDoc unsafeGlobalDynFlags) . pprType . idType
--------------------------------------------------------------------------------

resolveExpr p@(RVar v) typ rType = p
resolveExpr p@(RLit l) typ rType = p
resolveExpr (RApp e (RType t)) typ rType
  | t == typ = resolveExpr (RApp e (RType rType)) typ rType
  | otherwise = RApp (resolveExpr e typ rType) $ RType t
resolveExpr (RApp e p@(RVar v)) typ rType
  | isDictionary v = resolvedE
  | otherwise = RApp resolvedE $ resolveExpr p typ rType
  where resolvedE = resolveExpr e typ rType
resolveExpr (RApp e1 e2) typ rType = RApp (resolveExpr e1 typ rType) $ resolveExpr e2 typ rType
resolveExpr (RLam (v, t) e) typ rType
  | v == typ = resolvedE
  | isDictionary v = resolvedE
  | otherwise = RLam (v, t) resolvedE
  where resolvedE = resolveExpr e typ rType
resolveExpr (RLet b e) typ rType = RLet (resolveBind b typ rType) $ resolveExpr e typ rType
resolveExpr (RCase e alts) typ rType = RCase (resolveExpr e typ rType) $ resolveAlts alts typ rType
resolveExpr p@(RType t) typ rType
  | t == typ = RType rType
  | otherwise = p
--------------------------------------------------------------------------------

resolveAlts alts typ rType = map (\(a, b, e) -> (a, b, resolveExpr e typ rType)) alts
--------------------------------------------------------------------------------

resolveBind (RNonRec v e) typ rType = RNonRec v $ resolveExpr e typ rType
resolveBind (RRec bs) typ rType = RRec $ map (\(name, expr) -> (name, resolveExpr expr typ rType)) bs
--------------------------------------------------------------------------------

isDictionary = startswith "$d"
--------------------------------------------------------------------------------