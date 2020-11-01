module Core
where

--------------------------------------------------------------------------------
import CoreSyn
import Control.Monad ((<=<))
import HscTypes (mg_binds)
import GHC
import GHC.Paths (libdir)
import qualified Text.PrettyPrint.HughesPJClass as P
import Var
import Literal
import Outputable
import DynFlags
--------------------------------------------------------------------------------

compileToCore :: String -> IO [CoreBind]
compileToCore modName = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget (modName ++ ".hs") Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds
--------------------------------------------------------------------------------

toStr v = showSDoc unsafeGlobalDynFlags $ ppr v
--------------------------------------------------------------------------------

bName (NonRec v e) = toStr v
--------------------------------------------------------------------------------

instance P.Pretty AltCon where
  pPrint (DataAlt dataCon) = P.text "DataAlt " P.<> P.pPrint dataCon
  pPrint (LitAlt _) = P.text "LitAlt " P.<> P.text "?"
  pPrint DEFAULT = P.text "DEFAULT"
--------------------------------------------------------------------------------

instance P.Pretty Var.Var where
  pPrint v = P.text . (showSDoc unsafeGlobalDynFlags) $ ppr v
--------------------------------------------------------------------------------

instance P.Pretty Literal.Literal where
  pPrint l = P.text . (showSDoc unsafeGlobalDynFlags) $ ppr l
--------------------------------------------------------------------------------

instance P.Pretty DataCon where
  pPrint dc = P.text . (showSDoc unsafeGlobalDynFlags) $ ppr dc
--------------------------------------------------------------------------------

instance P.Pretty a => P.Pretty (Expr a) where
  pPrint (Var v) = P.text "Var " P.<> P.pPrint v
  pPrint (Lit l) = P.text "Lit " P.<> P.pPrint l
  pPrint (App e a) = P.vcat [ P.text "@"
                          , P.nest 2 (P.pPrint e)
                          , P.nest 2 (P.pPrint a)
                          ]
  pPrint (Lam v e) = P.vcat [ P.text "Lam " P.<> (P.pPrint v)
                          , P.nest 2 (P.pPrint e)
                          ]
  pPrint (Let b e) = P.vcat [ P.text "Let"
                          , P.nest 2 (P.pPrint b)
                          , P.nest 2 (P.pPrint e)
                          ]
  pPrint (Case e _ _ alts) = P.vcat [ P.text "Case " P.<> (P.pPrint e)
                                  , P.nest 2 (P.text "_")
                                  , P.nest 2 (P.text "_")
                                  , P.nest 2 (P.pPrint alts)
                                  ]
  pPrint (Type t) = P.text "Type " P.<> (P.pPrint t)
  pPrint _ = P.text "?"
--------------------------------------------------------------------------------

instance P.Pretty Type where
  pPrint ty = P.text . (showSDoc unsafeGlobalDynFlags) $ ppr ty
--------------------------------------------------------------------------------

instance P.Pretty a => P.Pretty (Bind a) where
  pPrint (NonRec v e) = P.vcat [ P.text "NonRec " P.<> (P.pPrint v)
                              , P.nest 2 (P.pPrint e)
                              ]
  pPrint (Rec bindings) = P.vcat [ P.text "Rec"
                                , P.pPrint bindings
                                ]
--------------------------------------------------------------------------------
