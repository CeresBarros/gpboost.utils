## non-exported imports from other packages -------------------------------------
getDrv <- getFromNamespace("getDrv", "reproducible")

## R6 objects
gpb.GPModel <- getFromNamespace("gpb.GPModel", "gpboost")
Booster <- getFromNamespace("Booster", "gpboost")
CVBooster <- getFromNamespace("CVBooster", "gpboost")
CB_ENV <- getFromNamespace("CB_ENV", "gpboost")
Predictor <- getFromNamespace("Predictor", "gpboost")

## functions
add.cb <- getFromNamespace("add.cb", "gpboost")
categorize.callbacks <- getFromNamespace("categorize.callbacks", "gpboost")
cb.print.evaluation <- getFromNamespace("cb.print.evaluation", "gpboost")
cb.record.evaluation <- getFromNamespace("cb.record.evaluation", "gpboost")
cb.early.stop <- getFromNamespace("cb.early.stop", "gpboost")
gpb.check.eval <- getFromNamespace("gpb.check.eval", "gpboost")
gpb.check_interaction_constraints <- getFromNamespace("gpb.check_interaction_constraints", "gpboost")
gpb.check.obj <- getFromNamespace("gpb.check.obj", "gpboost")
gpb.check.wrapper_param <- getFromNamespace("gpb.check.wrapper_param", "gpboost")
gpb.is.Booster <- getFromNamespace("gpb.is.Booster", "gpboost")
gpb.is.Dataset <- getFromNamespace("gpb.is.Dataset", "gpboost")
gpb.merge.cv.result <- getFromNamespace("gpb.merge.cv.result", "gpboost")
generate.cv.folds <- getFromNamespace("generate.cv.folds", "gpboost")
.PARAMETER_ALIASES <- getFromNamespace(".PARAMETER_ALIASES", "gpboost")
.EVAL_KEY <- getFromNamespace(".EVAL_KEY", "gpboost")

## re-exported functions --------------------------------------------------------
## (example not used here)
## @importFrom reproducible paddedFloatToChar
## @export
## reproducible::paddedFloatToCha
