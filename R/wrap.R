#' Deal with class for saving to and loading from Cache or Disk
#'
#' Methods that prepare `GPModel` and `gpb.Booster` objcts for
#' saving to disk (or RAM) via e.g., `saveRDS`, which need wrapping and unwrapping.
#'
#' @param obj Any arbitrary R object.
#' @param ... Arguments passed to methods; default does not use anything in `...`.
#' @inheritParams reproducible::Cache
#' @inheritParams reproducible::loadFromCache
#'
#' @return Returns an object that can be saved to disk e.g., via `saveRDS`.
#'
#' @export
#' @rdname dotWrap
.wrap.GPModel <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                          conn = getOption("reproducible.conn", NULL),
                          verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  ## Cache the model list, so that it can be saved as .rds using "normal" Cache methods
  obj <- obj$model_to_list(include_response_data = TRUE)
  class(obj) <- c("GPModel", class(obj))   ## to call .unwrap.GPModel from .unwrap.default (otherwise assumed to be a "Path")
  return(obj)
}

#' @export
#' @rdname dotWrap
.wrap.gpb.Booster <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                              conn = getOption("reproducible.conn", NULL),
                              verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  browser()
  if (is.na(obj$raw)) {
    obj$raw <- obj$save_model_to_string(NULL)
  }
  obj
}


#' @export
#' @rdname dotWrap
.unwrap.GPModel <- function(obj, cachePath, cacheId,
                            drv = getDrv(getOption("reproducible.drv", NULL)),
                            conn = getOption("reproducible.conn", NULL), ...) {
  ## go from JSON to model list:
  # model_list <- RJSONIO::fromJSON(content = obj)

  ## then from model list to GPModel
  obj <- gpboost:::gpb.GPModel$new(model_list = obj)
  obj
}

#' @export
#' @rdname dotWrap
.unwrap.gpb.Booster <- function(obj, cachePath, cacheId,
                                drv = getDrv(getOption("reproducible.drv", NULL)),
                                conn = getOption("reproducible.conn", NULL), ...) {
  browser()
  if (!is.na(obj$raw)) {
    obj2 <- gpboost::gpb.load(model_str = obj$raw)
    obj2$best_iter <- obj$best_iter
    obj2$record_evals <- obj$record_evals
    obj2$params <- obj$params
    obj2$raw <- NA ## as in gpboost::saveRDS.gpb.Booster
    return(obj2)
  }
  return(obj)
}
