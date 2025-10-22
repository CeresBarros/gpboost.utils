#' Deal with class for saving to and loading from Cache or Disk
#'
#' See [`reproducible::.wrap()`]
#'
#' @name .wrap
#' @rdname dotWrap
#' @export
#' @inheritParams reproducible::.wrap
.wrap <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                  conn = getOption("reproducible.conn", NULL),
                  verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  UseMethod(".wrap")
}

#' @rdname dotWrap
#' @export
#' @method .wrap GPModel
.wrap.GPModel <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                          conn = getOption("reproducible.conn", NULL),
                          verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  ## Cache the model list, so that it can be saved as .rds using "normal" Cache methods
  obj <- obj$model_to_list(include_response_data = TRUE)
  class(obj) <- c("GPModel", class(obj))   ## to call .unwrap.GPModel from .unwrap.default (otherwise assumed to be a "Path")
  return(obj)
}

#' @rdname dotWrap
#' @export
#' @method .wrap gpb.Booster
.wrap.gpb.Booster <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                              conn = getOption("reproducible.conn", NULL),
                              verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  if (is.na(obj$raw)) {
    obj$raw <- obj$save_model_to_string(NULL)
  }
  return(obj)
}

#' @rdname dotWrap
#' @export
#' @inheritParams reproducible::.unwrap
.unwrap <- function(obj, cachePath, cacheId,
                    drv = getDrv(getOption("reproducible.drv", NULL)),
                    conn = getOption("reproducible.conn", NULL), ...) {
  UseMethod(".unwrap")
}

#' @rdname dotWrap
#' @export
#' @method .unwrap GPModel
.unwrap.GPModel <- function(obj, cachePath, cacheId,
                            drv = getDrv(getOption("reproducible.drv", NULL)),
                            conn = getOption("reproducible.conn", NULL), ...) {
  ## go from JSON to model list:
  # model_list <- RJSONIO::fromJSON(content = obj)

  ## then from model list to GPModel
  obj <- gpb.GPModel$new(model_list = obj)
  return(obj)
}

#' @rdname dotWrap
#' @importFrom gpboost gpb.load
#' @export
#' @method .unwrap gpb.Booster
.unwrap.gpb.Booster <- function(obj, cachePath, cacheId,
                                drv = getDrv(getOption("reproducible.drv", NULL)),
                                conn = getOption("reproducible.conn", NULL), ...) {
  if (!is.na(obj$raw)) {
    obj2 <- gpb.load(model_str = obj$raw)
    obj2$best_iter <- obj$best_iter
    obj2$record_evals <- obj$record_evals
    obj2$params <- obj$params
    obj2$raw <- NA ## as in gpboost::saveRDS.gpb.Booster
    return(obj2)
  }
  return(obj)
}


register_all_s3_methods <- function() {
  s3_register("reproducible::.wrap", "gpb.Booster")
  s3_register("reproducible::.wrap", "GPModel")
  s3_register("reproducible::.unwrap", "gpb.Booster")
  s3_register("reproducible::.unwrap", "GPModel")
}
