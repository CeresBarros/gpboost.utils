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
#' @method .wrap gpb.CVBooster
.wrap.gpb.CVBooster <- function(obj, cachePath, preDigest,  drv = getDrv(getOption("reproducible.drv", NULL)),
                              conn = getOption("reproducible.conn", NULL),
                              verbose = getOption("reproducible.verbose"), outputObjects  = NULL, ...) {
  ## make "linked copy"
  obj.booster <- obj$boosters[[1]]$booster
  if (is.na(obj.booster$raw)) {
    ## make "linked copy"
    gp_model2 <- obj.booster$`.__enclos_env__`$private$gp_model

    if (isTRUE(gp_model2$`.__enclos_env__`$private$free_raw_data)) {
      stop(paste0("To Cache 'gpb.CVBooster' class, set 'free_raw_data = FALSE' when you create the 'GPModel'\n",
                  "  and use gpboost.utils::gpb.cv2"))
    }

    obj.booster$raw <- obj.booster$save_model_to_string(NULL)   ## this
    # gp_model2 <- .wrap(gp_model2) ## maybe not needed
    # obj.booster$`.__enclos_env__`$private$gp_model <- gp_model2
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

#' @rdname dotWrap
#' @importFrom gpboost gpb.load
#' @export
#' @method .unwrap gpb.CVBooster
.unwrap.gpb.CVBooster <- function(obj, cachePath, cacheId,
                                drv = getDrv(getOption("reproducible.drv", NULL)),
                                conn = getOption("reproducible.conn", NULL), ...) {

  obj.booster <- obj$boosters[[1]]$booster

  if (!is.na(obj.booster$raw)) {
    obj.booster2 <- gpb.load(model_str = obj.booster$raw)
    obj.booster2$best_iter <- obj.booster$best_iter
    obj.booster2$record_evals <- obj.booster$record_evals
    obj.booster2$params <- obj.booster$params
    obj.booster2$raw <- NA ## as in gpboost::saveRDS.gpb.Booster

    obj$boosters[[1]]$booster <- obj.booster2
  }

  return(obj)
}


register_all_s3_methods <- function() {
  s3_register("reproducible::.wrap", "gpb.Booster")
  s3_register("reproducible::.wrap", "gpb.CVBooster")
  s3_register("reproducible::.wrap", "GPModel")
  s3_register("reproducible::.unwrap", "gpb.Booster")
  s3_register("reproducible::.unwrap", "gpb.CVBooster")
  s3_register("reproducible::.unwrap", "GPModel")
}
