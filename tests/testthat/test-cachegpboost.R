library(gpboost, lib.loc = "E:/LandscapesInMotion/packages/x86_64-w64-mingw32/4.5/")
library(assertthat, lib.loc = "E:/LandscapesInMotion/packages/x86_64-w64-mingw32/4.5/")
Require::Require("CeresBarros/reproducible@CopyCacheAtts4gpboost")
options(reproducible.cachePath = file.path(tempdir(), "cache-test"))

## doesn't work:
# devtools::install_local()
# library(gpboost.utils)

devtools::load_all()
# source("C:/Users/cbarros/GitHub/gpboost.utils/R/wrap.R")

data(GPBoost_data, package = "gpboost")
dtrain <- gpb.Dataset(X, label = y)
funCall <- quote(gpb.train(data = dtrain,
                           gp_model = gp_model,
                           nrounds = 16,
                           learning_rate = 0.05,
                           max_depth = 6,
                           min_data_in_leaf = 5,
                           verbose = 0,
                           train_gp_model_cov_pars = FALSE))

## Caching a non-trained GPModel
## this will fail because gpboost save for GPModel won't work for untrained models
gp_model <- try({
  GPModel(group_data = group_data[,1], likelihood="gaussian") |>
    Cache(omitArgs = "data")
}, silent = TRUE)

is.error(gp_model)

## Caching a gpb.Booster model ---------
gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
set.seed(123)
bst <- eval(funCall) |>
  Cache()

pred <- predict(bst, data = X_test, gp_coords_pred = coords_test,
                group_data_pred = group_data_test[, 1],predict_cov_mat = TRUE)
assert_that(is(pred, "list"),
                        is(pred$response_mean, "numeric"))

bst2 <- eval(funCall) |>
  Cache()
pred2 <- predict(bst2, data = X_test, gp_coords_pred = coords_test,
                 group_data_pred = group_data_test[, 1], predict_cov_mat = TRUE)
are_equal(pred, pred2)
assert_that(!identical(bst, bst2))  ## they will never be identical
assert_that(!bst$`.__enclos_env__`$private$gp_model_prediction_data_loaded_from_file)
assert_that(bst2$`.__enclos_env__`$private$gp_model_prediction_data_loaded_from_file)

tmpfile <- tempfile(fileext = ".rds")
saveRDS.gpb.Booster(bst, tmpfile)

bstLoaded <- readRDS.gpb.Booster(tmpfile)
bst2NoCAttr <- copy(bst2)
att2rm <- setdiff(names(attributes(bst2NoCAttr)), names(attributes(bstLoaded)))
lapply(att2rm, function(attname) attr(bst2NoCAttr, attname) <- NULL)

are_equal(bst2NoCAttr, bstLoaded)

## Caching GPModel ---------
gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
set.seed(123)
gp_modelOut <- (function(qCall){
  bst <- eval(qCall)
  return(gp_model)
})(qCall = funCall)|>
  Cache()
attributes(gp_modelOut)

covpars <- get_cov_pars(gp_modelOut)
assert_that(is(covpars, "numeric"),
                        identical(names(covpars), c("Error_term", "Group_1")))
assert_that(is(gp_modelOut$model_to_list(), "list"))

gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
gp_modelOut2 <- (function(qCall){
  bst <- eval(qCall)
  return(gp_model)
})(qCall = funCall)|>
  Cache()

attributes(gp_modelOut2)

covpars2 <- get_cov_pars(gp_modelOut2)
are_equal(covpars, covpars2)

gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
set.seed(123)
bst <- eval(funCall)

tmpfile <-  tempfile(fileext = ".json")
saveGPModel(gp_model, tmpfile)
gp_modelLoaded <- loadGPModel(tmpfile)

covpars3 <- get_cov_pars(gp_modelLoaded)
are_equal(covpars, covpars3)

assert_that(!gp_modelOut$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)
assert_that(gp_modelOut2$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)
assert_that(gp_modelLoaded$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)

out <- try(is(gp_modelLoaded$model_to_list(), "list"), silent = TRUE)
out2 <- try(is(gp_modelOut2$model_to_list(), "list"), silent = TRUE)
are_equal(out, out2)   ## both Cache and loadGPModel result in an "incomplete" object.

gp_modelOut2Noatt <- copy(gp_modelOut2)
att2rm <- setdiff(names(attributes(gp_modelOut2Noatt)), names(attributes(gp_modelLoaded)))
lapply(att2rm, function(attname) attr(gp_modelOut2Noatt, attname) <- NULL)

are_equal(gp_modelOut2Noatt, gp_modelLoaded)

## Cache a list of models ---------
clearCache(ask = FALSE)
gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
set.seed(123)
bst.list <- (function(qCall){
  bst <- eval(qCall)
  return(list(bst = bst, gp_model = gp_model))
})(qCall = funCall) |>
  Cache()

covpars <- get_cov_pars(bst.list$gp_model)
pred <- predict(bst.list$bst, data = X_test, gp_coords_pred = coords_test,
                group_data_pred = group_data_test[, 1],predict_cov_mat = TRUE)
assert_that(is(summary(bst.list$gp_model), "GPModel"))
assert_that(is(bst.list$gp_model$model_to_list(), "list"))

gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
bst.list2 <- (function(qCall){
  bst <- eval(qCall)
  return(list(bst = bst, gp_model = gp_model))
})(qCall = funCall) |>
  Cache()

covpars2 <- get_cov_pars(bst.list2$gp_model)
pred2 <- predict(bst.list2$bst, data = X_test, gp_coords_pred = coords_test,
                 group_data_pred = group_data_test[, 1],predict_cov_mat = TRUE)

assert_that(all.equal(covpars, covpars2))
are_equal(covpars, covpars3)
assert_that(all.equal(pred, pred2))
assert_that(is(summary(bst.list2$gp_model), "GPModel"))

assert_that(!bst.list$gp_model$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)
assert_that(bst.list2$gp_model$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)

bst.list2Noatt <- copy(bst.list2$gp_model)
att2rm <- setdiff(names(attributes(bst.list2Noatt)), names(attributes(gp_modelLoaded)))
lapply(att2rm, function(attname) attr(bst.list2Noatt, attname) <- NULL)

are_equal(bst.list2Noatt, gp_modelLoaded)

