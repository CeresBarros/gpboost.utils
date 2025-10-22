test_that("test Caching non-trained GPModel", {
  testInit()

  savedSeed <- .Random.seed
  on.exit(assign(".Random.seed", savedSeed, envir = .GlobalEnv), add = TRUE)

  data(GPBoost_data, package = "gpboost")

  ## Caching a non-trained GPModel
  ## this will fail because gpboost save for GPModel won't work for untrained models
  expect_error({
    GPModel(group_data = group_data[,1], likelihood="gaussian") |>
      Cache(omitArgs = "data")
  })

})

test_that("test Caching gpb.Booster", {
  testInit()

  savedSeed <- .Random.seed
  on.exit(assign(".Random.seed", savedSeed, envir = .GlobalEnv), add = TRUE)

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

  ## Caching a gpb.Booster model ---------
  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
  set.seed(123)
  bst <- eval(funCall) |>
    Cache()

  pred <- predict(bst, data = X_test, gp_coords_pred = coords_test,
                  group_data_pred = group_data_test[, 1],predict_cov_mat = TRUE)
  expect_true(is(pred, "list"))
  expect_true(is(pred$response_mean, "numeric"))

  bst2 <- eval(funCall) |>
    Cache()
  pred2 <- predict(bst2, data = X_test, gp_coords_pred = coords_test,
                   group_data_pred = group_data_test[, 1], predict_cov_mat = TRUE)
  expect_equal(pred, pred2)
  expect_false(identical(bst, bst2))  ## they will never be identical
  expect_false(bst$`.__enclos_env__`$private$gp_model_prediction_data_loaded_from_file)
  expect_true(bst2$`.__enclos_env__`$private$gp_model_prediction_data_loaded_from_file)

  tmpfile <- tempfile(fileext = ".rds")
  saveRDS.gpb.Booster(bst, tmpfile)

  bstLoaded <- readRDS.gpb.Booster(tmpfile)
  bst2NoCAttr <- copy(bst2)
  att2rm <- setdiff(names(attributes(bst2NoCAttr)), names(attributes(bstLoaded)))
  lapply(att2rm, function(attname) attr(bst2NoCAttr, attname) <- NULL)

  expect_equal(bst2NoCAttr, bstLoaded)

})

test_that("test Caching gpb.CVBooster", {
  testInit()

  savedSeed <- .Random.seed
  on.exit(assign(".Random.seed", savedSeed, envir = .GlobalEnv), add = TRUE)

  data(GPBoost_data, package = "gpboost")
  dtrain <- gpb.Dataset(X, label = y)
  params <- list(learning_rate = 0.05,
                 max_depth = 6,
                 min_data_in_leaf = 5)
  funCall <- quote(gpb.cv2(params = params,
                          data = dtrain,
                          gp_model = gp_model,
                          nrounds = 100,
                          nfold = 4,
                          eval = "l2",
                          early_stopping_rounds = 5,
                          use_gp_model_for_validation = TRUE))

  ## Caching a gpb.CVBooster model ---------
  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian",
                      free_raw_data = TRUE)
  set.seed(123)
  expect_error({eval(funCall) |>
    Cache()})

  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian",
                      free_raw_data = FALSE)
  set.seed(123)
  cvbst <- eval(funCall) |>
    Cache()

  cvbst.booster <- cvbst$boosters[[1]]$booster
  pred <- predict(cvbst.booster, data = X_test, gp_coords_pred = coords_test,
                  group_data_pred = group_data_test[, 1], predict_cov_mat = TRUE)
  expect_true(is(pred, "list"))
  expect_true(is(pred$response_mean, "numeric"))

  set.seed(123)
  cvbst2 <- eval(funCall) |>
    Cache()
  cvbst2.booster <- cvbst2$boosters[[1]]$booster
  pred2 <- predict(cvbst2.booster, data = X_test, gp_coords_pred = coords_test,
                   group_data_pred = group_data_test[, 1], predict_cov_mat = TRUE)
  expect_equal(pred, pred2)
  expect_false(identical(cvbst, cvbst2))  ## they will never be identical
  expect_false(identical(cvbst.booster, cvbst2.booster))  ## they will never be identical
  expect_false(cvbst.booster$`.__enclos_env__`$private$gp_model_prediction_data_loaded_from_file)
  expect_true(cvbst2.booster$`.__enclos_env__`$private$gp_model_prediction_data_loaded_from_file)

})

test_that("test Caching GPModel", {
  testInit()

  savedSeed <- .Random.seed
  on.exit(assign(".Random.seed", savedSeed, envir = .GlobalEnv), add = TRUE)

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
  expect_true(is(covpars, "numeric"))
  expect_true(identical(names(covpars), c("Error_term", "Group_1")))
  expect_true(is(gp_modelOut$model_to_list(), "list"))

  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
  gp_modelOut2 <- (function(qCall){
    bst <- eval(qCall)
    return(gp_model)
  })(qCall = funCall)|>
    Cache()

  attributes(gp_modelOut2)

  covpars2 <- get_cov_pars(gp_modelOut2)
  expect_equal(covpars, covpars2)

  ## save/load model without Cache
  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
  set.seed(123)
  bst <- eval(funCall)
  tmpfile <-  tempfile(fileext = ".json")
  saveGPModel(gp_model, tmpfile)
  gp_modelLoaded <- loadGPModel(tmpfile)

  covpars3 <- get_cov_pars(gp_modelLoaded)
  expect_equal(covpars, covpars3)

  expect_false(gp_modelOut$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)
  expect_true(gp_modelOut2$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)
  expect_true(gp_modelLoaded$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)

  out <- try(is(gp_modelLoaded$model_to_list(), "list"), silent = TRUE)
  out2 <- try(is(gp_modelOut2$model_to_list(), "list"), silent = TRUE)
  expect_equal(out, out2)   ## both Cache and loadGPModel result in an "incomplete" object.

  gp_modelOut2Noatt <- copy(gp_modelOut2)
  att2rm <- setdiff(names(attributes(gp_modelOut2Noatt)), names(attributes(gp_modelLoaded)))
  lapply(att2rm, function(attname) attr(gp_modelOut2Noatt, attname) <- NULL)

  expect_equal(gp_modelOut2Noatt, gp_modelLoaded)
})

test_that("test Caching list of gpboost models", {
  testInit()

  savedSeed <- .Random.seed
  on.exit(assign(".Random.seed", savedSeed, envir = .GlobalEnv), add = TRUE)

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
  expect_true(is(summary(bst.list$gp_model), "GPModel"))
  expect_true(is(bst.list$gp_model$model_to_list(), "list"))

  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
  bst.list2 <- (function(qCall){
    bst <- eval(qCall)
    return(list(bst = bst, gp_model = gp_model))
  })(qCall = funCall) |>
    Cache()

  covpars2 <- get_cov_pars(bst.list2$gp_model)
  pred2 <- predict(bst.list2$bst, data = X_test, gp_coords_pred = coords_test,
                   group_data_pred = group_data_test[, 1],predict_cov_mat = TRUE)

  expect_true(all.equal(covpars, covpars2))

  ## save/load model without Cache
  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
  set.seed(123)
  bst <- eval(funCall)
  tmpfile <-  tempfile(fileext = ".json")
  saveGPModel(gp_model, tmpfile)
  gp_modelLoaded <- loadGPModel(tmpfile)
  covpars3 <- get_cov_pars(gp_modelLoaded)

  expect_equal(covpars, covpars3)
  expect_true(all.equal(pred, pred2))
  expect_true(is(summary(bst.list2$gp_model), "GPModel"))

  expect_false(bst.list$gp_model$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)
  expect_true(bst.list2$gp_model$`.__enclos_env__`$private$model_has_been_loaded_from_saved_file)

  ## save/load model without Cache
  gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian")
  set.seed(123)
  bst <- eval(funCall)
  tmpfile <-  tempfile(fileext = ".json")
  saveGPModel(gp_model, tmpfile)
  gp_modelLoaded <- loadGPModel(tmpfile)


  bst.list2Noatt <- copy(bst.list2$gp_model)
  att2rm <- setdiff(names(attributes(bst.list2Noatt)), names(attributes(gp_modelLoaded)))
  lapply(att2rm, function(attname) attr(bst.list2Noatt, attname) <- NULL)

  expect_equal(bst.list2Noatt, gp_modelLoaded)


})
