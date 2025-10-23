#' @name gpb.cv2
#' @title Variation of `gpboost::gpb.cv` for reproducible::Cache.
#' @description This version of [`gpboost::gpb.cv()`] respects `free_raw_data`
#'   value in passed to [`gpboost::GPModel()`]
#' @inheritParams gpboost::gpb.cv
#' @return a trained model `gpb.CVBooster`.
#'
#' @examples
#' # See https://github.com/fabsig/GPBoost/tree/master/R-package for more example
#' library(gpboost)
#' library(reproducible)
#' data(GPBoost_data, package = "gpboost")
#'
#' # Create random effects model and dataset
#' # free_raw_data must be FALSE to enable Caching
#' gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian", free_raw_data = FALSE)
#' dtrain <- gpb.Dataset(X, label = y)
#' params <- list(learning_rate = 0.05,
#'                max_depth = 6,
#'                min_data_in_leaf = 5)
#' # Run CV
#' set.seed(123)
#' cvbst <- gpb.cv2(params = params,
#'                  data = dtrain,
#'                  gp_model = gp_model,
#'                  nrounds = 100,
#'                  nfold = 4,
#'                  eval = "l2",
#'                  early_stopping_rounds = 5,
#'                  use_gp_model_for_validation = TRUE) |>
#'   Cache(omitArgs = c("gp_model", "data"))   ## omit gp_model and data -- they are changed in place
#' ## retrieve from Cache
#' set.seed(123)
#' cvbst <- gpb.cv2(params = params,
#'                  data = dtrain,
#'                  gp_model = gp_model,
#'                  nrounds = 100,
#'                  nfold = 4,
#'                  eval = "l2",
#'                  early_stopping_rounds = 5,
#'                  use_gp_model_for_validation = TRUE) |>
#'   Cache(omitArgs = c("gp_model", "data"))
#'
#' ## to cache both the GPModel and gpb.CVBooster wrap the call in a function and
#' ## export them in a list()
#' gp_model <- GPModel(group_data = group_data[,1], likelihood="gaussian", free_raw_data = FALSE)
#' set.seed(123)
#' \dontrun{
#' ## the following will fail, because gpb.cv2 does not train gp_model and
#' ## untrained 'GPModel' objects cannot be saved
#' cvbst.ls <- (function(){
#'   out <- gpb.cv2(params = params,
#'                  data = dtrain,
#'                  gp_model = gp_model,
#'                  nrounds = 100,
#'                  nfold = 4,
#'                  eval = "l2",
#'                  early_stopping_rounds = 5,
#'                  use_gp_model_for_validation = TRUE)
#'   list(cvbst = out, gp_model = gp_model)
#' })()|>
#'   Cache()
#' }
#' @importFrom data.table data.table setorderv
#' @importFrom methods is
#' @import gpboost
#' @export
gpb.cv2 <- function(params = list()
                    , data
                    , gp_model = NULL
                    , nrounds = 1000L
                    , early_stopping_rounds = NULL
                    , folds = NULL
                    , nfold = 5L
                    , metric = NULL
                    , verbose = 1L
                    , use_gp_model_for_validation = TRUE
                    , fit_GP_cov_pars_OOS = FALSE
                    , train_gp_model_cov_pars = TRUE
                    , label = NULL
                    , weight = NULL
                    , obj = NULL
                    , eval = NULL
                    , record = TRUE
                    , eval_freq = 1L
                    , showsd = FALSE
                    , stratified = TRUE
                    , init_model = NULL
                    , colnames = NULL
                    , categorical_feature = NULL
                    , callbacks = list()
                    , reset_data = FALSE
                    , delete_boosters_folds = FALSE
                    , ...
) {

  if (nrounds <= 0L) {
    stop("nrounds should be greater than zero")
  }

  # If 'data' is not an gpb.Dataset, try to construct one using 'label'
  if (!gpb.is.Dataset(x = data)) {
    if (is.null(label)) {
      stop("'label' must be provided for gpb.cv if 'data' is not an 'gpb.Dataset'")
    }
    data <- gpb.Dataset(data = data, label = label)
  }

  if (data$.__enclos_env__$private$free_raw_data) {
    warning("For true out-of-sample (cross-) validation, it is recommended to set free_raw_data = False when constructing the Dataset")
  }

  # Setup temporary variables
  if (!is.null(metric)) {
    params <- append(params, list(metric = metric))
  }
  params <- append(params, list(...))
  params$verbose <- verbose
  params <- gpb.check.obj(params = params, obj = obj)
  params <- gpb.check.eval(params = params, eval = eval)
  fobj <- NULL
  eval_functions <- list(NULL)
  has_custom_eval_functions <- FALSE

  params$use_gp_model_for_validation <- use_gp_model_for_validation
  params$train_gp_model_cov_pars <- train_gp_model_cov_pars

  # set some parameters, resolving the way they were passed in with other parameters
  # in `params`.
  # this ensures that the model stored with Booster$save() correctly represents
  # what was passed in
  params <- gpb.check.wrapper_param(
    main_param_name = "num_iterations"
    , params = params
    , alternative_kwarg_value = nrounds
  )
  params <- gpb.check.wrapper_param(
    main_param_name = "early_stopping_round"
    , params = params
    , alternative_kwarg_value = early_stopping_rounds
  )
  early_stopping_rounds <- params[["early_stopping_round"]]

  # Check for objective (function or not)
  if (is.function(params$objective)) {
    fobj <- params$objective
    params$objective <- "NONE"
  }

  # If eval is a single function, store it as a 1-element list
  # (for backwards compatibility). If it is a list of functions, store
  # all of them. This makes it possible to pass any mix of strings like "auc"
  # and custom functions to eval
  if (is.function(eval)) {
    eval_functions <- list(eval)
    has_custom_eval_functions <- TRUE
  }
  if (methods::is(eval, "list")) {
    eval_functions <- Filter(
      f = is.function
      , x = eval
    )
    if (length(eval_functions)>0) {
      has_custom_eval_functions <- TRUE
    }
  }

  # Init predictor to empty
  predictor <- NULL

  # Check for boosting from a trained model
  if (is.character(init_model)) {
    predictor <- Predictor$new(modelfile = init_model)
  } else if (gpb.is.Booster(x = init_model)) {
    predictor <- init_model$to_predictor()
  }

  # Set the iteration to start from / end to (and check for boosting from a trained model, again)
  begin_iteration <- 1L
  if (!is.null(predictor)) {
    begin_iteration <- predictor$current_iter() + 1L
  }
  end_iteration <- begin_iteration + params[["num_iterations"]] - 1L

  # Construct datasets, if needed
  data$update_params(params = params)
  if (data$.__enclos_env__$private$free_raw_data) {
    data$construct()
  } else if (is.character(data$.__enclos_env__$private$raw_data)) {
    data$construct()
  }

  # Check interaction constraints
  cnames <- NULL
  if (!is.null(colnames)) {
    cnames <- colnames
  } else if (!is.null(data$get_colnames())) {
    cnames <- data$get_colnames()
  }
  params[["interaction_constraints"]] <- gpb.check_interaction_constraints(params = params, column_names = cnames)

  if (!is.null(gp_model)) {
    if (has_custom_eval_functions & use_gp_model_for_validation) {
      # Note: if this option should be added, it can be done similarly as in gpb.cv using booster$add_valid(..., valid_set_gp = valid_set_gp, ...)
      stop("use_gp_model_for_validation=TRUE is currently not supported for custom validation functions. If you need this feature, contact the developer of this package or open a GitHub issue.")
    }
    if (gp_model$get_num_data() != data$dim()[1]) {
      stop("Different number of samples in data and gp_model")
    }
  }

  # Check for weights
  if (!is.null(weight)) {
    data$setinfo(name = "weight", info = weight)
  }

  # Update parameters with parsed parameters
  data$update_params(params = params)

  # Create the predictor set
  data$.__enclos_env__$private$set_predictor(predictor = predictor)

  # Write column names
  if (!is.null(colnames)) {
    data$set_colnames(colnames = colnames)
  }

  # Write categorical features
  if (!is.null(categorical_feature)) {
    data$set_categorical_feature(categorical_feature = categorical_feature)
  }

  # Check for folds
  if (!is.null(folds)) {

    # Check for list of folds
    if (!identical(class(folds), "list")) {
      stop(sQuote("folds"), " must be a list with vectors of indices for each CV-fold")
    }

    # Set number of folds
    nfold <- length(folds)

  } else {

    # Check fold value
    if (nfold <= 1L) {
      stop(sQuote("nfold"), " must be > 1")
    }

    # Create folds
    if (data$.__enclos_env__$private$free_raw_data){
      folds <- generate.cv.folds(
        nfold = nfold
        , nrows = nrow(data)
        , stratified = stratified
        , label = getinfo(dataset = data, name = "label")
        , group = getinfo(dataset = data, name = "group")
        , params = params
      )
    } else {
      folds <- generate.cv.folds(
        nfold = nfold
        , nrows = nrow(data)
        , stratified = stratified
        , label = data$.__enclos_env__$private$info$label
        , group = data$.__enclos_env__$private$info$group
        , params = params
      )
    }

  }

  # Add printing log callback
  if (verbose > 0L && eval_freq > 0L) {
    callbacks <- add.cb(cb_list = callbacks, cb = cb.print.evaluation(period = eval_freq))
  }

  # Add evaluation log callback
  if (record) {
    callbacks <- add.cb(cb_list = callbacks, cb = cb.record.evaluation())
  }

  # Did user pass parameters that indicate they want to use early stopping?
  using_early_stopping <- !is.null(early_stopping_rounds) && early_stopping_rounds > 0L

  boosting_param_names <- .PARAMETER_ALIASES()[["boosting"]]
  using_dart <- any(
    sapply(
      X = boosting_param_names
      , FUN = function(param) {
        identical(params[[param]], "dart")
      }
    )
  )

  # Cannot use early stopping with 'dart' boosting
  if (using_dart) {
    warning("Early stopping is not available in 'dart' mode.")
    using_early_stopping <- FALSE

    # Remove the cb.early.stop() function if it was passed in to callbacks
    callbacks <- Filter(
      f = function(cb_func) {
        !identical(attr(cb_func, "name"), "cb.early.stop")
      }
      , x = callbacks
    )
  }

  # If user supplied early_stopping_rounds, add the early stopping callback
  if (using_early_stopping) {
    callbacks <- add.cb(
      cb_list = callbacks
      , cb = cb.early.stop(
        stopping_rounds = early_stopping_rounds
        , first_metric_only = isTRUE(params[["first_metric_only"]])
        , verbose = verbose
      )
    )
  }

  cb <- categorize.callbacks(cb_list = callbacks)

  # Construct booster for each fold. The data.table() code below is used to
  # guarantee that indices are sorted while keeping init_score and weight together
  # with the correct indices. Note that it takes advantage of the fact that
  # someDT$some_column returns NULL is 'some_column' does not exist in the data.table
  bst_folds <- lapply(
    X = seq_along(folds)
    , FUN = function(k) {

      # For learning-to-rank, each fold is a named list with two elements:
      #   * `fold` = an integer vector of row indices
      #   * `group` = an integer vector describing which groups are in the fold
      # For classification or regression tasks, it will just be an integer
      # vector of row indices
      folds_have_group <- "group" %in% names(folds[[k]])
      if (folds_have_group) {
        test_indices <- folds[[k]]$fold
        test_group_indices <- folds[[k]]$group
        if (data$.__enclos_env__$private$free_raw_data){
          test_groups <- getinfo(dataset = data, name = "group")[test_group_indices]
          train_groups <- getinfo(dataset = data, name = "group")[-test_group_indices]
        }else{
          test_groups <- data$.__enclos_env__$private$info$group[test_group_indices]
          train_groups <- data$.__enclos_env__$private$info$group[-test_group_indices]
        }
      } else {
        test_indices <- folds[[k]]
      }
      train_indices <- seq_len(nrow(data))[-test_indices]

      # set up test and train Datasets
      if (data$.__enclos_env__$private$free_raw_data){# free_raw_data

        # set up indices for train and test data
        test_indexDT <- data.table::data.table(
          indices = test_indices
          , weight = getinfo(dataset = data, name = "weight")[test_indices]
          , init_score = getinfo(dataset = data, name = "init_score")[test_indices]
        )
        data.table::setorderv(x = test_indexDT, cols = "indices", order = 1L)
        train_indexDT <- data.table::data.table(
          indices = train_indices
          , weight = getinfo(dataset = data, name = "weight")[train_indices]
          , init_score = getinfo(dataset = data, name = "init_score")[train_indices]
        )
        data.table::setorderv(x = train_indexDT, cols = "indices", order = 1L)

        dtest <- slice(data, test_indexDT$indices)
        setinfo(dataset = dtest, name = "weight", info = test_indexDT$weight)
        setinfo(dataset = dtest, name = "init_score", info = test_indexDT$init_score)

        dtrain <- slice(data, train_indexDT$indices)
        setinfo(dataset = dtrain, name = "weight", info = train_indexDT$weight)
        setinfo(dataset = dtrain, name = "init_score", info = train_indexDT$init_score)

      }
      else {# not free_raw_data

        # set up indices for train and test data
        test_indexDT <- data.table::data.table(indices = test_indices)
        data.table::setorderv(x = test_indexDT, cols = "indices", order = 1L)
        train_indexDT <- data.table::data.table(indices = train_indices)
        data.table::setorderv(x = train_indexDT, cols = "indices", order = 1L)

        weight_train = NULL
        if (!is.null(data$.__enclos_env__$private$info$weight)) {
          weight_train = data$.__enclos_env__$private$info$weight[train_indexDT$indices]
        }
        init_score_train = NULL
        if (!is.null(data$.__enclos_env__$private$info$init_score)) {
          init_score_train = data$.__enclos_env__$private$info$init_score[train_indexDT$indices]
        }
        dtrain <- gpb.Dataset(data = as.matrix(data$.__enclos_env__$private$raw_data[train_indexDT$indices,]),
                              label = data$.__enclos_env__$private$info$label[train_indexDT$indices],
                              weight = weight_train,
                              init_score = init_score_train,
                              colnames = data$.__enclos_env__$private$colnames,
                              categorical_feature = data$.__enclos_env__$private$categorical_feature,
                              params = data$.__enclos_env__$private$params,
                              free_raw_data = data$.__enclos_env__$private$free_raw_data)

        weight_test = NULL
        if (!is.null(data$.__enclos_env__$private$info$weight)) {
          weight_test = data$.__enclos_env__$private$info$weight[test_indexDT$indices]
        }
        init_score_test = NULL
        if (!is.null(data$.__enclos_env__$private$info$init_score)) {
          init_score_test = data$.__enclos_env__$private$info$init_score[test_indexDT$indices]
        }
        dtest <- gpb.Dataset(data = as.matrix(data$.__enclos_env__$private$raw_data[test_indexDT$indices,]),
                             label = data$.__enclos_env__$private$info$label[test_indexDT$indices],
                             weight = weight_test,
                             init_score = init_score_test,
                             reference = dtrain,
                             colnames = data$.__enclos_env__$private$colnames,
                             categorical_feature = data$.__enclos_env__$private$categorical_feature,
                             params = data$.__enclos_env__$private$params,
                             free_raw_data = data$.__enclos_env__$private$free_raw_data)

      }# end not free_raw_data

      if (folds_have_group) {
        setinfo(dataset = dtest, name = "group", info = test_groups)
        setinfo(dataset = dtrain, name = "group", info = train_groups)
      }

      if (!is.null(gp_model)) {

        group_data_pred <- NULL
        group_data <- gp_model$get_group_data()
        if (!is.null(group_data)) {
          group_data_pred <- group_data[test_indexDT$indices,]
          group_data <- group_data[train_indexDT$indices,]
        }

        group_rand_coef_data_pred <- NULL
        group_rand_coef_data <- gp_model$get_group_rand_coef_data()
        if (!is.null(group_rand_coef_data)) {
          group_rand_coef_data_pred <- group_rand_coef_data[test_indexDT$indices,]
          group_rand_coef_data <- group_rand_coef_data[train_indexDT$indices,]
        }

        gp_coords_pred <- NULL
        gp_coords <- gp_model$get_gp_coords()
        if (!is.null(gp_coords)) {
          gp_coords_pred <- gp_coords[test_indexDT$indices,]
          gp_coords <- gp_coords[train_indexDT$indices,]
        }

        gp_rand_coef_data_pred <- NULL
        gp_rand_coef_data <- gp_model$get_gp_rand_coef_data()
        if (!is.null(gp_rand_coef_data)) {
          gp_rand_coef_data_pred <- gp_rand_coef_data[test_indexDT$indices,]
          gp_rand_coef_data <- gp_rand_coef_data[train_indexDT$indices,]
        }

        cluster_ids_pred <- NULL
        cluster_ids <- gp_model$get_cluster_ids()
        if (!is.null(cluster_ids)) {
          cluster_ids_pred <- cluster_ids[test_indexDT$indices]
          cluster_ids <- cluster_ids[train_indexDT$indices]
        }

        weights <- gp_model$get_weights()
        if (!is.null(weights)) {
          weights <- weights[train_indexDT$indices]
        }

        gp_model_train <- gpb.GPModel$new(likelihood = gp_model$get_likelihood_name()
                                          , group_data = group_data
                                          , group_rand_coef_data = group_rand_coef_data
                                          , ind_effect_group_rand_coef = gp_model$.__enclos_env__$private$ind_effect_group_rand_coef
                                          , drop_intercept_group_rand_effect = gp_model$.__enclos_env__$private$drop_intercept_group_rand_effect
                                          , gp_coords = gp_coords
                                          , gp_rand_coef_data = gp_rand_coef_data
                                          , cov_function = gp_model$.__enclos_env__$private$cov_function
                                          , cov_fct_shape = gp_model$.__enclos_env__$private$cov_fct_shape
                                          , gp_approx = gp_model$.__enclos_env__$private$gp_approx
                                          , num_parallel_threads = gp_model$.__enclos_env__$private$num_parallel_threads
                                          , matrix_inversion_method = gp_model$.__enclos_env__$private$matrix_inversion_method
                                          , weights = weights
                                          , likelihood_learning_rate = gp_model$.__enclos_env__$private$likelihood_learning_rate
                                          , cov_fct_taper_range = gp_model$.__enclos_env__$private$cov_fct_taper_range
                                          , cov_fct_taper_shape = gp_model$.__enclos_env__$private$cov_fct_taper_shape
                                          , num_neighbors = gp_model$.__enclos_env__$private$num_neighbors
                                          , vecchia_ordering = gp_model$.__enclos_env__$private$vecchia_ordering
                                          , ind_points_selection = gp_model$.__enclos_env__$private$ind_points_selection
                                          , num_ind_points = gp_model$.__enclos_env__$private$num_ind_points
                                          , cover_tree_radius = gp_model$.__enclos_env__$private$cover_tree_radius
                                          , seed = gp_model$.__enclos_env__$private$seed
                                          , cluster_ids = cluster_ids
                                          , likelihood_additional_param = gp_model$.__enclos_env__$private$likelihood_additional_param
                                          , free_raw_data = gp_model$.__enclos_env__$private$free_raw_data)

        valid_set_gp <- NULL
        if (use_gp_model_for_validation) {
          gp_model_train$set_prediction_data(group_data_pred = group_data_pred
                                             , group_rand_coef_data_pred = group_rand_coef_data_pred
                                             , gp_coords_pred = gp_coords_pred
                                             , gp_rand_coef_data_pred = gp_rand_coef_data_pred
                                             , cluster_ids_pred = cluster_ids_pred
                                             , vecchia_pred_type = gp_model$.__enclos_env__$private$vecchia_pred_type
                                             , num_neighbors_pred = gp_model$.__enclos_env__$private$num_neighbors_pred
                                             , cg_delta_conv_pred = gp_model$.__enclos_env__$private$cg_delta_conv_pred
                                             , nsim_var_pred = gp_model$.__enclos_env__$private$nsim_var_pred
                                             , rank_pred_approx_matrix_lanczos = gp_model$.__enclos_env__$private$rank_pred_approx_matrix_lanczos)
          if (has_custom_eval_functions) {
            # Note: Validation using the GP model is only done in R if there are custom evaluation functions in eval_functions,
            #        otherwise it is directly done in C++. See the function Eval() in regression_metric.hpp
            valid_set_gp <- list(group_data_pred = group_data_pred,
                                 group_rand_coef_data_pred = group_rand_coef_data_pred,
                                 gp_coords_pred = gp_coords_pred,
                                 gp_rand_coef_data_pred = gp_rand_coef_data_pred,
                                 cluster_ids_pred = cluster_ids_pred)
          }

        }

        booster <- Booster$new(params = params, train_set = dtrain, gp_model = gp_model_train)
        gp_model$set_likelihood(gp_model_train$get_likelihood_name()) ## potentially change likelihood in case this was done in the booster to reflect implied changes in the default optimizer for different likelihoods
        gp_model_train$set_optim_params(params = gp_model$get_optim_params())

      } else {
        booster <- Booster$new(params = params, train_set = dtrain)
      }

      booster$add_valid(data = dtest, name = "valid", valid_set_gp = valid_set_gp,
                        use_gp_model_for_validation = use_gp_model_for_validation)

      return(
        list(booster = booster)
      )
    }
  )

  # Create new booster
  cv_booster <- CVBooster$new(x = bst_folds)

  # Callback env
  env <- CB_ENV$new()
  env$model <- cv_booster
  env$begin_iteration <- begin_iteration
  env$end_iteration <- end_iteration
  error_in_first_iteration <- FALSE

  # Start training model using number of iterations to start and end with
  for (i in seq.int(from = begin_iteration, to = end_iteration)) {

    # Overwrite iteration in environment
    env$iteration <- i
    env$eval_list <- list()

    for (f in cb$pre_iter) {
      f(env)
    }

    # Update one boosting iteration
    tryCatch({

      msg <- lapply(cv_booster$boosters, function(fd) {
        fd$booster$update(fobj = fobj)
        out <- list()
        for (eval_function in eval_functions) {
          out <- append(out, fd$booster$eval_valid(feval = eval_function))
        }

        return(out)
      })

    },
    error = function(err) {
      message(paste0("Error in boosting iteration ", i))
      message(err)
      env$met_early_stop <- TRUE
      if (env$iteration == begin_iteration) {
        error_in_first_iteration <<- TRUE
      }

    })# end tryCatch

    # Check for early stopping and break if needed
    if (error_in_first_iteration) {
      cv_booster$best_score <- NA
      return(cv_booster)
    } else {

      tryCatch({

        # Prepare collection of evaluation results
        merged_msg <- gpb.merge.cv.result(
          msg = msg
          , showsd = showsd
        )

        # Write evaluation result in environment
        env$eval_list <- merged_msg$eval_list

        # Check for standard deviation requirement
        if (showsd) {
          env$eval_err_list <- merged_msg$eval_err_list
        }

        # Loop through env
        for (f in cb$post_iter) {
          f(env)
        }

      },
      error = function(err) {
        env$met_early_stop <- TRUE
        if (env$iteration == begin_iteration) {
          error_in_first_iteration <<- TRUE
        }
        message(paste0("Error in boosting iteration ", i))
      })# end tryCatch

      if (error_in_first_iteration) {
        cv_booster$best_score <- NA
        return(cv_booster)
      }

    }

    if (env$met_early_stop) break

  }

  # When early stopping is not activated, we compute the best iteration / score ourselves
  # based on the first first metric
  if (record && is.na(env$best_score)) {
    # when using a custom eval function, the metric name is returned from the
    # function, so figure it out from record_evals
    if (!is.null(eval_functions[1L])) {
      first_metric <- names(cv_booster$record_evals[["valid"]])[1L]
    } else {
      first_metric <- cv_booster$.__enclos_env__$private$eval_names[1L]
    }
    .find_best <- which.min
    if (isTRUE(env$eval_list[[1L]]$higher_better[1L])) {
      .find_best <- which.max
    }
    cv_booster$best_iter <- unname(
      .find_best(
        unlist(
          cv_booster$record_evals[["valid"]][[first_metric]][[.EVAL_KEY()]]
        )
      )
    )
    cv_booster$best_score <- cv_booster$record_evals[["valid"]][[first_metric]][[.EVAL_KEY()]][[cv_booster$best_iter]]
  }

  if (!is.null(gp_model) & fit_GP_cov_pars_OOS) {
    pred_fixed_effect_OOS <- rep(NA,nrow(data))
    for (k in 1:nfold) {
      fd <- cv_booster$boosters[[k]]
      ##Predict on OOS data
      predictor <- Predictor$new(fd$booster$.__enclos_env__$private$handle)
      pred_fixed_effect_OOS[folds[[k]]] = predictor$predict( data = data$.__enclos_env__$private$raw_data[folds[[k]],]
                                                             , start_iteration = 0L
                                                             , num_iteration = cv_booster$best_iter
                                                             , rawscore = TRUE
                                                             , predleaf = FALSE
                                                             , predcontrib = FALSE
                                                             , header = FALSE
                                                             , reshape = FALSE )
    }

    # message("Fitting GPModel on out-of-sample data...") # message removed in version 0.7.8
    if(gp_model$get_likelihood_name() == "gaussian"){
      gp_model$fit(y = data$.__enclos_env__$private$info$label - pred_fixed_effect_OOS)
    }
    else{
      gp_model$fit(y = data$.__enclos_env__$private$info$label, offset = pred_fixed_effect_OOS)
    }

  }

  if (reset_data) {
    lapply(cv_booster$boosters, function(fd) {
      # Store temporarily model data elsewhere
      booster_old <- list(
        best_iter = fd$booster$best_iter
        , best_score = fd$booster$best_score
        , record_evals = fd$booster$record_evals
      )
      # Reload model
      fd$booster <- gpb.load(model_str = fd$booster$save_model_to_string())
      fd$booster$best_iter <- booster_old$best_iter
      fd$booster$best_score <- booster_old$best_score
      fd$booster$record_evals <- booster_old$record_evals
    })
  }
  if (delete_boosters_folds) {
    lapply(cv_booster$boosters, function(fd) {
      fd$booster$.__enclos_env__$private$finalize()
    })
    cv_booster$boosters = NULL
  }

  return(cv_booster)

}
