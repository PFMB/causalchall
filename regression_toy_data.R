rm(list = ls())
pacman::p_load("keras","tensorflow", "tfdatasets", "reticulate","dplyr",
               "randomForest","mgcv","ggplot2","stringr","data.table","ROCR")
set.seed(1)
n <- 1000
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
x <- matrix(rnorm(n*30), ncol = 30) # noise variables
x_fac <- as.character(sample(c("a","b","c"), n, replace = T))
y <- 3*x1*x2*x3 + 3*x2^3 - 5*sqrt(x3) + (x_fac != "a")*3 + rnorm(n)
d = data.frame(y = y,x1,x2,x3,x_fac,x)

row_idx <- 1:nrow(d)
tst_idx <- sample(row_idx, round(0.2*nrow(d)))
tr_idx <- row_idx[!row_idx %in% tst_idx]

SL.NN_base <- function(Y, X, newX = NULL, family = "binomial", nn_arc = "A") {
  
  # X: named matrix of features for training
  # Y: vector of responses/labels for training 
  # newX: named matrix of features for testing
  # family: character "binomial" for binary classification, regression otherwise
  # type of architecture: 1-4 as integer
  
  # no train/test split needed due to super learner  
  # row_idx <- 1:nrow(X)
  # tst_idx <- sample(row_idx, round(0.2*nrow(X)))
  # tr_idx <- row_idx[!row_idx %in% tst_idx]
  
  pacman::p_load("keras","tensorflow", "tfdatasets", "reticulate","dplyr")
  st_time <- Sys.time()
  set_random_seed(1)
  
  #cbind(X, model.matrix(~ .^2, data = X))
  #cbind(newX, model.matrix(~ .^2, data = X))
  
  dd <- X <- X %>% as_tibble(.name_repair = "minimal")
  newX <- newX %>% as_tibble(.name_repair = "minimal")
  Y <- array(Y)
  dd$Y <- Y
  
  spec <- feature_spec(dd, Y ~ . ) %>%
    #step_crossed_column(c(all_numeric(), all_nominal()), hash_bucket_size = 100) %>%
    step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
    step_categorical_column_with_vocabulary_list(all_nominal()) %>% # factors/categorical should be coded as char
    step_indicator_column(all_nominal()) %>% 
    #step_embedding_column(all_nominal(), dimension = ...) %>% 
    fit()
  
  # check: str(spec$dense_features())
  
  input <- layer_input_from_dataset(dd %>% select(-Y))
  
  output <- switch(nn_arc, A = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                                     layer_dense(units = 8, activation = "relu", use_bias = T, name = "layer_1",
                                                 kernel_regularizer = regularizer_l1(0.1)),
                          B = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                                        layer_dense(units = 8, activation = "relu", use_bias = T, name = "layer_1",
                                                    kernel_regularizer = regularizer_l1(0.1)) %>%
                                        layer_dense(units = 8, activation = "relu", use_bias = T, name = "layer_2",
                                                    kernel_regularizer = regularizer_l1(0.1)),
                          C = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                                        layer_dense(units = 64, activation = "relu", use_bias = T, name = "layer_1",
                                                    kernel_regularizer = regularizer_l1(0.1)) %>%
                                        layer_dropout(rate = 0.2) %>%
                                        layer_batch_normalization() %>%
                                        layer_dense(units = 64, activation = "relu", use_bias = T, name = "layer_2",
                                                    kernel_regularizer = regularizer_l1(0.1)) %>%
                                        layer_dropout(rate = 0.2) %>%
                                        layer_batch_normalization(),
                          D = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                                        layer_dense(units = 256, activation = "relu", use_bias = T, name = "layer_1",
                                                    kernel_regularizer = regularizer_l1(0.1)) %>%
                                        layer_dropout(rate = 0.2) %>%
                                        layer_batch_normalization() %>%
                                        layer_dense(units = 256, activation = "relu", use_bias = T, name = "layer_2", 
                                                    kernel_regularizer = regularizer_l1(0.1)) %>%
                                        layer_dropout(rate = 0.2) %>%
                                        layer_batch_normalization() %>%
                                        layer_dense(units = 256, activation = "relu", use_bias = T, name = "layer_3", 
                                                    kernel_regularizer = regularizer_l1(0.1)) %>%
                                        layer_dropout(rate = 0.2) %>%
                                        layer_batch_normalization())

  output <- output %>%  layer_dense(units = 1, name = "output_layer", 
                                    activation = ifelse(family == "binomial", "sigmoid", "linear")) 
  model <- keras_model(input, output)
  
  model %>% compile(
    loss = ifelse(family == "binomial", "binary_crossentropy", "mse"), #loss_huber(), loss_mean_squared_error(), loss_binary_crossentropy()
    optimizer = optimizer_rmsprop(learning_rate = 1e-3), #optimizer_rmsprop
    metrics = ifelse(family == "binomial", "accuracy", "mse") # "mse","mae","accuracy"
  )
  
  lr_sched =  list(
    callback_early_stopping(monitor = "val_loss", patience = ifelse(family == "binomial", 200, 100)),
    callback_reduce_lr_on_plateau(monitor = ifelse(family == "binomial", "val_accuracy", "val_mse"),patience = 30, factor = 0.8)
    # callback_tensorboard("logs/run_a", histogram_freq = 5)
    # callback_learning_rate_scheduler(
    #   tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .8))
  )
  
  history <- model %>% fit(x = X, y = Y, epochs = 12e3,
                           validation_split = 0.2, verbose = 2, batch_size = 32L,#shuffle = FALSE,
                           view_metrics = FALSE, callbacks = lr_sched
  )
  
  end_time <- Sys.time()
  cat("- NN learner took", format(end_time - st_time, units = "min"),"-\n")
  
  model %>% predict(newX)
}

## Classifier 1:
nn_pred <- SL.NN_base(Y = d[tr_idx,"y"], X = d[tr_idx,] %>% select(-y), 
                      newX = d[tst_idx,] %>% select(-y), family = "gaussian", nn_arc = "A")
mean((nn_pred - d[tst_idx,"y"])^2)

## Classifier 2:
d$x_fac <- with(d, as.factor(x_fac))
form <- paste0("y ~ s(x1,x2,x3) + s(x2) + s(x3) + x_fac + ", 
               paste0("X",1:ncol(x), collapse = " + "))
m <- gam(as.formula(form), family = gaussian(), data = d[tr_idx,]); summary(m)
gam_pred <- predict(m, type = "response", newdata = d[tst_idx,])
mean((gam_pred - d[tst_idx,"y"])^2)

## Classifier 3:
fit.rf <- randomForest(y = as.factor(d[tr_idx,"y"]), x = d[tr_idx,] %>% select(-y), 
                       ntree = 300, xtest = d[tst_idx,] %>% select(-y), 
                       keep.forest = TRUE, importance = TRUE)
rf_pred <- fit.rf$test$predicted

## Classifier 4:
SL.xgboost_man <- function (Y, X, newX, family, obsWeights, id, ntrees = 1000, 
                            max_depth = 4, shrinkage = 0.1, minobspernode = 10, params = list(), 
                            nthread = 1, verbose = 0, save_period = NULL, ...) 
{
  #.SL.require("xgboost")
  if (packageVersion("xgboost") < 0.6) 
    stop("SL.xgboost requires xgboost version >= 0.6, try help('SL.xgboost') for details")
  if (!is.matrix(X)) {
    X = model.matrix(~. - 1, X)
  }
  xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)
  if (family$family == "gaussian") {
    if (packageVersion("xgboost") >= "1.1.1.1") {
      objective <- "reg:squarederror"
    }
    else {
      objective <- "reg:linear"
    }
    model = xgboost::xgboost(data = xgmat, objective = objective, 
                             nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
                             eta = shrinkage, verbose = verbose, nthread = nthread, 
                             params = params, save_period = save_period)
  }
  if (family$family == "binomial") {
    model = xgboost::xgboost(data = xgmat, objective = "binary:logistic", 
                             nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
                             eta = shrinkage, verbose = verbose, nthread = nthread, 
                             params = params, save_period = save_period, eval_metric = "logloss")
  }
  if (family$family == "multinomial") {
    model = xgboost::xgboost(data = xgmat, objective = "multi:softmax", 
                             nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
                             eta = shrinkage, verbose = verbose, num_class = length(unique(Y)), 
                             nthread = nthread, params = params, save_period = save_period)
  }
  if (!is.matrix(newX)) {
    newX = model.matrix(~. - 1, newX)
  }
  pred = predict(model, newdata = newX)
  fit = list(object = model)
  class(fit) = c("SL.xgboost")
  out = list(pred = pred, fit = fit)
  return(out)
}

famil_y <- list(family = "binomial")
out_p <- SL.xgboost_man(Y = d[tr_idx,"y"], X = d[tr_idx,] %>% select(-y),
                        newX = d[tst_idx,] %>% select(-y), family = famil_y, 
                        obsWeights = rep(1, length(tr_idx)))

preds <- data.frame(nn = nn_pred, gam = gam_pred, rf = fit.rf$test$votes[,"1"], 
                    xgb = out_p$pred, true = d[tst_idx, "y"])