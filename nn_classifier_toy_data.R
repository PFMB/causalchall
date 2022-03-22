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
y <- plogis(3*x1*x2*x3 + 3*x2^3 - 5*sqrt(x3) + (x_fac != "a")*3 + rnorm(n)) > 0.5; mean(y)
d = data.frame(y = as.integer(y),x1,x2,x3,x_fac,x)

row_idx <- 1:nrow(d)
tst_idx <- sample(row_idx, round(0.2*nrow(d)))
tr_idx <- row_idx[!row_idx %in% tst_idx]

SL.NN_base <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, nn_arc = "A", l1_pen = 0.1, ...) {
  
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
  
  X <- X[,apply(X, 2, function(x) all(x != 1))] # SL passes a column of 1s (intercept)
  newX <- newX[,apply(newX, 2, function(xx) all(xx != 1))]
  
  # SL sometimes indicates family$family == "binomial" when Y is actually continuous
  family <- family$family 
  if(length(unique(Y)) > 2) family <- "non-binomial"
  
  # SL sometimes adds an column in X/newX that is not in newX/X
  if (ncol(X) != ncol(newX)) {
    cl_Xcol <- colnames(X)
    cl_newX <- colnames(newX)
    newX <- newX[,cl_newX %in% cl_Xcol, drop = F]
    X <- X[,cl_Xcol %in% cl_newX, drop = F]
  }
  
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
                                 kernel_regularizer = regularizer_l1(l1_pen)),
                   B = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                     layer_dense(units = 8, activation = "relu", use_bias = T, name = "layer_1",
                                 kernel_regularizer = regularizer_l1(l1_pen)) %>%
                     layer_dense(units = 8, activation = "relu", use_bias = T, name = "layer_2",
                                 kernel_regularizer = regularizer_l1(l1_pen)),
                   C = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                     layer_dense(units = 64, activation = "relu", use_bias = T, name = "layer_1",
                                 kernel_regularizer = regularizer_l1(l1_pen)) %>%
                     layer_dropout(rate = 0.2) %>%
                     layer_batch_normalization() %>%
                     layer_dense(units = 64, activation = "relu", use_bias = T, name = "layer_2",
                                 kernel_regularizer = regularizer_l1(l1_pen)) %>%
                     layer_dropout(rate = 0.2) %>%
                     layer_batch_normalization(),
                   D = input %>% layer_dense_features(dense_features(spec), name = "layer_0") %>%
                     layer_dense(units = 256, activation = "relu", use_bias = T, name = "layer_1",
                                 kernel_regularizer = regularizer_l1(l1_pen)) %>%
                     layer_dropout(rate = 0.2) %>%
                     layer_batch_normalization() %>%
                     layer_dense(units = 256, activation = "relu", use_bias = T, name = "layer_2", 
                                 kernel_regularizer = regularizer_l1(l1_pen)) %>%
                     layer_dropout(rate = 0.2) %>%
                     layer_batch_normalization() %>%
                     layer_dense(units = 256, activation = "relu", use_bias = T, name = "layer_3", 
                                 kernel_regularizer = regularizer_l1(l1_pen)) %>%
                     layer_dropout(rate = 0.2) %>%
                     layer_batch_normalization())
  
  output <- output %>%  layer_dense(units = 1, name = "output_layer", 
                                    activation = ifelse(family == "binomial", "sigmoid", "linear")) 
  model <- keras_model(input, output)
  
  model %>% compile(
    loss = ifelse(family == "binomial", "binary_crossentropy", "mse"), #loss_huber(), loss_mean_squared_error(), loss_binary_crossentropy()
    optimizer = optimizer_rmsprop(learning_rate = 1e-2), #optimizer_rmsprop
    metrics = ifelse(family == "binomial", "accuracy", "mse") # "mse","mae","accuracy"
  )
  
  cat("Estimation with family", family, "and loss",model$loss,"\n")
  
  # loss and mse might not match during training due to regularizer (l1) and obs. weights
  
  lr_sched =  list(# accuracy is too unstable for callbacks, stops too quickly
    callback_early_stopping(monitor = ifelse(family == "binomial", "val_loss", "val_mse"), patience = ifelse(family == "binomial", 150, 75)),
    callback_reduce_lr_on_plateau(monitor = ifelse(family == "binomial", "val_loss", "val_mse"),
                                  patience = ifelse(family == "binomial", 50, 25), factor = 0.5)
    # callback_tensorboard("logs/run_a", histogram_freq = 5)
    # callback_learning_rate_scheduler(
    #   tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .8))
  )
  
  history <- model %>% fit(x = X, y = Y, epochs = 12e3,
                           validation_split = 0.2, verbose = 2, batch_size = 64L, shuffle = FALSE,
                           view_metrics = FALSE, callbacks = lr_sched#, sample_weight = array(obsWeights/1000)
  )
  
  #class(model) <- "SL.NN_base"
  fit <- list(object = model)
  class(fit) <- "SL.NN_base"
  pred <- model %>% predict(newX)
  #cat("Mean NN Test:",mean(pred),"\n")
  out <- list(pred = pred, fit = fit)
  
  # }, error = function(e) {
  #   
  #   meanY <- weighted.mean(Y, w = obsWeights)
  #   pred <- rep.int(meanY, times = nrow(newX))
  #   fit <- list(object = meanY)
  #   out <- list(pred = pred, fit = fit)
  #   class(out$fit) <- c("SL.mean")
  #   cat("- NN failed: took weighted mean instead - \n")
  #   out
  #   
  # })
  
  end_time <- Sys.time()
  cat("- NN learner took", format(end_time - st_time, units = "min"),"-\n")
  
  out
  
} 

## Classifier 1:
fam <- list(family = "binomial")
nn_pred <- SL.NN_base(Y = d[tr_idx,"y"], X = d[tr_idx,] %>% select(-y), 
                      newX = d[tst_idx,] %>% select(-y), family = fam, 
                      obsWeights = rep(1, nrow(d[tr_idx,])), nn_arc = "A")
mean(nn_pred$pred)

## Classifier 2:
d$x_fac <- as.factor(d$x_fac)
form <- paste0("y ~ s(x1,x2,x3) + s(x2) + s(x3) + x_fac + ", 
               paste0("X",1:ncol(x), collapse = " + "))
m <- gam(as.formula(form), family = binomial(), data = d[tr_idx,]); summary(m)
gam_pred <- predict(m, type = "response", newdata = d[tst_idx,])

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

preds <- data.frame(nn = nn_pred$pred, gam = gam_pred, rf = fit.rf$test$votes[,"1"], 
                    xgb = out_p$pred, true = d[tst_idx, "y"])

#colMeans(apply(preds, 2, function(x) x == preds$true))

### Receiver Operating Characteristics: Measures the pred. power in two-class problems; more informative than
# just one cut-off since multiple cut-offs are considered.
# We assign one of the two classes the signal and the other one the non-signal. Then we
# are able to distinguish between sensitivity (Prob. of pred. a signal, when signal 
# is actually the truth) and specificity (Prob. of pred. a no-signal, when no-signal
# is actually the truth). Remember Type I error ("false alarm"): Discarding H_0 although H_0 is actually true.
# So this is the prob of pred. a signal, when no-signal is actually the truth. Similarly, Type II ("miss")
# error: Not. discarding H_0 although H_1 is actually true.

# compute hit-rate (true-positive) against false-alarm rate (false-positive) for different cut-offs

calc_tpr_fpr <- function(cutoff = 0.5, pr) {
  pred_class <- pr[,-ncol(pr)] > cutoff
  tpr <- colMeans(pred_class[pr$true == 1,])
  fpr <- colMeans(pred_class[pr$true == 0,])
  data.frame(tpr,fpr)
} 

cut_off <- seq(0,1, by = 0.01)
tpr_fpr <- lapply(cut_off, calc_tpr_fpr, pr = preds)
tpr_fpr <- do.call("rbind",tpr_fpr)
rw_names <- rownames(tpr_fpr)
tpr_fpr$cutoff <- rep(as.character(cut_off), each = ncol(preds) - 1)
tpr_fpr$method <- str_extract(rw_names, "[aA-zZ]+")
setDT(tpr_fpr)

# taking the max helps to solve the non-monotonicity issue with ggplot
tpr_fpr[, max_tpr := max(tpr), by = list(fpr,method)]
ggplot(tpr_fpr) + geom_line(aes(x = fpr, y = max_tpr, color = method))

# not clear how to proceed with the non-monotonicity, see ROCR package for an comparison
# non-monotonicity stems from having different tpr for the same fpr
# see for a comparison:
# sorted_pr <- sort(preds$nn)
# dups <- rev(duplicated(rev(sorted_pr)))
# cutoffs <- c(Inf, sorted_pr[!dups])
# #trace(prediction, edit = T)
# pred <- ROCR::prediction(preds$nn, preds$true)
# #trace(performance, edit = T)
# perf <- ROCR::performance(pred,"tpr","fpr")
# ROCR::plot(perf,colorize=TRUE)

roc_func <- function(fpr_value) {
  d <- tpr_fpr[method == meth]
  idx <- which.min(abs(d$fpr - fpr_value))
  tpr_fpr[idx, ]$max_tpr # try tpr instead and compare
}

meth <- "nn"
round(integrate(Vectorize(roc_func), lower = 0, upper = 1, subdivisions = 200L)$value,3) # 0.988
meth <- "gam"
round(integrate(Vectorize(roc_func), lower = 0, upper = 1, subdivisions = 200L)$value,3) # 0.985
meth <- "rf"
round(integrate(Vectorize(roc_func), lower = 0, upper = 1, subdivisions = 200L)$value,3) # 0.973
meth <- "xgb"
round(integrate(Vectorize(roc_func), lower = 0, upper = 1, subdivisions = 200L)$value,3) # 0.988


