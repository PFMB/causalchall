############
# LEARNERS #
############

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
    optimizer = optimizer_rmsprop(learning_rate = 1e-3), #optimizer_rmsprop
    metrics = ifelse(family == "binomial", "accuracy", "mse") # "mse","mae","accuracy"
  )
  
  lr_sched =  list(
    callback_early_stopping(monitor = "val_loss", patience = ifelse(family == "binomial", 30, 15)),
    callback_reduce_lr_on_plateau(monitor = ifelse(family == "binomial", "val_accuracy", "val_mse"),
                                  patience = ifelse(family == "binomial", 10, 5), factor = 0.8)
    # callback_tensorboard("logs/run_a", histogram_freq = 5)
    # callback_learning_rate_scheduler(
    #   tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .8))
  )
  
  history <- model %>% fit(x = X, y = Y, epochs = 12e3,
                           validation_split = 0.2, verbose = 2, batch_size = 32L,#shuffle = FALSE,
                           view_metrics = FALSE, callbacks = lr_sched, sample_weight = array(obsWeights)
  )
  
  #class(model) <- "SL.NN_base"
  fit <- list(object = model)
  class(fit) <- "SL.NN_base"
  pred <- model %>% predict(newX)
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

predict.SL.NN_base <- function(object, newdata, family, ...) {
  
  # SL issue: no. of columns of X and newX might differ for some unknown reason, this is
  # taken care of this in the SL.NN_base method but also needs to be taken care of
  # here
  
  vars <- names(get_input_at(object$object %>% get_layer("layer_0"), node_index = 0L))
  if (ncol(newdata) != length(vars)) {
    cl_nms <- colnames(newdata)
    newdata <- newdata[, cl_nms[cl_nms %in% vars], drop = F]
  }
  predict(object = object$object, x = newdata)
} 

# Try different Hyperparamters
tuneGrid <- expand.grid(nn_arc = c("A","B","C","D"), l1_pen = c(0.01, 0.1))
for (i in seq(nrow(tuneGrid))) {
  eval(parse(text = paste0(
    "SL.NN_base_arch_", tuneGrid[i, 1], "_l1_", tuneGrid[i, 2],
    "<- function(..., nn_arc = '", tuneGrid[i, 1], "', l1_pen = ", tuneGrid[i, 2], ")
                           {SL.NN_base(..., nn_arc = nn_arc, l1_pen = l1_pen)}"
  )))
}


SL.glm.interaction_info <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, ...) {
  cat("- GLM Interaction was started and is making predictions - \n")
  glm_time <- system.time({
    
    # chose family dependent upon response variable
    Y <- as.vector(as.matrix(Y))
    if (all(Y == 0 | Y == 1)) {
      family$family <- binomial()
    } else {
      family$family <- gaussian() # scat() also reasonable but computationally intense
    }
    
    if (is.matrix(X)) {
      X <- as.data.frame(X)
    }
    fit.glm <- glm(Y ~ .^2, data = X, family = family$family, weights = obsWeights)
    if (is.matrix(newX)) {
      newX <- as.data.frame(newX)
    }
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.glm"
    out <- list(pred = pred, fit = fit)
  })
  
  cat("- GLM Interaction was finished lasting: ", round(unclass(glm_time)["elapsed"], 2), " - \n")
  return(out)
}


SL.gam2 <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, deg.gam = 2, cts.num = 5,
                    ...) {
  
  # deg.gam == 2 is needed due to frequently occurring convergence failure
  # chose family dependent upon response variable
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- binomial()
  } else {
    family$family <- gaussian() # scat() also reasonable but computationally intense
  }
  
  cat(" - gam::gam was started and is making predictions - \n")
  gam_time <- system.time({
    SuperLearner:::.SL.require("gam")
    s <- gam:::s # s() is also used by 'mgcv' package - avoid clash
    
    # adjust model formula for metric and categorical predictors
    metric_var <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
    if (sum(metric_var) != 0 & sum(metric_var) != length(metric_var)) {
      # metric and categorical variables
      gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                      colnames(X[, metric_var, drop = FALSE]), ",", deg.gam,
                                                      ")",
                                                      sep = ""
      ), collapse = "+"), "+", paste(colnames(X[,
                                                !metric_var,
                                                drop = FALSE
      ]), collapse = "+")))
    }
    if (all(metric_var)) {
      # metric variables only
      gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                      colnames(X[, metric_var, drop = FALSE]), ",", deg.gam,
                                                      ")",
                                                      sep = ""
      ), collapse = "+")))
    } else {
      # all categorical
      gam.model <- as.formula(paste("Y~", paste(colnames(X),
                                                collapse = "+"
      ), sep = ""))
    }
    fit.gam <- gam::gam(gam.model,
                        data = X, family = family$family,
                        control = gam::gam.control(maxit = 50, bf.maxit = 50),
                        weights = obsWeights
    )
    # or predict.gam depending on version
    pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
    fit <- list(object = fit.gam)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.gam")
  })
  cat("- gam::gam was finished lasting: ", round(unclass(gam_time)["elapsed"], 2), " - \n")
  return(out)
}


SL.randomForest_base <- function(Y, X, newX = NULL, family = list(), mtry = ifelse(family$family ==
                                                                                     "gaussian", max(floor(ncol(X) / 3), 1), floor(sqrt(ncol(X)))),
                                 ntree = 300, nodesize = ifelse(family$family == "gaussian",
                                                                5, 1
                                 ), maxnodes = NULL, importance = FALSE, ...) {
  cat(" - randomForest was started (ntree =", ntree, ") and is making predictions - \n")
  randomForest_time <- system.time({
    SuperLearner:::.SL.require("randomForest")
    
    # avoid infinite search for split points in trees
    if (all(apply(X,2,var) == 0)) {
      fit.rf <- "Empty"
      attr(fit.rf, "class") <- "try-error"
      pred <- rep(mean(Y), nrow(Xnew))
      fit <- list(object = fit.rf)
      cat("- Failed random forest - \n")
    }
    
    if (family$family == "gaussian" & !exists("fit.rf")) {
      fit.rf <- randomForest::randomForest(Y ~ .,
                                           data = X,
                                           ntree = ntree, xtest = newX, keep.forest = TRUE,
                                           mtry = mtry, nodesize = nodesize, maxnodes = maxnodes,
                                           importance = importance
      )
      try(pred <- fit.rf$test$predicted, silent = TRUE)
      if (any(class(fit.rf) == "try-error")) {
        pred <- rep(mean(Y), nrow(Xnew))
        cat("- Failed random forest - \n")
      }
      fit <- list(object = fit.rf)
    }
    if (family$family == "binomial" & !exists("fit.rf")) {
      fit.rf <- randomForest::randomForest(
        y = as.factor(Y),
        x = X, ntree = ntree, xtest = newX, keep.forest = TRUE,
        mtry = mtry, nodesize = nodesize, maxnodes = maxnodes,
        importance = importance
      )
      try(pred <- fit.rf$test$votes[, 2], silent = TRUE)
      if (any(class(fit.rf) == "try-error")) {
        pred <- rep(mean(Y), nrow(Xnew))
        cat("- Failed random forest - \n")
      }
      fit <- list(object = fit.rf)
    }
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.randomForest")
  })
  cat("- randomForest was finished lasting: ", round(unclass(randomForest_time)["elapsed"], 2), " - \n")
  return(out)
}

SL.gbm2  <- function(Y, X, newX, family, obsWeights, gbm.trees = 10000,
    interaction.depth = 2, shrinkage = 0.001, ...)
{
    #cat("boosting \n")
    gbm.model <- as.formula(paste("Y~", paste(colnames(X), collapse = "+")))
    if (family$family == "gaussian") {
        fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "gaussian",
            n.trees = gbm.trees, interaction.depth = interaction.depth,
            shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, bag.fraction=0.75,
            weights = obsWeights, verbose = FALSE)
    }
    if (family$family == "binomial" & all(Y%in%c(0,1))) {
        fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "bernoulli",
            n.trees = gbm.trees, interaction.depth = interaction.depth,
            shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, bag.fraction=0.75,
            weights = obsWeights, verbose = FALSE)
    }
    if (family$family == "binomial" & all(Y%in%c(0,1))==FALSE) {
    fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "gaussian",
            n.trees = gbm.trees, interaction.depth = interaction.depth,
            shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, bag.fraction=0.75,
            weights = obsWeights, verbose = FALSE)
    }
    best.iter <- gbm::gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
    pred <- predict(fit.gbm, newdata = newX, best.iter, type = "response")
    fit <- list(object = fit.gbm, n.trees = best.iter)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.gbm")
    return(out)
}

SL.earth2 <- function(Y, X, newX, family, obsWeights, id, degree = 2, penalty = 3,
    nk = max(21, 2 * ncol(X) + 1), pmethod = "backward", nfold = 0,
    ncross = 1, minspan = 0, endspan = 0, ...)
{
    #cat("earth \n")
    if (family$family == "gaussian") {
        fit.earth <- earth::earth(x = X, y = Y, degree = degree,
            nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
            ncross = ncross, minspan = minspan, endspan = endspan)
    }
    if (family$family == "binomial" & all(Y%in%c(0,1))){
        fit.earth <- earth::earth(x = X, y = Y, degree = degree,
            nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
            ncross = ncross, minspan = minspan, endspan = endspan,
            glm = list(family = binomial))
    }
    if (family$family == "binomial" & all(Y%in%c(0,1))==FALSE) {
    fit.earth <- earth::earth(x = X, y = Y, degree = degree,
            nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
            ncross = ncross, minspan = minspan, endspan = endspan)
    }
    pred <- predict(fit.earth, newdata = newX, type = "response")
    fit <- list(object = fit.earth)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.earth")
    return(out)
}

#############
# Screening #
#############
library(glmnet)
screen.glmnet3 <- function(Y, X, family, alpha = 1, minscreen = 2, pw=F,
                             maxtries=4, nfolds = 10, nlambda = 200, ...){
    #cat("screen: Lasso \n")
    if(family$family == "binomial" & all(Y%in%c(0,1))==FALSE){myfamily<- "gaussian"}else{myfamily<-family$family}
    if (!is.matrix(X)) {X <- try(model.matrix(~-1 + ., data=X),silent=T)}
    successfulfit <- FALSE
    fitCV <- try(glmnet::cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = "deviance",
        nfolds = nfolds, family = myfamily, alpha = alpha,
        nlambda = nlambda, keep=T),silent=T)
    if(class(fitCV)=="try-error"){
    i <- 2
    while(successfulfit==FALSE & i<=maxtries){
    if(pw==T){cat(paste("glmnet failed, new try #",i,"\n"))}
    fitCV <- try(glmnet::cv.glmnet(x = X, y = Y, lambda = log(fitCV$glmnet.fit$lambda+1), type.measure = "deviance",
        nfolds = nfolds, family = myfamily, alpha = alpha, nfolds=4,
        nlambda = nlambda*(i+3), keep=T, foldid=sample(fitCV$foldid)),silent=T)
    i <- i+1
    if(class(fitCV)=="try-error"){successfulfit <- FALSE
    }else{successfulfit <- TRUE}
    }
    }else{successfulfit <- TRUE}
    whichVariable <- NULL
    if(successfulfit==TRUE){
    si <- try(abs((max(fitCV$nzero)- (dim(X)[2]/2))-fitCV$nzero),silent=T)
    whichVariable2 <- try((as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] != 0),silent=T)
    whichVariable3 <- try(as.numeric(glmnet::coef.glmnet(fitCV, s = fitCV$lambda[si==min(si)][1]))[-1] != 0,silent=T)
    if(sum(whichVariable2)>1){whichVariable<-whichVariable2}else{if(sum(whichVariable3)>1 & sum(whichVariable3)<(dim(X)[2]/2)){whichVariable<-whichVariable3}}
    }
    if(is.null(whichVariable)){
    whichVariable<-screen.cramersv(Y,X)
    if(pw==T){cat("Lasso failed and screening was based on Cramer's V\n")}}
    if(pw==T){cat(paste("Number of included variables:",sum(whichVariable),"\n"))}
    return(whichVariable)
}


screen.glmnet_nVar <- function(Y, X, family, alpha = 0.75, nfolds = 5, nlambda = 200, nVar = 5,...){
  
  #cat("screen: Elastic Net \n")
  # chose family dependent upon response variable
  Y <- as.vector(as.matrix(Y))
  if (all(Y==0|Y==1)) {
    family$family = "binomial"
  } else{
    family$family = "gaussian"
  }
  if (!is.matrix(X)) {X <- try(model.matrix(~-1 + ., data=X),silent=T)}
  if(ncol(X)>1){ # if only one variable, no screening is conducted
    whichVariable <- rep(FALSE,ncol(X)) # no intercept
      fitCV <- try(glmnet::cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = "deviance",
                               nfolds = nfolds, family = family$family, alpha = alpha,
                               nlambda = nlambda, keep=T),silent=TRUE)
    if(all(fitCV$nzero==0)){ # transform lamdas from prior fit and try again
      fitCV <- try(glmnet::cv.glmnet(x = X, y = Y, lambda = log(fitCV$glmnet.fit$lambda+1), type.measure = "deviance",
                                     nfolds = nfolds, family = family$family, alpha = alpha, keep=T),silent=TRUE)
    }
    if(all(fitCV$nzero!=nVar)){ # nVar is not available
    lambda_index_with_nVar <- min(which(abs(fitCV$nzero-nVar)==min(abs(fitCV$nzero-nVar))))
    }else{ # lambda value for the no. of coef. as defined in nVar
    lambda_index_with_nVar <- min(which(fitCV$nzero==nVar))
    }
    coefList <- coef(fitCV$glmnet.fit, s=fitCV$glmnet.fit$lambda[lambda_index_with_nVar])
    non_zero_index <- which(coefList!=0)[-1]-1 # drop intercept and adjust index reducing one
    whichVariable[non_zero_index] <- TRUE
  }else{
    whichVariable<-screen.cramersv(Y,X)
  }
  #cat(paste0(sum(whichVariable)," vars were screened. \n"))
  #if (nVar!=sum(whichVariable)){
  #cat(paste0("nVar is not available, instead ", sum(whichVariable)," vars were screened. \n"))
  #}
  return(whichVariable)
}

library(vcd)
screen.cramersv <- function(Y, X, nscreen = 4, num_cat = 10, ...) {
  # cat("- screen.cramersv screens", nscreen, "variables -\n")
  # Selects nscreen variables among X that have the highest association with Y.
  # Metric vars (X or Y) are automatically turned into factors when the number
  # of unique observations falls below num_cat.
  if (ncol(X) > nscreen) {
    dat <- cbind(Y, X)
    contin_var <- apply(dat, 2, function(var) length(unique(var)) > num_cat)
    make_categ <- function(var) {
      cut(var, unique(quantile(var, prob = seq(0, 1, 0.2))), include.lowest = T)
    }
    if (any(contin_var)) {
      dat[, contin_var] <- apply(dat[, contin_var, drop = FALSE], 2, make_categ)
    }
    calc_cram_v <- function(x_var, y_var) vcd::assocstats(table(y_var, x_var))$cramer
    cramers_v <- apply(dat[, !colnames(dat) %in% "Y"], 2, calc_cram_v, y_var = dat[, "Y"])
    # cramers_v is between 0 and 1, the higher the more relevant
    return(unname(rank(-cramers_v) <= nscreen))
  }
  rep(TRUE, ncol(X))
}

screen.cramersv2 <- function(Y, X, nscreen = 8, num_cat = 10, ...) {
  # cat("- screen.cramersv screens", nscreen, "variables -\n")
  # Selects nscreen variables among X that have the highest association with Y.
  # Metric vars (X or Y) are automatically turned into factors when the number
  # of unique observations falls below num_cat.
  if (ncol(X) > nscreen) {
    dat <- cbind(Y, X)
    contin_var <- apply(dat, 2, function(var) length(unique(var)) > num_cat)
    make_categ <- function(var) {
      cut(var, unique(quantile(var, prob = seq(0, 1, 0.2))), include.lowest = T)
    }
    if (any(contin_var)) {
      dat[, contin_var] <- apply(dat[, contin_var, drop = FALSE], 2, make_categ)
    }
    calc_cram_v <- function(x_var, y_var) vcd::assocstats(table(y_var, x_var))$cramer
    cramers_v <- apply(dat[, !colnames(dat) %in% "Y"], 2, calc_cram_v, y_var = dat[, "Y"])
    # cramers_v is between 0 and 1, the higher the more relevant
    return(unname(rank(-cramers_v) <= nscreen))
  }
  rep(TRUE, ncol(X))
}

###############
# Diagnostics #
###############

cc_trunc <- function(ltmle_est,column=NULL){

  # if only one intervention: change matrices to arrays
  if(length(dim(ltmle_est$cum.g))==2){
  ltmle_est$cum.g            <- array(ltmle_est$cum.g, dim=c(dim(ltmle_est$cum.g ),1))
  ltmle_est$cum.g.unbounded  <- array(ltmle_est$cum.g.unbounded , dim=c(dim(ltmle_est$cum.g.unbounded ),1))
  ltmle_est$cum.g.used       <- array(ltmle_est$cum.g.used, dim=c(dim(ltmle_est$cum.g.used),1))
  ltmle_est$fit$g            <- list(ltmle_est$fit$g)
  }
  
  #
  number_interventions <- dim(ltmle_est$cum.g)[3]
  cc_trunc_matrix      <- matrix(NA,ncol=number_interventions,nrow=9)
  if(is.null(column)){last_column  <- ncol(ltmle_est$cum.g)}else{last_column <- column}
  
  for(i in 1:number_interventions){
  # lc <- max((1:last_column)[(unlist(lapply(ltmle_est$fit$g[[i]],class))=="matrix")[1:last_column]])  # last g column with updates 
  cum_prob_A_used <- ltmle_est$cum.g[,last_column,i] # last column = last point in time
  cum_prob_A_est  <- ltmle_est$cum.g.unbounded[,last_column,i] # last column = last point in time
  followed_A <- ltmle_est$cum.g.used[,last_column,i] # which subjects acutally followed A
  cc <- 1/cum_prob_A_used    # inverse probabilites 
  cc[!followed_A] <- 0       # those who did NOT follow A in every point in time til the very end are set to 0
  cc <- unclass(summary(cc)) # summary statistics for the clever covariate
  
  # how many probabilities were truncated (among those who followed A and are uncensored)
  trunc_share <- mean(cum_prob_A_est[followed_A]!=cum_prob_A_used[followed_A])
  
  # store results
  cc_trunc_matrix[,i] <- c(sum(followed_A),mean(followed_A),trunc_share,cc)  
  }
  

  # collect 
  rownames(cc_trunc_matrix) <- c("number followed","proportion followed","% truncated",names(cc))
  colnames(cc_trunc_matrix) <- c(paste("Intervention",c(1:number_interventions)))
 
  # print
  cat("Subjects following intervention:\n\n")
  print(round(cc_trunc_matrix[1:2,,drop=FALSE],digits=3))
  cat("\n")
  cat("\n")
  cat("% probabilites truncated (among those following abar) :\n\n")
  print(round(cc_trunc_matrix[3,,drop=FALSE],digits=3))
  cat("\n")
  cat("\n")
  cat("Summary of clever covariate :\n\n")
  print(round(cc_trunc_matrix[4:9,,drop=FALSE],digits=3))
  cat("\n") 
 
  # 
  return(invisible(cc_trunc_matrix))
}

library(ggplot2)
library(gridExtra) 

plot.learner <- function(x, by.intervention=TRUE){
  
  ltmle_est <- x
  
  ew <- function(myfit){
  if(is.character(myfit[[1]])){return(rep(NA,dim(ltmle_est$fit$Q[[1]][[1]])[1]))}else{
  return(myfit[,2])}
  }
  ewg <- function(myfit){
  if(is.character(myfit[[1]])){return(rep(NA,dim(ltmle_est$fit$g[[1]][[1]])[1]))}else{
  return(myfit[,2])}
  }
  if(length(dim(ltmle_est$cum.g))==2){
  ltmle_est$cum.g <- array(ltmle_est$cum.g, dim=c(dim(ltmle_est$cum.g ),1))
  ltmle_est$fit$g            <- list(ltmle_est$fit$g)
  ltmle_est$fit$Q            <- list(ltmle_est$fit$Q)
  }
  number_interventions <- dim(ltmle_est$cum.g)[3]
  
  learner_names <- names(ltmle_est$fit$Q[[1]][[1]][,"Coef"])
  learner_weights <- unlist(lapply(ltmle_est$fit$Q, function(x){lapply(x, ew)}))
  learner_data <- as.data.frame(learner_weights)
  colnames(learner_data) <- "weights"
  learner_data$Intervention <- c(paste("Intervention",sort(rep(1:number_interventions,length(learner_weights)/number_interventions )) ))
  learner_data$learner <- NA
  for(i in 1:length(learner_names)){learner_data$learner[sapply(names(learner_weights),FUN=function(x){grepl(learner_names[i],x)})] <- learner_names[i]}

  p1 <- ggplot(data = na.omit(learner_data)) + geom_pointrange(mapping = aes(x = learner, y = weights), stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = mean) + theme_bw() + ggtitle("Q-form Models") + 
  theme(axis.title.x = element_text(size=12), plot.title = element_text(hjust = 0.5, size=16), axis.text.x = element_text(size=12,angle=45,hjust=.5,vjust=.5,face="plain"),
  axis.title.y = element_text(size=12, angle = 90), axis.text.y = element_text(size=12)) + scale_x_discrete("learner \n")
  if(by.intervention==T){p1<-p1+facet_grid(. ~ Intervention)}

  learner_names_g <- names(ltmle_est$fit$g[[1]][[1]][,"Coef"])
  learner_weights_g <- unlist(lapply(ltmle_est$fit$g, function(x){lapply(x, ewg)}))
  learner_data_g <- as.data.frame(learner_weights_g)
  colnames(learner_data_g) <- "weights"
  learner_data_g$Intervention <- c(paste("Intervention",sort(rep(1:number_interventions,length(learner_weights_g)/number_interventions )) ))
  learner_data_g$learner <- NA
  for(i in 1:length(learner_names_g)){learner_data_g$learner[sapply(names(learner_weights_g),FUN=function(x){grepl(learner_names_g[i],x)})] <- learner_names_g[i]}

  p2 <- ggplot(data = na.omit(learner_data_g)) + geom_pointrange(mapping = aes(x = learner, y = weights), stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = mean) + theme_bw() + ggtitle("g-form Models") + 
  theme(axis.title.x = element_text(size=12), plot.title = element_text(hjust = 0.5, size=16), axis.text.x = element_text(size=12,angle=45,hjust=.5,vjust=.5,face="plain"),
  axis.title.y = element_text(size=12, angle = 90), axis.text.y = element_text(size=12)) + scale_x_discrete("learner \n")
  if(by.intervention==T){p2<-p2+facet_grid(. ~ Intervention)}

  return(grid.arrange(p1,p2))  
}

