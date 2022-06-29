############
# LEARNERS #
############

# SL.xgboost_mod <- function (Y, X, newX, family, obsWeights, id, ntrees = 1000, 
#           max_depth = 4, shrinkage = 0.1, minobspernode = 10, params = list(), 
#           nthread = 1, verbose = 0, save_period = NULL, ...) 
# {
#   pacman::p_load(xgboost)
#   
#   if (packageVersion("xgboost") < 0.6) 
#     stop("SL.xgboost requires xgboost version >= 0.6, try help('SL.xgboost') for details")
#   if (!is.matrix(X)) {
#     X = model.matrix(~. - 1, X)
#   }
#   
#   #browser()
#   xgmat = xgboost::xgb.DMatrix(data = X, label = Y, weight = obsWeights)
#   if (family$family == "gaussian") {
#     if (packageVersion("xgboost") >= "1.1.1.1") {
#       objective <- "reg:squarederror"
#     }
#     else {
#       objective <- "reg:linear"
#     }
#     model = xgboost::xgboost(data = xgmat, objective = objective, 
#                              nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
#                              eta = shrinkage, verbose = verbose, nthread = nthread, 
#                              params = params, save_period = save_period)
#   }
#   if (family$family == "binomial") {
#     model = xgboost::xgboost(data = xgmat, objective = "binary:logistic", 
#                              nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
#                              eta = shrinkage, verbose = verbose, nthread = nthread, 
#                              params = params, save_period = save_period, eval_metric = "logloss")
#   }
#   if (family$family == "multinomial") {
#     model = xgboost::xgboost(data = xgmat, objective = "multi:softmax", 
#                              nrounds = ntrees, max_depth = max_depth, min_child_weight = minobspernode, 
#                              eta = shrinkage, verbose = verbose, num_class = length(unique(Y)), 
#                              nthread = nthread, params = params, save_period = save_period)
#   }
#   
# 
#   if (!is.matrix(newX)) {
#     newX = model.matrix(~. - 1, newX)
#   }
#   pred = predict(model, newdata = newX)
#   fit = list(object = model)
#   class(fit) = c("SL.xgboost")
#   out = list(pred = pred, fit = fit)
#   return(out)
# }
# 
ll <- list(c("SL.mean", "screen.corPearson"),
           c("SL.glm", "screen.corPearson"))

# ll <- list(c("SL.mean", "All"),
#            c("SL.glm", "All"),
#            c("SL.bayesglm", "All"),
#   
#            c("SL.mean", "screen.corPearson"),
#            c("SL.glm", "screen.corPearson"),
#            c("SL.bayesglm", "screen.corPearson"),
#            c("SL.rpart", "screen.corPearson"),
#            c("SL.gam2", "screen.corPearson"),
#            
#            c("SL.mean", "screen.cramersv_grid4"),
#            c("SL.glm", "screen.cramersv_grid4"),
#            c("SL.bayesglm", "screen.cramersv_grid4"),
#            c("SL.rpart", "screen.cramersv_grid4"),
#            c("SL.gam2", "screen.cramersv_grid4"),
#            
#            c("SL.mean", "screen.glmnet_nVar"),
#            c("SL.glm", "screen.glmnet_nVar"),
#            c("SL.bayesglm", "screen.glmnet_nVar"),
#            c("SL.rpart", "screen.glmnet_nVar"),
#            c("SL.gam2", "screen.glmnet_nVar"),
#            
#            c("SL.mean", "screen.glmnet3"),
#            c("SL.glm", "screen.glmnet3"),
#            c("SL.bayesglm", "screen.glmnet3"),
#            c("SL.rpart", "screen.glmnet3"),
#            c("SL.gam2", "screen.glmnet3"),
#            
#            c("SL.glm.interaction_info", "screen.cramersv_grid4"),
#            c("SL.glm.interaction_info", "screen.glmnet3"),
#            
#            c("SL.xgboost","All"),
#            c("SL.xgboost","screen.glmnet_nVar"),
#            c("SL.xgboost","screen.cramersv_grid4"),
#            
#            c("SL.earth2","All"),
#            c("SL.earth2","screen.glmnet_nVar"),
#            c("SL.earth2","screen.cramersv_grid4"),
#            
#            c("SL.nnet","All"),
#            c("SL.nnet","screen.glmnet_nVar"),
#            c("SL.nnet","screen.cramersv_grid4"),
#            
#            c("SL.randomForest_grid500","All"),
#            c("SL.randomForest_grid500","screen.glmnet_nVar"),
#            c("SL.randomForest_grid500","screen.cramersv_grid4"))
#            
#            #c("SL.NN_base_arch_A_l1_0.1", "All")
#            #c("SL.NN_base_arch_B_l1_0.01", "All"),
#            #c("SL.NN_base_arch_C_l1_0.01", "All"),
#            #c("SL.NN_base_arch_D_l1_0.01", "All"))
attr(ll, "return.fit") <- FALSE

# ll <- list(c("SL.mean", "All"),
#            c("SL.glm", "screen.corPearson"),
#            c("SL.glm", "screen.cramersv_grid4"))
attr(ll, "return.fit") <- FALSE

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
    whichVariable<-screen.cramersv_grid4(Y,X)
    if(pw==T){cat("Lasso failed and screening was based on Cramer's V\n")}}
  if(pw==T){cat(paste("Number of included variables:",sum(whichVariable),"\n"))}
  return(whichVariable)
}

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
  # Keras Version >= 2.8 0
  tryCatch(set_random_seed(1), error = function(e) use_session_with_seed(1))
  
  tryCatch({
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
        optimizer = optimizer_rmsprop(learning_rate = 0.1), #optimizer_rmsprop
        metrics = ifelse(family == "binomial", "accuracy", "mse") # "mse","mae","accuracy"
      )
      
      #cat("Estimation with family", family, "and loss",model$loss,"\n")
      
      # loss and mse might not match during training due to regularizer (l1) and obs. weights
      
      lr_sched =  list(
        callback_early_stopping(monitor = ifelse(family == "binomial", "val_accuracy", "val_mse"), patience = ifelse(family == "binomial", 50, 40)),
        callback_reduce_lr_on_plateau(monitor = ifelse(family == "binomial", "val_accuracy", "val_mse"),
                                      patience = ifelse(family == "binomial", 20, 15), factor = 0.1)
        # callback_tensorboard("logs/run_a", histogram_freq = 5)
        # callback_learning_rate_scheduler(
        #   tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .8))
      )
      
      history <- model %>% fit(x = X, y = Y, epochs = 12e3,
                               validation_split = 0.2, verbose = 0, batch_size = 16L, shuffle = FALSE,
                               view_metrics = FALSE, callbacks = lr_sched#, sample_weight = array(obsWeights/1000)
      )
      
      #class(model) <- "SL.NN_base"
      fit <- list(object = model)
      class(fit) <- "SL.NN_base"
      pred <- model %>% predict(newX)
      ##cat("Mean NN Test:",mean(pred),"\n")
      out <- list(pred = pred, fit = fit)
      out
  
  }, error = function(e) {

      meanY <- weighted.mean(Y, w = obsWeights)
      pred <- rep.int(meanY, times = nrow(newX))
      fit <- list(object = meanY)
      out <- list(pred = pred, fit = fit)
      class(out$fit) <- c("SL.mean")
      #cat("- NN failed: took weighted mean instead - \n")
      out

  })
  
  end_time <- Sys.time()
  #cat("- NN learner took", format(end_time - st_time, units = "min"),"-\n")
  
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

#### Screening ####

### 1) Random Forest with 'randomForest::randomForest'

screen.randomForest_base <- function(Y, X, family = list(), nVar = 8, ntree = 200, mtry = ifelse(family$family ==
                                                                                                   "gaussian", floor(sqrt(ncol(X))), max(floor(ncol(X) / 3), 1)),
                                     nodesize = ifelse(family$family == "gaussian", 5, 1), maxnodes = NULL,
                                     ...) {
  # chose family dependent upon response variable
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  if (ncol(X) > 8) {
    #cat("- screen.randomForest (ntree =", ntree, ") was started and screens", nVar, " vars. -\n")
    t_ime <- system.time({
      SuperLearner:::.SL.require("randomForest")
      if (family$family == "gaussian") {
        rank.rf.fit <- randomForest::randomForest(Y ~ .,
                                                  data = X,
                                                  ntree = ntree, mtry = mtry, nodesize = nodesize,
                                                  keep.forest = FALSE, maxnodes = maxnodes, importance = TRUE
        )
        # variables with the largest %IncMSE are the most important ones
        # negative scores mean zero or low importance
        imp_measure <- rank.rf.fit$importance[, "%IncMSE"]
        return(rank(-imp_measure, ties.method = "random") <= nVar)
      }
      if (family$family == "binomial") {
        rank.rf.fit <- randomForest::randomForest(as.factor(Y) ~ .,
                                                  data = X,
                                                  ntree = ntree, mtry = mtry, nodesize = nodesize,
                                                  keep.forest = FALSE, maxnodes = maxnodes, importance = TRUE
        )
        # variables with the largest mean decrease in accuracy are the most important ones
        # negative scores mean zero or low importance
        imp_measure <- rank.rf.fit$importance[, "MeanDecreaseAccuracy"]
        return(rank(-imp_measure, ties.method = "random") <= nVar)
      }
    })
    ##cat("- screen.randomForest finished - \n")
  }
  rep(TRUE, ncol(X))
}

# Try different Hyperparamters
tuneGrid <- expand.grid(ntree = c(500, 1000))
for (i in seq(nrow(tuneGrid))) {
  eval(parse(text = paste0(
    "screen.randomForest_grid", tuneGrid[i, 1],
    "<- function(..., ntree = ", tuneGrid[i, 1], ")
    {screen.randomForest_base(..., ntree = ntree)}"
  )))
}

### 2) Cramers V with vcd::assocstats

screen.cramersv_base <- function(Y, X, nscreen = 4, num_cat = 10, ...) {
  #cat("- screen.cramersv screens", nscreen, "variables -\n")
  pacman::p_load(vcd)
  
  # Selects nscreen variables among X that have the highest association with Y.
  # Metric vars (X or Y) are automatically turned into factors when the number
  # of unique observations falls below num_cat.
  
  if (ncol(X) > 8) {
    dat <- cbind(Y, X)
    contin_var <- apply(dat, 2, function(var) length(unique(var)) > num_cat)
    make_categ <- function(var) {
      cut(var, unique(quantile(var, prob = seq(0, 1, 0.2))), include.lowest = T)
    }
    if (any(contin_var)) {
      dat[, contin_var] <- apply(dat[, contin_var, drop = FALSE], 2, make_categ)
    }
    calc_cram_v <- function(x_var, y_var) assocstats(table(y_var, x_var))$cramer
    cramers_v <- apply(dat[, !colnames(dat) %in% "Y"], 2, calc_cram_v, y_var = dat[, "Y"])
    # cramers_v is between 0 and 1, the higher the more relevant
    return(unname(rank(-cramers_v, ties.method = "random") <= nscreen))
  }
  rep(TRUE, ncol(X))
}

# Try different Hyperparamters
tuneGrid <- expand.grid(nscreen = seq(4, 8, 4))
for (i in seq(nrow(tuneGrid))) {
  eval(parse(text = paste0(
    "screen.cramersv_grid", tuneGrid[i, 1],
    "<- function(..., nscreen = ", tuneGrid[i, 1], ")
                           {screen.cramersv_base(..., nscreen = nscreen)}"
  )))
}

### 3) Elastic Net with glmnet::cv.glmnet

screen.glmnet_nVar <- function(Y, X, family = list(), alpha = 0.75, nfolds = 5, nlambda = 150, nVar = 8, ...) {
  SuperLearner:::.SL.require("glmnet")
  
  # relevant for column names but shouldnt be a matrix anyways
  X <- as.data.frame(X)
  
  # chose family dependent upon response
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  
  # needed for var names to select from levels of factors later on
  if (ncol(X) > 26 * 27) stop("Find further column names for X!")
  let <- c(letters, sort(do.call("paste0", expand.grid(letters, letters[1:26]))))
  names(X) <- let[1:ncol(X)]
  
  # factors are coded as dummies which are standardized in cv.glmnet()
  # intercept is not in model.matrix() because its already in cv.glmnet()
  is_fact_var <- sapply(X, is.factor)
  X <- try(model.matrix(~ -1 + ., data = X), silent = FALSE)
  
  # cv.glmnet() calls glmnet(), thus arguments are given to glmnet()
  if (ncol(X) > 8) {
    fitCV <- try(glmnet::cv.glmnet(
      x = X, y = Y, lambda = NULL, type.measure = "deviance",
      nfolds = nfolds, family = family$family, alpha = alpha,
      nlambda = nlambda, keep = T
    ), silent = TRUE)
    # if no variable was selected, penalization might have been too strong, try log(lambda)
    if (all(fitCV$nzero == 0) | all(is.na(fitCV$nzero))) {
      fitCV <- try(glmnet::cv.glmnet(
        x = X, y = Y, lambda = log(fitCV$glmnet.fit$lambda + 1), type.measure = "deviance",
        nfolds = nfolds, family = family$family, alpha = alpha, keep = T
      ), silent = TRUE)
    }
    # if nVar is not available, take the closest to nVar available
    if (all(fitCV$nzero != nVar)) {
      lambda_index_with_nVar <- min(which(abs(fitCV$nzero - nVar) == min(abs(fitCV$nzero - nVar))))
      # nVar is available
    } else if (any(fitCV$nzero == nVar)) {
      lambda_index_with_nVar <- min(which(fitCV$nzero == nVar))
    }
    coefs <- coef(fitCV$glmnet.fit, s = fitCV$glmnet.fit$lambda[lambda_index_with_nVar])
    var_nms <- coefs@Dimnames[[1]]
    
    # Instead of Group Lasso:
    # If any level of a dummy coded factor is selected, the whole factor is selected
    if (any(is_fact_var)) {
      nms_fac <- names(which(is_fact_var))
      is_selected <- coefs[-1] != 0 # drop intercept
      # model.matrix adds numbers to dummy coded factors which we need to get rid of
      var_nms_sel <- gsub("[^::a-z::]", "", var_nms[-1][is_selected])
      sel_fac <- nms_fac[nms_fac %in% var_nms_sel]
      sel_numer <- var_nms_sel[!var_nms_sel %in% sel_fac]
      all_sel_vars <- c(sel_fac, sel_numer)
      whichVariable <- names(is_fact_var) %in% all_sel_vars
    } else {
      # metric variables only
      whichVariable <- coefs[-1] != 0
    }
  } else {
    # no screening
    whichVariable <- rep(TRUE, ncol(X))
  }
  if (nVar != sum(whichVariable)) {
    #cat("- Screening with glmnet:", nVar, "vars were not available, instead", sum(whichVariable), "vars were screened. - \n")
  }
  return(whichVariable)
}

### 4) Pearson Correlation Coef. with cor.test()$p.value

screen.corPearson <- function(Y, X, family, obsWeights, id, method = "pearson", minPvalue = 0.01,
                              minscreen = 3, maxscreen = 8, ...) {
  if (ncol(X) > 8) {
    p_val <- apply(X, 2, function(var) {
      # factors are only selected when no driving metrics seem to be there
      if (length(unique(var)) < 5 | is.factor(var)) {
        return(minPvalue + 1e-4)
      }
      # predictors with zero variance are not meaningful
      if (var(var) == 0) {
        return(1)
      }
      cor.test(var, y = Y, method = "pearson")$p.value
    })
    no_sel <- sum(p_val <= minPvalue)
    if (no_sel > maxscreen) {
      return(rank(p_val, ties.method = "random") <= maxscreen)
    }
    if (no_sel < minscreen) {
      return(rank(p_val, ties.method = "random") <= minscreen)
    }
    return(p_val <= minPvalue)
  }
  rep(TRUE, ncol(X))
}

#### Prediction ####

# 1) multivariate adaptive regression splines with 'earth'

SL.earth2 <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, id = NULL, degree = 2, penalty = 3,
                      nk = max(21, 2 * ncol(X) + 1), pmethod = "backward", nfold = 0,
                      ncross = 1, minspan = 0, endspan = 0, ...) {
  #cat(" - earth2 was started and is making predictions - \n")
  
  # chose family dependent upon response
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  
  earth_time <- system.time({
    SuperLearner:::.SL.require("earth")
    if (family$family == "gaussian") {
      fit.earth <- earth::earth(
        x = X, y = Y, degree = degree,
        nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
        ncross = ncross, minspan = minspan, endspan = endspan
      )
    }
    if (family$family == "binomial" & all(Y %in% c(0, 1))) {
      fit.earth <- earth::earth(
        x = X, y = Y, degree = degree,
        nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
        ncross = ncross, minspan = minspan, endspan = endspan,
        glm = list(family = binomial)
      )
    }
    if (family$family == "binomial" & all(Y %in% c(0, 1)) == FALSE) {
      fit.earth <- earth::earth(
        x = X, y = Y, degree = degree,
        nk = nk, penalty = penalty, pmethod = pmethod, nfold = nfold,
        ncross = ncross, minspan = minspan, endspan = endspan
      )
    }
    pred <- predict(fit.earth, newdata = newX, type = "response")
    fit <- list(object = fit.earth)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.earth")
  })
  #cat("- earth2 was finished lasting:", round(unclass(earth_time)["elapsed"], 2), " - \n")
  return(out)
}

# 2) Random Forest with different numbers of Trees

SL.randomForest_base <- function(Y, X, newX = NULL, family = list(), mtry = ifelse(family$family ==
                                                                                     "gaussian", max(floor(ncol(X) / 3), 1), floor(sqrt(ncol(X)))),
                                 ntree = 100, nodesize = ifelse(family$family == "gaussian",
                                                                5, 1
                                 ), maxnodes = NULL, importance = FALSE, ...) {
  #cat(" - randomForest was started (ntree =", ntree, ") and is making predictions - \n")
  randomForest_time <- system.time({
    SuperLearner:::.SL.require("randomForest")
    
    # avoid infinite search for split points in trees
    if (all(apply(X,2,var) == 0)) {
      fit.rf <- "Empty"
      attr(fit.rf, "class") <- "try-error"
      pred <- rep(mean(Y), nrow(Xnew))
      fit <- list(object = fit.rf)
      #cat("- Failed random forest - \n")
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
        #cat("- Failed random forest - \n")
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
        #cat("- Failed random forest - \n")
      }
      fit <- list(object = fit.rf)
    }
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.randomForest")
  })
  #cat("- randomForest was finished lasting: ", round(unclass(randomForest_time)["elapsed"], 2), " - \n")
  return(out)
}

# Try different Hyperparamters
tuneGrid <- expand.grid(ntree = c(250, 500))
for (i in seq(nrow(tuneGrid))) {
  eval(parse(text = paste0(
    "SL.randomForest_grid", tuneGrid[i, 1],
    "<- function(..., ntree = ", tuneGrid[i, 1], ")
                           {SL.randomForest_base(..., ntree = ntree)}"
  )))
}

# 3) Generalized Boosted Regression with tuning grid
# failes frequently and takes a lot of time

SL.gbm_base <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, gbm.trees = 10,
                        interaction.depth = 1, shrinkage = 0.001, ...) {
  #cat(" - GBM started and is making predictions, interaction depth = ", interaction.depth, " and gbm.trees = ", gbm.trees, " - \n")
  SuperLearner:::.SL.require("gbm")
  gbm.model <- as.formula(paste("Y~", paste(colnames(X), collapse = "+")))
  
  # chose family dependent upon response
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- "binomial"
  } else {
    family$family <- "gaussian"
  }
  
  if (family$family == "gaussian") {
    fit.gbm <- gbm::gbm(
      formula = gbm.model, data = X, distribution = "gaussian",
      n.trees = gbm.trees, interaction.depth = interaction.depth,
      shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, bag.fraction = 0.75,
      weights = obsWeights, verbose = FALSE
    )
  }
  if (family$family == "binomial" & all(Y %in% c(0, 1))) {
    fit.gbm <- gbm::gbm(
      formula = gbm.model, data = X, distribution = "bernoulli",
      n.trees = gbm.trees, interaction.depth = interaction.depth,
      shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, bag.fraction = 0.75,
      weights = obsWeights, verbose = FALSE
    )
  }
  if (family$family == "binomial" & all(Y %in% c(0, 1)) == FALSE) {
    fit.gbm <- gbm::gbm(
      formula = gbm.model, data = X, distribution = "gaussian",
      n.trees = gbm.trees, interaction.depth = interaction.depth,
      shrinkage = shrinkage, cv.folds = 5, keep.data = TRUE, bag.fraction = 0.75,
      weights = obsWeights, verbose = FALSE
    )
  }
  best.iter <- gbm::gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
  pred <- try(predict(fit.gbm, newdata = newX, best.iter, type = "response"), silent = TRUE)
  if (any(class(pred) == "try-error")) {
    #cat("- GBM Unsuccessful! - \n")
    pred <- rep(median(Y), length(Y))
  }
  fit <- list(object = fit.gbm, n.trees = best.iter)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gbm")
  #cat(" - GBM finished - \n")
  return(out)
}

# Try different Hyperparamters
tuneGrid <- expand.grid(gbm.trees = c(10), interaction.depth = c(1, 2))
for (i in seq(nrow(tuneGrid))) {
  eval(parse(text = paste0(
    "SL.gbm_grid", tuneGrid[i, 1], "_interaction.depth", tuneGrid[i, 2],
    "<- function(..., gbm.trees = ", tuneGrid[i, 1], ", interaction.depth = ", tuneGrid[i, 2], ")
                           {SL.gbm_base(..., gbm.trees = gbm.trees, interaction.depth = interaction.depth)}"
  )))
}

# 4) GAM from 'gam' package just slighlty modified for fluent estimation procedure
# no substantial changes

SL.gam2 <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, deg.gam = 2, cts.num = 5,
                    ...) {
  
  # deg.gam == 2 is needed due to frequently occurring convergence failure
  # chose family dependent upon response variable
  Y <- as.vector(as.matrix(Y))
  if (all(Y == 0 | Y == 1)) {
    family$family <- binomial()
  } else {
    family$family <- gaussian() # s#cat() also reasonable but computationally intense
  }
  
  #cat(" - gam::gam was started and is making predictions - \n")
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
  #cat("- gam::gam was finished lasting: ", round(unclass(gam_time)["elapsed"], 2), " - \n")
  return(out)
}

# 6) glm.interaction with informative output

SL.glm.interaction_info <- function(Y, X, newX = NULL, family = list(), obsWeights = NULL, ...) {
  #cat("- GLM Interaction was started and is making predictions - \n")
  glm_time <- system.time({
    
    # chose family dependent upon response variable
    Y <- as.vector(as.matrix(Y))
    if (all(Y == 0 | Y == 1)) {
      family$family <- binomial()
    } else {
      family$family <- gaussian() # s#cat() also reasonable but computationally intense
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
  
  #cat("- GLM Interaction was finished lasting: ", round(unclass(glm_time)["elapsed"], 2), " - \n")
  return(out)
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
  #cat("Subjects following intervention:\n\n")
  print(round(cc_trunc_matrix[1:2,,drop=FALSE],digits=3))
  #cat("\n")
  #cat("\n")
  #cat("% probabilites truncated (among those following abar) :\n\n")
  print(round(cc_trunc_matrix[3,,drop=FALSE],digits=3))
  #cat("\n")
  #cat("\n")
  #cat("Summary of clever covariate :\n\n")
  print(round(cc_trunc_matrix[4:9,,drop=FALSE],digits=3))
  #cat("\n") 
 
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

learner_weights_summary_Q <- function(ltmle_est, mean_tf = TRUE){
  
  # takes ltmle output and extracts the coefficient/weight of each learner in every 
  # Q-Model. It returns a vector, where each entry corresponds to the mean of this learner's weights across
  # all treatment and all control models (here: 2*11 points in time). Controls and Treatment weights for the same point
  # in time might be equal and merged anyway but this has no further consequence since the relative frequency is of interest.
  
  ltmle_est$fit$Q <- c(ltmle_est$fit$Q[[1]],ltmle_est$fit$Q[[2]]) # treatment and control
  bad_preds <- sapply(ltmle_est$fit$Q, function(x) is.character(x[[1]]))
  if(any(bad_preds)) {
    ltmle_est$fit$Q[bad_preds] <- NULL # no estimation occured
    message("Some Q-Learner had constant Ys and hence no estimation occured.")
  } 
  learner_weights <- sapply(ltmle_est$fit$Q, function(x) {
    try(return(x[,"Coef"]), silent = TRUE)
    x$coef
  })
  if(mean_tf == FALSE) return(learner_weights)
  mean_weights <- rowMeans(learner_weights)
  return(mean_weights)
}

learner_weights_summary_g <- function(ltmle_est, mean_tf = TRUE){
  
  # takes ltmle output and extracts the coefficient/weight of each learner in every 
  # g-Model. It returns a vector, where each entry corresponds to the mean of this learner's weights across
  # all treatment and all control models (here: 2*11 points in time). Controls and Treatment weights for the same point
  # in time might be equal and merged anyway but this has no further consequence since the relative frequency is of interest.
  
  ltmle_est$fit$g <- c(ltmle_est$fit$g[[1]],ltmle_est$fit$g[[2]]) # treatment and control
  bad_preds <- sapply(ltmle_est$fit$g, function(x) is.character(x[[1]]))
  if(any(bad_preds)) {
    ltmle_est$fit$g[bad_preds] <- NULL # no estimation occured
    message("Some g-Learner had constant Ys and hence no estimation occured.")
  } 
  learner_weights <- sapply(ltmle_est$fit$g, function(x) {
    try(return(x[,"Coef"]), silent = TRUE)
    x$coef
  })
  if(mean_tf == FALSE) return(learner_weights)
  mean_weights <- rowMeans(learner_weights)
  return(mean_weights)
}
