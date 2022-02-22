############
# LEARNERS #
############

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
