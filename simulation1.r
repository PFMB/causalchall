setwd("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge/simulations")
directory <- getwd()

library(simcausal)
library(SuperLearner)
library(ltmle)
library(ggplot2)

set.seed(241280)

###########################
# Data generating process #
###########################


# definition: left-truncated normal distribution
rnorm_trunc <- function(n, mean, sd, minval = 0, maxval = 10000,
                        min.low = 0, max.low = 50, min.high = 5000, max.high = 10000)
{                                                                                      
  out <- rnorm(n = n, mean = mean, sd = sd)
  minval <- minval[1]; min1 <- min.low[1]; max1 <- max.low[1]
  maxval <- maxval[1]; min2 <- min.high[1]; max2 <- max.high[1]
  leq.zero <- length(out[out <= minval])
  geq.max <- length(out[out >= maxval])
  out[out <= minval] <- runif(n = leq.zero, min = min1, max = max1)
  out[out >= maxval] <- runif(n = geq.max, min = min2, max = max2)
  out
}

#define the number of time points
t.end <- 12

#initialize the dag
D <- DAG.empty()

#everything at baseline (t = 0)
D.base <- D +
  node("V1",                                      #region -> 0 = west, 1 = southern 
       t = 0,
       distr = "rbern",
       prob = 4392/5826) +
  node("V2",                                      #sex -> 0 = female, 1 = male 
       t = 0,
       distr = "rbern",
       prob = ifelse(V1[0] == 1, 2222/4392, 758/1434)) +
  node("V3",                                      #age 
       t = 0,
       distr = "runif",
       min = 1,
       max = 5) +
  node("L1",                                      #cd4-count 
       t = 0,
       distr = "rnorm_trunc",
       mean = ifelse(V1[0] == 1, 650, 720),
       sd = ifelse(V1[0] == 1, 350, 400),
       minval = 0, maxval = 10000,
       min.low = 0, max.low = 50, min.high = 5000, max.high = 10000) +
  node("L1scaled",                                #auxilliary-variable 
       t = 0,
       distr = "rnorm",
       mean = (L1[0]-671.7468)/(10*352.2788)+1,
       sd = 0) +
  node("L2",                                      #cd4% 
       t = 0,
       distr = "rnorm_trunc",
       mean = .16 + .05 * (L1[0] - 650)/650,
       sd = .07,
       minval = .06, maxval = .8,
       min.low = .03, max.low = .09, min.high = .7, max.high = .8) +
  node("L2scaled",                                #auxilliary-variable 
       t = 0,
       distr = "rnorm",
       mean = (L2[0]-0.1648594)/(10*0.06980332)+1,
       sd = 0) +
  node("L3",                                      #waz 
       t = 0,
       distr = "rnorm_trunc",
       mean = ifelse(V1[0] == 1, - 1.65 + .1 * V3[0] + .05 * (L1[0] - 650)/650 + .05 * (L2[0] - 16)/16,
                     - 2.05 + .1 * V3[0] + .05 * (L1[0] - 650)/650 + .05 * (L2[0] - 16)/16),
       sd = 1,
       minval = -5, maxval = 5,
       min.low = -10, max.low = -3, min.high = 3, max.high = 10) +
  node("A",                                       # ART
       t = 0,
       distr = "rbern",
       prob = 0) +
  node("C",                                      # censoring (death?)
       t = 0,
       distr = "rbern",
       prob = 0,
       EFU = F) +
  node("Y",                                      # HAZ
       t = 0,
       distr = "rnorm_trunc",
       mean = -2.6 + .1 * I(V3[0] > 2) + .3 * I(V1[0] == 0) + (L3[0] + 1.45),
       sd = 1.1,
       minval = -5, maxval = 5,
       min.low = -10, max.low = -3, min.high = 3, max.high = 10)

#time-dependent variables at later time-points
D <- D.base +
  node("L1",                                      #cd4-count
       t = 1:4,
       distr = "rnorm_trunc",
       mean = 13*log(t * (1034-662)/8) + L1[t-1] + 2 * L2[t-1] + 2 * L3[t-1] + 2.5 * A[t-1],
       sd = 50,
       minval = 0, maxval = 10000,
       min.low = 0, max.low = 50, min.high = 5000, max.high = 10000) +
  node("L1",                                      #cd4-count
       t = 5:8,
       distr = "rnorm_trunc",
       mean = 4*log(t * (1034-662)/8) + L1[t-1] + 2 * L2[t-1] + 2 * L3[t-1] + 2.5 * A[t-1],
       sd = 50,
       minval = 0, maxval = 10000,
       min.low = 0, max.low = 50, min.high = 5000, max.high = 10000) +
  node("L1",                                      #cd4-count
       t = 9:t.end,
       distr = "rnorm_trunc",
       mean = L1[t-1] + 2 * L2[t-1] + 2 * L3[t-1] + 2.5 * A[t-1],
       sd = 50,
       minval = 0, maxval = 10000,
       min.low = 0, max.low = 50, min.high = 5000, max.high = 10000) +
  node("L2",                                      #cd4%
       t = 1:t.end,
       distr = "rnorm_trunc",
       mean = L2[t-1] + .0003 * (L1[t]-L1[t-1]) + .0005 * (L3[t-1]) + .0005 * A[t-1] * L1scaled[0],
       sd = .02,
       minval = .06, maxval = .8,
       min.low = .03, max.low = .09, min.high = .7, max.high = .8) +
  node("L3",                                      #waz
       t = 1:t.end,
       distr = "rnorm_trunc",
       mean = L3[t-1] + .0017 * (L1[t] - L1[t-1]) + .2 * (L2[t] - L2[t-1]) + .005 * A[t-1] * L2scaled[0],
       sd = .5,
       minval = -5, maxval = 5,
       min.low = -10, max.low = -3, min.high = 3, max.high = 10) +
  node("A",                                       #art
       t = 1:t.end,
       distr = "rbern",
       prob = ifelse(A[t-1] == 1, 1, plogis(-2.4 + .015 * (750 - L1[t]) + 5 * (.2 - L2[t]) - .8 * L3[t] + .8 * t))) +
  node("C",                                      # censoring
       t = 1:t.end,
       distr = "rbern",
       prob = plogis(-6 + .01 * (750 - L1[t]) + 1 * (.2 - L2[t]) - .65 * L3[t] - A[t]),
       EFU = F)

D <- D +
  node("Y",                                      #haz
       t = 1:t.end,
       distr = "rnorm_trunc",
       mean = Y[t-1] +
         .00005 * (L1[t] - L1[t-1]) - 0.000001 * ((L1[t] - L1[t-1])*sqrt(L1scaled[0]))^2 +
         .01 * (L2[t] - L2[t-1]) - .0001 * ((L2[t] - L2[t-1])*sqrt(L2scaled[0]))^2 +
         .07 * ((L3[t]-L3[t-1])*(L3[0]+1.5135)) - .001 * ((L3[t]-L3[t-1])*(L3[0]+1.5135))^2 +
         .005 * A[t] + .075 * A[t-1] + .05 * A[t]*A[t-1] ,
       sd = .01,
       minval = -5, maxval = 5,
       min.low = -10, max.low = -3, min.high = 3, max.high = 10)




#specify (dynamic) interventions
Dset <- set.DAG(D)
int.a <- c(node("A", t = 1:t.end, distr = "rbern",
              prob = ifelse(A[t-1] == 1, 1, ifelse(L1[t] < theta1 | L2[t] < theta2 | L3[t] < theta3, 1, 0))),
           node("C", t = 1:t.end, distr = "rbern", prob = 0))

D.dyn1 <- Dset + action("A_th1", nodes = int.a, theta1 = 10000, theta2 = .99, theta3 = 10) # treat all (A=1)
D.dyn4 <- Dset + action("A_th4", nodes = int.a, theta1 = -1, theta2 = -.1, theta3 = -11) # treat never (A=0)

# Intervention on the DAG given the specified interventions
dat1 <- simcausal::sim(DAG = D.dyn1, actions = "A_th1", n = 1000000, rndseed = 7693)
dat2 <- simcausal::sim(DAG = D.dyn4, actions = "A_th4", n = 1000000, rndseed = 7693)
dat3 <- simcausal::sim(DAG = Dset, n = 1000000, rndseed = 7693)

# true ATTs
true_ATE_1 <- mean(c(dat1[["A_th1"]])$Y_1) - mean(c(dat2[["A_th4"]])$Y_1) 
true_ATT_1 <- mean(c(dat1[["A_th1"]])$Y_1[dat3$A_1==1]) - mean(c(dat2[["A_th4"]])$Y_1[dat3$A_1==1]) 

true_ATE_2 <- mean(c(dat1[["A_th1"]])$Y_2) - mean(c(dat2[["A_th4"]])$Y_2) 
true_ATT_2 <- mean(c(dat1[["A_th1"]])$Y_2[dat3$A_1==1 & dat3$A_2==1]) - mean(c(dat2[["A_th4"]])$Y_2[dat3$A_1==1 & dat3$A_2==1]) 

#############
# Estimates #
#############

####### GFORMULA ##########
library(CICI)

mydata <- simcausal::sim(DAG = Dset, n = 1000, rndseed = 7693)[,c(2,3,4,5,7,9,12,13,14,15,16,18,19,20,21:22,24)]
treated_1 <- as.numeric(mydata$A_1==1)
treated_2 <- as.numeric(mydata$A_1==1 & mydata$A_1==1)
mydata2 <- cbind(treated_1,treated_2,mydata)

### ATE
estimates <- gformula(X=mydata,
                      Lnodes  = c("L1_2", "L2_2", "L3_2"),
                      Ynodes  = c("Y_1","Y_2"),
                      Anodes  = c("A_1","A_2"),
                      abar=c(0,1)
)
estimates

check.models <- make.model.formulas(X.m=mydata, Ynodes.m=c("Y_1","Y_2"),
                                    Lnodes.m= c("L1_2", "L2_2", "L3_2"),
                                    Cnodes.m=NULL, Anodes.m=c("A_1","A_2"), evaluate=F)
glmnet.formulas <-  model.formulas.update(check.models$model.names, mydata, with.s=T,pw=T)

estimates2 <- gformula(X=mydata2,
                       Lnodes  = c("L1_2", "L2_2", "L3_2"),
                       Ynodes  = c("Y_1","Y_2"),
                       Anodes  = c("A_1","A_2"),
                       Yform=glmnet.formulas$Ynames, Lform=glmnet.formulas$Lnames,
                       calc.support=F, ret=T,
                       abar=seq(0,1)
)

estimates2$results$psi[2]-estimates2$results$psi[1]
estimates2$results$psi[4]-estimates2$results$psi[3]

estimates3 <- gformula(X=mydata,
                       Lnodes  = c("L1_2", "L2_2", "L3_2"),
                       Ynodes  = c("Y_1","Y_2"),
                       Anodes  = c("A_1","A_2"),
                       Yform=glmnet.formulas$Ynames, Lform=glmnet.formulas$Lnames,
                       Aform=glmnet.formulas$Anames,
                       abar="natural", B=100, ncores=7
)
plot(estimates3)
estimates3

# ATE
estimates2$results$psi[2]-estimates2$results$psi[1]
estimates2$results$psi[4]-estimates2$results$psi[3]


### ATT
estimates2_1 <- custom.measure(estimates2, fun=mean, cond="treated_1==1")
estimates2_2 <- custom.measure(estimates2, fun=mean, cond="treated_2==1")

estimates2_1$results$psi[2]-estimates2_1$results$psi[1]
estimates2_2$results$psi[4]-estimates2_2$results$psi[3]


######## LTMLE MSM ########

# simple library for testing
mylibrary <-  NULL
# slightly advanced library
source('own_learners.r')
mylibrary2 <- list(Q=list("SL.mean","SL.glm", "SL.bayesglm", "SL.stepAIC",
                          c("SL.bayesglm","screen.cramersv"), c("SL.gam","screen.cramersv"), c("SL.step.interaction","screen.cramersv"), 
                          c("SL.glm","screen.glmnet3"), 
                          c("SL.glm","screen.glmnet_nVar"),c("SL.gam","screen.glmnet_nVar"),c("SL.step.interaction","screen.glmnet_nVar")
),
g=list("SL.mean","SL.glm", "SL.bayesglm", "SL.stepAIC","SL.knn",
       c("SL.bayesglm","screen.cramersv"), c("SL.gam","screen.cramersv"), c("SL.step.interaction","screen.cramersv"), 
       c("SL.glm","screen.glmnet3"), 
       c("SL.glm","screen.glmnet_nVar"),c("SL.gam","screen.glmnet_nVar"),c("SL.step.interaction","screen.glmnet_nVar")
))

#                        
regimesList <- list(function(row) c(1,1),
                    function(row) c(0,0)
                    
)

my.sum.measures <- array(c(c(1,0),c(1,1),
                           c(1,0),c(2,2))
                         ,dim=c(2,2,2),dimnames=list(NULL,c("A","time"),NULL))



estimates4 <- ltmleMSM(mydata2,
                       Lnodes  = c("L1_2", "L2_2", "L3_2"),
                       Ynodes  = c("Y_1","Y_2"),
                       Anodes  = c("A_1","A_2"), survivalOutcome=F, Yrange=c(-10,3),
                       Qform=NULL, gform=NULL, stratify=FALSE,
                       SL.library=mylibrary, estimate.time=F, variance.method="ic", gcomp=F,
                       final.Ynodes=c("Y_1","Y_2"),
                       regimes=regimesList,
                       working.msm="Y ~ time*A",
                       summary.measures=my.sum.measures 
)
summary(estimates4)


ATE_1 <- (invlogit(t(c(1,1,1,1))%*%estimates4$beta)*13-10) - (invlogit(t(c(1,1,0,0))%*%estimates4$beta)*13-10)
ATE_2 <- (invlogit(t(c(1,2,1,2))%*%estimates4$beta)*13-10) - (invlogit(t(c(1,2,0,0))%*%estimates4$beta)*13-10)
ATE_1
ATE_2


# ATT
estimates5 <- ltmleMSM(mydata2,
                       Lnodes  = c("L1_2", "L2_2", "L3_2"),
                       Ynodes  = c("Y_1","Y_2"),
                       Anodes  = c("A_1","A_2"), survivalOutcome=F, Yrange=c(-10,3),
                       Qform=NULL, gform=NULL, stratify=FALSE,
                       SL.library=mylibrary, estimate.time=F, variance.method="ic", gcomp=F,
                       final.Ynodes=c("Y_1","Y_2"),
                       regimes=regimesList,
                       working.msm="Y ~ time*A*treated_2",
                       summary.measures=my.sum.measures 
)
summary(estimates5)

ATT_2 <- (invlogit(t(c(1,2,1,1,2,2,1,2))%*%estimates5$beta)*13-10) - (invlogit(t(c(1,2,0,1,0,2,0,0))%*%estimates5$beta)*13-10)
ATT_2

####### SUMMARY

truth_ATE <- c(mean(c(dat1[["A_th1"]])$Y_1), mean(c(dat2[["A_th4"]])$Y_1) , mean(c(dat1[["A_th1"]])$Y_2) , mean(c(dat2[["A_th4"]])$Y_2) )
truth_ATT <- c(mean(c(dat1[["A_th1"]])$Y_1[dat3$A_1==1]) , mean(c(dat2[["A_th4"]])$Y_1[dat3$A_1==1]), 
               mean(c(dat1[["A_th1"]])$Y_2[dat3$A_1==1 & dat3$A_2==1]) , mean(c(dat2[["A_th4"]])$Y_2[dat3$A_1==1 & dat3$A_2==1]) 
               )
  
gcomp_ATE <- estimates2$results$psi[c(2,1,4,3)]
gcomp_ATT <- c(estimates2_1$results$psi[2],estimates2_1$results$psi[1],estimates2_2$results$psi[4],estimates2_2$results$psi[3])

ltmle_ATE <- c((invlogit(t(c(1,1,1,1))%*%estimates4$beta)*13-10) , (invlogit(t(c(1,1,0,0))%*%estimates4$beta)*13-10),
               (invlogit(t(c(1,2,1,2))%*%estimates4$beta)*13-10) , (invlogit(t(c(1,2,0,0))%*%estimates4$beta)*13-10)
               )
ltmle_ATT <- c(NA,NA,(invlogit(t(c(1,2,1,1,2,2,1,2))%*%estimates5$beta)*13-10) , (invlogit(t(c(1,2,0,1,0,2,0,0))%*%estimates5$beta)*13-10))

cbind(truth_ATE,gcomp_ATE,ltmle_ATE)
cbind(truth_ATT,gcomp_ATT,ltmle_ATT)
