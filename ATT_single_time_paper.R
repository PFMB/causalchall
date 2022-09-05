#
source("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge/simulations/ltmleMSM_CI.R")

#
library(msm)  # for delta method (used in msm.se())
library(ltmle)

set.seed(241280)

GenerateData <- function(n, abar = NULL) {
  W <- rnorm(n)
  A <- rexpit(W)
  if (is.null(abar)) {
    Y <- rexpit(W + A)
  } else {
    Y <- rexpit(W + abar)
  }
  
  if (is.null(abar)) {
    # observed data
    return(data.frame(W, A, Y))
  } else {
    # counterfactual mean
    return(mean(Y[A == 1])) #among treated
    # return(mean(Y))       #among all
  }
}

rexpit <- ltmle:::rexpit
invlogit <- function (x) {  1/(1 + exp(-x))}
psi0 <- GenerateData(1e6, abar = 1)
print(psi0) # true ATT

# setup for ltmleMSM

regimesList <- list(function(row) c(1),
                    function(row) c(0))

my.sum.measures <- array(c(c(1,0),c(1,1))
                         ,dim=c(2,2,1),dimnames=list(NULL,c("Int","time"),NULL))

niter <- 10000 # number of simulation runs
n <- 1000      # sample size  
est <-  rep(NA_real_, niter); est2 <- rep(NA_real_, niter)
coverage <- coverage2 <- matrix(NA,nrow=niter,ncol=2)

for (i in 1:niter) {
  dt <- GenerateData(n)
  r <- ltmle(dt, Anodes = "A", Ynodes = "Y", estimate.time = F, abar = 1)
  est[i] <- mean(r$Qstar[dt$A == 1]) # point estimate through ltmle
  # data setup for ltmle MSM: add trt A additionally as `baseline indicator Z'
  dt2 <- dt; dt2$Z <- dt$A; dt2 <- dt2[,c("Z","W","A","Y")]
  r2 <- ltmleMSM(dt2, Anodes = "A", Ynodes = "Y",
                 final.Ynodes=c("Y"),
                 Qform = c(Y="Q.kplus1 ~ W + A"),  gform = "A ~ W",
                 regimes=regimesList,
                 working.msm="Y ~ Int * Z", # `Int' -> see summary measures
                 summary.measures=my.sum.measures,
                 variance.method="tmle"
  )
  est2[i] <- invlogit(c(1,1,1,1)%*%r2$beta) # point estimate through ltmleMSM
  # coverage / 90% CI as in data challenge
  se1 <- msm.se(r2,b1=c(1,1,1,1),b2=NULL, cov="IC")
  se2 <- msm.se(r2,b1=c(1,1,1,1),b2=NULL, cov="not IC")
  coverage[i,1] <- invlogit(c(1,1,1,1)%*%r2$beta) - ((qnorm(0.95)*se1/sqrt(n) ))
  coverage[i,2] <- invlogit(c(1,1,1,1)%*%r2$beta) + ((qnorm(0.95)*se1/sqrt(n) ))
  coverage2[i,1] <- invlogit(c(1,1,1,1)%*%r2$beta) - ((qnorm(0.95)*se2/sqrt(n) ))
  coverage2[i,2] <- invlogit(c(1,1,1,1)%*%r2$beta) + ((qnorm(0.95)*se2/sqrt(n) ))
  # 
}

# give ltmle and ltmleMSM same results? -> YES
round(summary(est-est2),digits=8)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      0       0       0       0       0       0 

# approximately unbiased? -> YES
mean(psi0-est)
mean(psi0-est2)
# 0.0003247021
# 0.0003247022

# coverage -> conservative; better non-IC based
mean(as.numeric(coverage[,1] <= psi0 & psi0 <= coverage[,2]))    # coverage with IC based
mean(as.numeric(coverage2[,1] <= psi0 & psi0 <= coverage2[,2]))  # coverage tmle based
# [1] 0.9996
# [1] 0.9673

