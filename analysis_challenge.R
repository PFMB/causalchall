#setwd("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")
#setwd("C:/Users/ua341au/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")
setwd("/Users/flipst3r/RStHomeDir/GitHub/causalchall")

#
library(parallel)
#library(doParallel)
#library(foreach)
library(msm)
# index
#index <- 1:14 #1:3400
#index_str <- formatC(index, width = 4, format = "d", flag = "0")
#
source('own_learners.r')
source('ltmleMSM_CI.r')
#
#ncores<-7
#cl <- parallel::makeCluster(ncores); doParallel::registerDoParallel(cl)
#exp.var <- c("screen.cramersv","screen.glmnet3","screen.glmnet_nVar")
#prog=T
#if(prog==TRUE){write(matrix("started with analysis..."),file=paste0(getwd(),"/progress.txt",sep=""))}
#

#analysis <- foreach(i = index, .export=exp.var, .errorhandling="pass") %dopar% {
#
#if(prog==TRUE){write(matrix(paste("started with analyzing dataset number...",i,"\n")),file=paste0(getwd(),"/progress.txt",sep=""),append=TRUE)}
#

mydata <- paste0("data/track2/practice/acic_practice_0005.csv")
mydata2 <- paste0("data/track2/practice_year/acic_practice_year_0005.csv")
# read in and merge data
d1 <- read.csv(mydata)
d2 <- read.csv(mydata2)
d3 <- merge(d1,d2)

# data management
d3$A <- d3$Z*d3$post
d3 <- d3[,c("id.practice","year","Z","X1","X2","X3","X4","X5","X6","X7","X8","X9","A","n.patients","V1_avg",
            "V2_avg","V3_avg","V4_avg","V5_A_avg","V5_B_avg","V5_C_avg","Y")]
dwide <- reshape(d3,v.names=c("A","n.patients","V1_avg","V2_avg","V3_avg","V4_avg","V5_A_avg","V5_B_avg","V5_C_avg","Y"),
                 idvar = "id.practice",timevar="year",direction="wide")
dwide <- dwide[,-c(1,grep("A.1",colnames(dwide)),grep("A.2",colnames(dwide)))]
dwide$X4 <- as.factor(dwide$X4)
dwide$X2 <- as.factor(dwide$X2)
# descriptives
#plot(density(sqrt(dwide$n.patients.2)))
### TO DO Daten anschauen und verstehen!
### TO DO naiver vergleich mit Mittelwerten

##############
# LTMLE MSM  #
##############
library(ltmle)
library(arm) # f?r invlogit und bayesglm
#
source('own_learners.r')
#
# mylibrary2 <- list(Q=list("SL.mean","SL.glm", "SL.bayesglm", "SL.stepAIC",
#                           c("SL.bayesglm","screen.cramersv"), c("SL.gam","screen.cramersv"), c("SL.step.interaction","screen.cramersv"), 
#                           c("SL.glm","screen.glmnet3"), 
#                           c("SL.glm","screen.glmnet_nVar"),c("SL.gam","screen.glmnet_nVar"),c("SL.step.interaction","screen.glmnet_nVar")
# ),
# g=list("SL.mean","SL.glm", "SL.bayesglm", "SL.stepAIC","SL.knn",
#        c("SL.bayesglm","screen.cramersv"), c("SL.gam","screen.cramersv"), c("SL.step.interaction","screen.cramersv"), 
#        c("SL.glm","screen.glmnet3"), 
#        c("SL.glm","screen.glmnet_nVar"),c("SL.gam","screen.glmnet_nVar"),c("SL.step.interaction","screen.glmnet_nVar")
# ))

# mylibrary2 <- list(Q=list("SL.mean","SL.glm", "SL.bayesglm", "SL.stepAIC"),
# g=list("SL.mean","SL.glm", "SL.bayesglm", "SL.stepAIC"))

mylibrary2 <- list(Q=list("SL.mean","SL.NN_base"),
g=list("SL.mean","SL.NN_base"))


###################################################################################################################

#                        
regimesList <- list(function(row) c(1,1),
                    function(row) c(0,0)

)

my.sum.measures <- array(c(c(1,0),c(1,1),
                           c(1,0),c(2,2))
                         ,dim=c(2,2,2),dimnames=list(NULL,c("A","time"),NULL))



m_1 <- try(ltmleMSM(dwide,
                        Anodes=c("A.3","A.4"),
                        Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                                "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                                "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                                "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                                ),
                        Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                        Qform=NULL, gform=NULL, stratify=FALSE,
                        SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                        final.Ynodes=c("Y.3","Y.4"),
                        regimes=regimesList,
                        working.msm="Y ~ A * Z",
                        summary.measures=my.sum.measures,
                        observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
))
summary(m_1)
#cc_trunc(m_1) # correct?


m_2 <- try(ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*time*Z",
                summary.measures=my.sum.measures,
                observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
))
summary(m_2)

m_3 <- try(ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X1",
                summary.measures=my.sum.measures,
                observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2 
))
summary(m_3)

m_4 <- try(ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X2",
                summary.measures=my.sum.measures,
                observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
))
summary(m_4)


m_5 <- try(ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X3",
                summary.measures=my.sum.measures,
                observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2 
))
summary(m_5)

m_6 <- try(ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X4",
                summary.measures=my.sum.measures,
                observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2 
))
summary(m_6)


m_7 <- try(ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X5",
                summary.measures=my.sum.measures ,
                observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
))
summary(m_7)

#### ESTIMANDS
a<-attr(m_1$transformOutcome,"Yrange")[1]
b<-attr(m_1$transformOutcome,"Yrange")[2] 
shrink <- 1
#
SATT <- (invlogit(t(c(1,1,1,1))%*%m_1$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0))%*%m_1$beta)*(b-a)+a)
SATT

se_SATT <- msm.se(m_1,b1=c(1,1,1,1),b2=c(1,0,1,0))
SATT_lower <- SATT - qnorm(0.95)*(se_SATT/sqrt(dim(dwide)[1]))
SATT_upper <- SATT + qnorm(0.95)*(se_SATT/sqrt(dim(dwide)[1]))
SATT_lower
SATT_upper
# Bootstrap Resultate
#       5%      95% 
# 10.56665 47.38844 

#
SATT_year_3 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_2$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_2$beta)*(b-a)+a)
SATT_year_4 <- (invlogit(t(c(1,1,2,1,2,1,2,2))%*%m_2$beta)*(b-a)+a) - (invlogit(t(c(1,0,2,1,0,0,2,0))%*%m_2$beta)*(b-a)+a)
SATT_year_3
SATT_year_4

se_SATT_year_3 <- msm.se(m_2,b1=c(1,1,1,1,1,1,1,1),b2=c(1,0,1,1,0,0,1,0))
se_SATT_year_4 <- msm.se(m_2,b1=c(1,1,2,1,2,1,2,2),b2=c(1,0,2,1,0,0,2,0))
SATT_year_3_lower <- SATT_year_3 - qnorm(0.95)*(se_SATT_year_3/sqrt(dim(dwide)[1]))
SATT_year_3_upper <- SATT_year_3 + qnorm(0.95)*(se_SATT_year_3/sqrt(dim(dwide)[1]))
SATT_year_4_lower <- SATT_year_4 - qnorm(0.95)*(se_SATT_year_4/sqrt(dim(dwide)[1]))
SATT_year_4_upper <- SATT_year_4 + qnorm(0.95)*(se_SATT_year_4/sqrt(dim(dwide)[1]))

#
SATT_X1_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_3$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_3$beta)*(b-a)+a)
SATT_X1_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_3$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_3$beta)*(b-a)+a)
SATT_X1_0
SATT_X1_1

se_SATT_X1_1 <- msm.se(m_3,b1=c(1,1,1,1,1,1,1,1),b2=c(1,0,1,1,0,0,1,0))
se_SATT_X1_0 <- msm.se(m_3,b1=c(1,1,1,0,1,0,0,0),b2=c(1,0,1,0,0,0,0,0))
SATT_X1_1_lower <- SATT_X1_1 - qnorm(0.95)*(se_SATT_X1_1/sqrt(dim(dwide)[1]))
SATT_X1_1_upper <- SATT_X1_1 + qnorm(0.95)*(se_SATT_X1_1/sqrt(dim(dwide)[1]))
SATT_X1_0_lower <- SATT_X1_0 - qnorm(0.95)*(se_SATT_X1_0/sqrt(dim(dwide)[1]))
SATT_X1_0_upper <- SATT_X1_0 + qnorm(0.95)*(se_SATT_X1_0/sqrt(dim(dwide)[1]))
#

SATT_X2_A <- (invlogit(t(c(1,1,1,0,0,1,0,0,0,0,0,0))%*%m_4$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0,0,0,0,0))%*%m_4$beta)*(b-a)+a)
SATT_X2_B <- (invlogit(t(c(1,1,1,1,0,1,1,0,1,0,1,0))%*%m_4$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,0,0,1,0,0,0))%*%m_4$beta)*(b-a)+a)
SATT_X2_C <- (invlogit(t(c(1,1,1,0,1,1,0,1,0,1,0,1))%*%m_4$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,1,0,0,0,0,1,0,0))%*%m_4$beta)*(b-a)+a)
SATT_X2_A
SATT_X2_B
SATT_X2_C

se_SATT_X2_A <- msm.se(m_4,b1=c(1,1,1,0,0,1,0,0,0,0,0,0),b2=c(1,0,1,0,0,0,0,0,0,0,0,0))
se_SATT_X2_B <- msm.se(m_4,b1=c(1,1,1,1,0,1,1,0,1,0,1,0),b2=c(1,0,1,1,0,0,0,0,1,0,0,0))
se_SATT_X2_C <- msm.se(m_4,b1=c(1,1,1,0,1,1,0,1,0,1,0,1),b2=c(1,0,1,0,1,0,0,0,0,1,0,0))
SATT_X2_A_lower <- SATT_X2_A - qnorm(0.95)*(se_SATT_X2_A/sqrt(dim(dwide)[1]))
SATT_X2_A_upper <- SATT_X2_A + qnorm(0.95)*(se_SATT_X2_A/sqrt(dim(dwide)[1]))
SATT_X2_B_lower <- SATT_X2_B - qnorm(0.95)*(se_SATT_X2_B/sqrt(dim(dwide)[1]))
SATT_X2_B_upper <- SATT_X2_B + qnorm(0.95)*(se_SATT_X2_B/sqrt(dim(dwide)[1]))
SATT_X2_C_lower <- SATT_X2_C - qnorm(0.95)*(se_SATT_X2_C/sqrt(dim(dwide)[1]))
SATT_X2_C_upper <- SATT_X2_C + qnorm(0.95)*(se_SATT_X2_C/sqrt(dim(dwide)[1]))
#

SATT_X3_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_5$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_5$beta)*(b-a)+a)
SATT_X3_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_5$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_5$beta)*(b-a)+a)
SATT_X3_0 
SATT_X3_1

se_SATT_X3_1 <- msm.se(m_5,b1=c(1,1,1,1,1,1,1,1),b2=c(1,0,1,1,0,0,1,0))
se_SATT_X3_0 <- msm.se(m_5,b1=c(1,1,1,0,1,0,0,0),b2=c(1,0,1,0,0,0,0,0))
SATT_X3_1_lower <- SATT_X3_1 - qnorm(0.95)*(se_SATT_X3_1/sqrt(dim(dwide)[1]))
SATT_X3_1_upper <- SATT_X3_1 + qnorm(0.95)*(se_SATT_X3_1/sqrt(dim(dwide)[1]))
SATT_X3_0_lower <- SATT_X3_0 - qnorm(0.95)*(se_SATT_X3_0/sqrt(dim(dwide)[1]))
SATT_X3_0_upper <- SATT_X3_0 + qnorm(0.95)*(se_SATT_X3_0/sqrt(dim(dwide)[1]))

#
SATT_X4_A <- (invlogit(t(c(1,1,1,0,0,1,0,0,0,0,0,0))%*%m_6$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0,0,0,0,0))%*%m_6$beta)*(b-a)+a)
SATT_X4_B <- (invlogit(t(c(1,1,1,1,0,1,1,0,1,0,1,0))%*%m_6$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,0,0,1,0,0,0))%*%m_6$beta)*(b-a)+a)
SATT_X4_C <- (invlogit(t(c(1,1,1,0,1,1,0,1,0,1,0,1))%*%m_6$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,1,0,0,0,0,1,0,0))%*%m_6$beta)*(b-a)+a)
SATT_X4_A
SATT_X4_B
SATT_X4_C

se_SATT_X4_A <- msm.se(m_6,b1=c(1,1,1,0,0,1,0,0,0,0,0,0),b2=c(1,0,1,0,0,0,0,0,0,0,0,0))
se_SATT_X4_B <- msm.se(m_6,b1=c(1,1,1,1,0,1,1,0,1,0,1,0),b2=c(1,0,1,1,0,0,0,0,1,0,0,0))
se_SATT_X4_C <- msm.se(m_6,b1=c(1,1,1,0,1,1,0,1,0,1,0,1),b2=c(1,0,1,0,1,0,0,0,0,1,0,0))
SATT_X4_A_lower <- SATT_X4_A - qnorm(0.95)*(se_SATT_X4_A/sqrt(dim(dwide)[1]))
SATT_X4_A_upper <- SATT_X4_A + qnorm(0.95)*(se_SATT_X4_A/sqrt(dim(dwide)[1]))
SATT_X4_B_lower <- SATT_X4_B - qnorm(0.95)*(se_SATT_X4_B/sqrt(dim(dwide)[1]))
SATT_X4_B_upper <- SATT_X4_B + qnorm(0.95)*(se_SATT_X4_B/sqrt(dim(dwide)[1]))
SATT_X4_C_lower <- SATT_X4_C - qnorm(0.95)*(se_SATT_X4_C/sqrt(dim(dwide)[1]))
SATT_X4_C_upper <- SATT_X4_C + qnorm(0.95)*(se_SATT_X4_C/sqrt(dim(dwide)[1]))
#

SATT_X5_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_7$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_7$beta)*(b-a)+a)
SATT_X5_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_7$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_7$beta)*(b-a)+a)
SATT_X5_0 
SATT_X5_1

se_SATT_X5_1 <- msm.se(m_7,b1=c(1,1,1,1,1,1,1,1),b2=c(1,0,1,1,0,0,1,0))
se_SATT_X5_0 <- msm.se(m_7,b1=c(1,1,1,0,1,0,0,0),b2=c(1,0,1,0,0,0,0,0))
SATT_X5_1_lower <- SATT_X5_1 - qnorm(0.95)*(se_SATT_X5_1/sqrt(dim(dwide)[1]))
SATT_X5_1_upper <- SATT_X5_1 + qnorm(0.95)*(se_SATT_X5_1/sqrt(dim(dwide)[1]))
SATT_X5_0_lower <- SATT_X5_0 - qnorm(0.95)*(se_SATT_X5_0/sqrt(dim(dwide)[1]))
SATT_X5_0_upper <- SATT_X5_0 + qnorm(0.95)*(se_SATT_X5_0/sqrt(dim(dwide)[1]))


# # LTMLE plain
# m_1_0 <- try(ltmle(dwide,
#                     Anodes=c("A.3","A.4"),
#                     Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3",
#                              "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
#                              "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4",
#                              "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
#                     ),
#                     Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
#                     Qform=NULL, gform=NULL, stratify=FALSE,
#                     SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
#                     abar = c(0,0)
# 
# ))
# 
# m_1_1 <- try(ltmle(dwide,
#                    Anodes=c("A.3","A.4"),
#                    Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3",
#                             "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
#                             "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4",
#                             "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
#                    ),
#                    Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
#                    Qform=NULL, gform=NULL, stratify=FALSE,
#                    SL.library=mylibrary2, estimate.time=F, variance.method="tmle", gcomp=F,
#                    abar = c(1,1)
# 
# ))
# (mean(m_1_1$Qstar))*(b-a)+a
# (mean(m_1_0$Qstar))*(b-a)+a
# (mean(m_1_1$Qstar[dwide$Z == 1]))*(b-a)+a
# (mean(m_1_0$Qstar[dwide$Z == 1]))*(b-a)+a

#### TABLE FOR RESPECTIVE DATASET
c1 <- rep(index_str[i], 15)
c2  <- c(rep("Overall",3),rep("X1",2),rep("X2",3),rep("X3",2),rep("X4",3),rep("X5",2))
c3  <- c(rep("NA",3), "0","1","A","B","C","0","1","A","B","C","0","1")
c4  <- c("NA",3,4,rep(NA,12))
c5  <- c(SATT,SATT_year_3,SATT_year_4,SATT_X1_0,SATT_X1_1,SATT_X2_A,SATT_X2_B,SATT_X2_C,
         SATT_X3_0,SATT_X3_1,SATT_X4_A,SATT_X4_B,SATT_X4_C,SATT_X5_0,SATT_X5_1)
c6 <- c(SATT_lower,SATT_year_3_lower,SATT_year_4_lower,SATT_X1_0_lower,SATT_X1_1_lower,
        SATT_X2_A_lower,SATT_X2_B_lower,SATT_X2_C_lower,
        SATT_X3_0_lower,SATT_X3_1_lower,SATT_X4_A_lower,SATT_X4_B_lower,SATT_X4_C_lower,
        SATT_X5_0_lower,SATT_X5_1_lower)
c7 <- c(SATT_upper,SATT_year_3_upper,SATT_year_4_upper,SATT_X1_0_upper,SATT_X1_1_upper,
        SATT_X2_A_upper,SATT_X2_B_upper,SATT_X2_C_upper,
        SATT_X3_0_upper,SATT_X3_1_upper,SATT_X4_A_upper,SATT_X4_B_upper,SATT_X4_C_upper,
        SATT_X5_0_upper,SATT_X5_1_upper)
results <- data.frame(rep(NA,15))
results$dataset.num <- c1
results$variable <- c2
results$level    <- c3  
results$year     <- c4
results$satt     <- c5
results$lower90  <- c6
results$upper90  <- c7
results <- results[,-1]
results 


}

#
parallel::stopCluster(cl)

analysis.results   <- do.call("rbind",analysis)
analysis.results

write.csv(analysis.results, file="results.csv")