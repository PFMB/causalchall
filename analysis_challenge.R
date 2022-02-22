setwd("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")

# index
index <- 1:3400


# read in and merge data
d1 <- read.csv("data/track2/practice/acic_practice_0005.csv")
d2 <- read.csv("data/track2/practice_year/acic_practice_year_0005.csv")
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
plot(density(sqrt(dwide$n.patients.2)))
### TO DO Daten anschauen und verstehen!
### TO DO naiver vergleich mit Mittelwerten

##############
# LTMLE MSM  #
##############
library(ltmle)
library(arm) # für invlogit und bayesglm
# simple library for testing
mylibrary <-  NULL
# better library
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
# final library
mylibrary3 <- NULL #update

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
                        working.msm="Y ~ A + Z",
                        summary.measures=my.sum.measures 
))
summary(m_1)
cc_trunc(m_1) # correct?
# if(class(m_1)=="try-error"){m_1 <- ...}

m_2 <- ltmleMSM(dwide,
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
                summary.measures=my.sum.measures 
)
summary(m_2)

m_3 <- ltmleMSM(dwide,
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
                summary.measures=my.sum.measures 
)
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
                summary.measures=my.sum.measures 
))
summary(m_4)
# if(class(m1)=="try-error"){m1 <- ...}

m_5 <- ltmleMSM(dwide,
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
                summary.measures=my.sum.measures 
)
summary(m_5)

m_6 <- ltmleMSM(dwide,
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
                summary.measures=my.sum.measures 
)
summary(m_6)


m_7 <- ltmleMSM(dwide,
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
                summary.measures=my.sum.measures 
)
summary(m_7)

#### ESTIMANDS
a<-attr(m_1$transformOutcome,"Yrange")[1]
b<-attr(m_1$transformOutcome,"Yrange")[2] 
#
SATT <- (invlogit(t(c(1,1,1))%*%m_1$beta)*(b-a)+a) - (invlogit(t(c(1,0,1))%*%m_1$beta)*(b-a)+a)
SATT

#
SATT_year_3 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_2$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_2$beta)*(b-a)+a)
SATT_year_4 <- (invlogit(t(c(1,1,2,1,2,1,2,2))%*%m_2$beta)*(b-a)+a) - (invlogit(t(c(1,0,2,1,0,0,2,0))%*%m_2$beta)*(b-a)+a)
SATT_year_3
SATT_year_4

#
SATT_X1_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_3$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_3$beta)*(b-a)+a)
SATT_X1_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_3$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_3$beta)*(b-a)+a)
SATT_X1_0
SATT_X1_1

SATT_X2_A <- (invlogit(t(c(1,1,1,0,0,1,0,0,0,0,0,0))%*%m_4$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0,0,0,0,0))%*%m_4$beta)*(b-a)+a)
SATT_X2_B <- (invlogit(t(c(1,1,1,1,0,1,1,0,1,0,1,0))%*%m_4$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,0,0,1,0,0,0))%*%m_4$beta)*(b-a)+a)
SATT_X2_C <- (invlogit(t(c(1,1,1,0,1,1,0,1,0,1,0,1))%*%m_4$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,1,0,0,0,0,1,0,0))%*%m_4$beta)*(b-a)+a)
SATT_X2_A
SATT_X2_B
SATT_X2_C

SATT_X3_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_5$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_5$beta)*(b-a)+a)
SATT_X3_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_5$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_5$beta)*(b-a)+a)
SATT_X3_0 
SATT_X3_1

SATT_X4_A <- (invlogit(t(c(1,1,1,0,0,1,0,0,0,0,0,0))%*%m_6$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0,0,0,0,0))%*%m_6$beta)*(b-a)+a)
SATT_X4_B <- (invlogit(t(c(1,1,1,1,0,1,1,0,1,0,1,0))%*%m_6$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,0,0,1,0,0,0))%*%m_6$beta)*(b-a)+a)
SATT_X4_C <- (invlogit(t(c(1,1,1,0,1,1,0,1,0,1,0,1))%*%m_6$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,1,0,0,0,0,1,0,0))%*%m_6$beta)*(b-a)+a)
SATT_X4_A
SATT_X4_B
SATT_X4_C

SATT_X5_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_7$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_7$beta)*(b-a)+a)
SATT_X5_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_7$beta)*(b-a)+a) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_7$beta)*(b-a)+a)
SATT_X5_0 
SATT_X5_1

#### TABLE FOR RESPECTIVE DATASET
c1 <- NA
c2  <- c(rep("Overall",3),rep("X1",2),rep("X2",3),rep("X3",2),rep("X4",3),rep("X5",2))
c3  <- c(rep("NA",3), "0","1","A","B","C","0","1","A","B","C","0","1")
c4  <- c("NA",3,4,rep(NA,12))
c5  <- c(SATT,SATT_year_3,SATT_year_4,SATT_X1_0,SATT_X1_1,SATT_X2_A,SATT_X2_B,SATT_X2_C,
         SATT_X3_0,SATT_X3_1,SATT_X4_A,SATT_X4_B,SATT_X4_C,SATT_X5_0,SATT_X5_1)
results <- data.frame(rep(NA,15))
results$variable <- c2
results$level    <- c3  
results$year     <- c4
results$satt     <- c5
results <- results[,-1]
results 

# 90% CI über 1_3$msm und Kovarianzmatrix ...vcov(m_1$msm)
