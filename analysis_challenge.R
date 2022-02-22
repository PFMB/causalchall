setwd("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")

# read in and merge data
d1 <- read.csv("data/track2/practice/acic_practice_0001.csv")
d2 <- read.csv("data/track2/practice_year/acic_practice_year_0001.csv")
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



m_1 <- ltmleMSM(dwide,
                        Anodes=c("A.3","A.4"),
                        Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                                "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                                "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                                "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                                ),
                        Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                        Qform=NULL, gform=NULL, stratify=FALSE,
                        SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
                        final.Ynodes=c("Y.3","Y.4"),
                        regimes=regimesList,
                        working.msm="Y ~ A + Z",
                        summary.measures=my.sum.measures 
)
summary(m_1)

m_2 <- ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*time + Z",
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
                SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X1",
                summary.measures=my.sum.measures 
)
summary(m_3)

m_4 <- ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X2",
                summary.measures=my.sum.measures 
)
summary(m_4)


m_5 <- ltmleMSM(dwide,
                Anodes=c("A.3","A.4"),
                Lnodes=c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
                         "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
                         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
                         "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
                ),
                Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                Qform=NULL, gform=NULL, stratify=FALSE,
                SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
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
                SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
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
                SL.library=mylibrary2, estimate.time=F, variance.method="ic", gcomp=F,
                final.Ynodes=c("Y.3","Y.4"),
                regimes=regimesList,
                working.msm="Y ~ A*Z*X5",
                summary.measures=my.sum.measures 
)
summary(m_7)

#### ESTIMANDS
# find out how to generically extract Yrange or specify Yrange but make sure it makes sense for *all* data sets

#
SATT <- (invlogit(t(c(1,1,1))%*%m_1$beta)*1620.238+499.518) - (invlogit(t(c(1,0,1))%*%m_1$beta)*+499.518)
SATT

#
SATT_year_3 <- (invlogit(t(c(1,1,1,1,1))%*%m_2$beta)*1620.238+499.518) - (invlogit(t(c(1,0,1,1,0))%*%m_2$beta)*+499.518)
SATT_year_4 <- (invlogit(t(c(1,1,2,1,2))%*%m_2$beta)*1620.238+499.518) - (invlogit(t(c(1,0,2,1,0))%*%m_2$beta)*+499.518)
SATT_year_3
SATT_year_4

#
SATT_X1_1 <- (invlogit(t(c(1,1,1,1,1,1,1,1))%*%m_3$beta)*1620.238+499.518) - (invlogit(t(c(1,0,1,1,0,0,1,0))%*%m_3$beta)*+499.518)
SATT_X1_0 <- (invlogit(t(c(1,1,1,0,1,0,0,0))%*%m_3$beta)*1620.238+499.518) - (invlogit(t(c(1,0,1,0,0,0,0,0))%*%m_3$beta)*+499.518)

# equivalent for X2-X5
