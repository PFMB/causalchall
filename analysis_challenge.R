#setwd("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")
#setwd("C:/Users/ua341au/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")
#setwd("/Users/flipst3r/RStHomeDir/GitHub/causalchall")
#setwd("/cluster/home/phibauma/causalchall")
#setwd("/cluster/home/scstepha/causalchall")
#setwd("/home/david/causalchall")
#
library(parallel)
library(doParallel)
library(foreach)
library(msm)
# index
index <- 1:10 #1:1700 # 3400 1701:3400
index_str <- formatC(index, width = 4, format = "d", flag = "0")
#
source('own_learners.r') # case sensitive .r vs .R on cluster
source('ltmleMSM_CI.R')

ncores<-10
library(SuperLearner) # so SL.mean etc is found
cl <- parallel::makeCluster(ncores, outfile = ""); doParallel::registerDoParallel(cl)
exp.var <- setdiff(unlist(ll),"All")
prog=T
if(prog==TRUE){write(matrix("started with analysis..."),file=paste0(getwd(),"/progress.txt",sep=""))}
#

analysis <- foreach(i = index, .export=exp.var, .errorhandling="pass") %dopar% {
        
        set.seed(1)
        st_time <- Sys.time()
        L_nodes <- c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", 
          "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
          "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", 
          "V5_A_avg.4",  "V5_B_avg.4",  "V5_C_avg.4"
        )
        
        #
        if(prog==TRUE){write(matrix(paste("started with analyzing dataset number...",i,"\n")),file=paste0(getwd(),"/progress.txt",sep=""),append=TRUE)}
        #  
        mydata <- paste0("data/track2/practice/acic_practice_",index_str[i],".csv")
        mydata2 <- paste0("data/track2/practice_year/acic_practice_year_",index_str[i],".csv")
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
        library(data.table)
        #setDT(dwide)
        #dwide[,(L_nodes) := lapply(.SD, function(x) log(x + abs(min(x)) + 1)), .SDcols = L_nodes]
        
        source('own_learners.r')
        
        #                        
        regimesList <- list(function(row) c(1,1),
                    function(row) c(0,0)
                    
        )
        
        my.sum.measures <- array(c(c(1,0),c(1,1),
                           c(1,0),c(2,2))
                         ,dim=c(2,2,2),dimnames=list(NULL,c("A","time"),NULL))
        
        m_1 <- try(ltmleMSM(dwide,
                    Anodes=c("A.3","A.4"),
                    Lnodes=L_nodes,
                    Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                    Qform=NULL, gform=NULL, stratify=FALSE,
                    SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                    final.Ynodes=c("Y.3","Y.4"),
                    regimes=regimesList,
                    working.msm="Y ~ A * Z",
                    summary.measures=my.sum.measures,
                    observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
        ))
        q_w <- as.data.frame(learner_weights_summary_Q(m_1))
        g_w <- as.data.frame(learner_weights_summary_g(m_1))
        summary(m_1)
        #cc_trunc(m_1) # correct?
        cat("m_1 finished \n")
        
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
        
        rm(m_1)
        
        ###
        
        m_2 <- try(ltmleMSM(dwide,
                            Anodes=c("A.3","A.4"),
                            Lnodes=L_nodes,
                            Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                            Qform=NULL, gform=NULL, stratify=FALSE,
                            SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                            final.Ynodes=c("Y.3","Y.4"),
                            regimes=regimesList,
                            working.msm="Y ~ A*time*Z",
                            summary.measures=my.sum.measures,
                            observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
        ))
        q_w <- cbind(q_w, learner_weights_summary_Q(m_2))
        g_w <- cbind(g_w, learner_weights_summary_g(m_2))
        summary(m_2)
        cat("m_2 finished \n")
        
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
        
        rm(m_2)
        
        ###
        
        m_3 <- try(ltmleMSM(dwide,
                            Anodes=c("A.3","A.4"),
                            Lnodes=L_nodes,
                            Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                            Qform=NULL, gform=NULL, stratify=FALSE,
                            SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                            final.Ynodes=c("Y.3","Y.4"),
                            regimes=regimesList,
                            working.msm="Y ~ A*Z*X1",
                            summary.measures=my.sum.measures,
                            observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2 
        ))
        q_w <- cbind(q_w, learner_weights_summary_Q(m_3))
        g_w <- cbind(g_w, learner_weights_summary_g(m_3))
        summary(m_3)
        cat("m_3 finished \n")
        
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
        
        rm(m_3)
        
        ###
        
        m_4 <- try(ltmleMSM(dwide,
                            Anodes=c("A.3","A.4"),
                            Lnodes=L_nodes,
                            Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                            Qform=NULL, gform=NULL, stratify=FALSE,
                            SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                            final.Ynodes=c("Y.3","Y.4"),
                            regimes=regimesList,
                            working.msm="Y ~ A*Z*X2",
                            summary.measures=my.sum.measures,
                            observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
        ))
        q_w <- cbind(q_w, learner_weights_summary_Q(m_4))
        g_w <- cbind(g_w, learner_weights_summary_g(m_4))
        summary(m_4)
        cat("m_4 finished \n")
        
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
        
        rm(m_4)
        
        ###
        
        m_5 <- try(ltmleMSM(dwide,
                            Anodes=c("A.3","A.4"),
                            Lnodes=L_nodes,
                            Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                            Qform=NULL, gform=NULL, stratify=FALSE,
                            SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                            final.Ynodes=c("Y.3","Y.4"),
                            regimes=regimesList,
                            working.msm="Y ~ A*Z*X3",
                            summary.measures=my.sum.measures,
                            observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2 
        ))
        q_w <- cbind(q_w, learner_weights_summary_Q(m_5))
        g_w <- cbind(g_w, learner_weights_summary_g(m_5))
        summary(m_5)
        cat("m_5 finished \n")
        
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
        
        rm(m_5)
        
        ###
        
        m_6 <- try(ltmleMSM(dwide,
                            Anodes=c("A.3","A.4"),
                            Lnodes=L_nodes,
                            Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                            Qform=NULL, gform=NULL, stratify=FALSE,
                            SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                            final.Ynodes=c("Y.3","Y.4"),
                            regimes=regimesList,
                            working.msm="Y ~ A*Z*X4",
                            summary.measures=my.sum.measures,
                            observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2 
        ))
        q_w <- cbind(q_w, learner_weights_summary_Q(m_6))
        g_w <- cbind(g_w, learner_weights_summary_g(m_6))
        summary(m_6)
        cat("m_6 finished \n")
        
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
        
        rm(m_6)
        
        ###
        
        m_7 <- try(ltmleMSM(dwide,
                            Anodes=c("A.3","A.4"),
                            Lnodes=L_nodes,
                            Ynodes=c("Y.3","Y.4"), survivalOutcome=F,
                            Qform=NULL, gform=NULL, stratify=FALSE,
                            SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
                            final.Ynodes=c("Y.3","Y.4"),
                            regimes=regimesList,
                            working.msm="Y ~ A*Z*X5",
                            summary.measures=my.sum.measures ,
                            observation.weights=(dwide$n.patients.3+dwide$n.patients.4)/2
        ))
        
        summary(m_7)
        cat("m_7 finished \n")
        
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
        
        ###
        
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
        #                     SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
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
        #                    SL.library=ll, estimate.time=F, variance.method="tmle", gcomp=F,
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
        end_time <- Sys.time()
        cat("- Estimation took", format(end_time - st_time, units = "min"),"-\n")

        list(res = results , 
             Q_weights = rowMeans(cbind(q_w, learner_weights_summary_Q(m_7))),
             g_weights = rowMeans(cbind(g_w, learner_weights_summary_g(m_7))))
        
        
}

parallel::stopCluster(cl)

res <- lapply(analysis, "[[", "res")
analysis.results <- do.call("rbind",res)
analysis.results

r <- paste0(range(index), collapse = "_")
Q_weights <- sapply(analysis, "[[", "Q_weights")
g_weights <- sapply(analysis, "[[", "g_weights")
write.csv(round(sort(rowMeans(Q_weights)), 3), file = paste0("Qweights", r,".csv"))
write.csv(round(sort(rowMeans(g_weights)), 3), file = paste0("gweights", r,".csv"))
write.csv(analysis.results, file= paste0("results", r ,".csv"))
