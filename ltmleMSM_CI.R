msm.se <- function(msmobj,b1=NULL,b2=NULL,cov="IC"){
  
if(is.null(attr(msmobj$transformOutcome,"Yrange"))==FALSE){
    a<-attr(msmobj$transformOutcome,"Yrange")[1]
    b<-attr(msmobj$transformOutcome,"Yrange")[2]
}else{{a<-0; b<-1}}
ab <- b-a
 
if(is.null(b1)){stop("provide vector for b1")} 
if(is.null(b2)){ind <- 0}else{ind <- 1}
  
comb1 <- paste(paste0(b1,"*x",1:length(b1)),collapse="+")
if(is.null(b2)==FALSE){comb2 <- paste(paste0(b2,"*x",1:length(b2)),collapse="+")}else{comb2 <- 0}

quasibinform <- sprintf(paste0("~ ((exp(",comb1,"))/(1+exp(",comb1,")))* %f - %f *((exp(",comb2,
                               "))/(1+exp(",comb2,")))* %f"), ab, ind, ab)

if(cov=="IC"){covm <- cov(ltmle:::GetSummaryLtmleMSMInfo(msmobj,estimator="tmle")$IC)}else{
covm <- msmobj$variance.estimate}

se <- msm::deltamethod(as.formula(quasibinform),msmobj$beta, covm)
se

}
