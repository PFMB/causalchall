#setwd("C:/Users/01465840.F875A4D1C344/Dropbox/Documents/Projects_Other/ACIC_2022_data_challenge")
setwd("/Users/flipst3r/RStHomeDir/GitHub/causalchall")
library(dplyr)
library(tfdatasets)
library(keras)
library(tensorflow)
library(reticulate)
# index
index <- 1:3400
rm(list = ls())
set.seed(1)
set_random_seed(1)


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

###

#v1 <- c("A.3","A.4")
#bad_vars <- c("V5_C_avg.3","V5_C_avg.4") # linear dependence
v2 <- c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
        "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", "V5_A_avg.4",  "V5_B_avg.4", "V5_C_avg.4")
#v2 <- v2[!v2 %in% bad_vars]
feat <- c(v2)
label <- c("A.3") # Y.3

#d <- sapply(dwide[,vars], scale)

d_rows <- 1:nrow(dwide)
tst_rows <- sample(d_rows,100) 
tr_rows <- d_rows[!d_rows %in% tst_rows]

###

#boston_housing <- dataset_boston_housing()
tr_d <- as.matrix(dwide[tr_rows ,feat])
tr_d <- tr_d %>% as_tibble(.name_repair = "minimal")
tr_d$label <- array(dwide[tr_rows ,label])

tst_d <- as.matrix(dwide[tst_rows ,feat])
tst_d <- tst_d %>% as_tibble(.name_repair = "minimal")
tst_d$label <- array(dwide[tst_rows ,label])



spec <- feature_spec(tr_d, label ~ . ) %>%
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

build_model <- function() {
  
  input <- layer_input_from_dataset(tr_d %>% select(-label))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec), name = "layer_0") %>% 
    #layer_batch_normalization() %>%
    #layer_dense(units = 8, activation = "relu", use_bias = T, name = "layer_1", 
    #            kernel_regularizer = regularizer_l2(0.1)) %>%
    layer_dense(units = 32, activation = "relu", use_bias = T, name = "layer_1", kernel_regularizer = regularizer_l2(0.2)) %>%
    #layer_dense(units = 16, activation = "relu", use_bias = T, name = "layer_1") %>%
    #layer_batch_normalization() %>%
    #layer_dense(units = 3, activation = "relu", use_bias = T, name = "layer_2") %>%
    #layer_dense(units = 18, activation = "relu", use_bias = T, name = "layer_2") %>%
    #layer_batch_normalization() %>%
    #layer_dense(units = 18, activation = "relu", use_bias = T, name = "layer_3") %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 32, activation = "relu", use_bias = T, name = "layer_2", kernel_regularizer = regularizer_l2(0.2)) %>%
    layer_dropout(rate = 0.2) %>%
    layer_batch_normalization() %>%
    #layer_batch_normalization() %>%
    #layer_dense(units = 1, name = "output_layer")
    layer_dense(units = 1, name = "output_layer", activation = "sigmoid") 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = loss_binary_crossentropy(), #loss_huber(), loss_mean_squared_error(), loss_binary_crossentropy()
      optimizer = optimizer_rmsprop(learning_rate = 1e-5), #optimizer_rmsprop
      metrics = c("accuracy") # "mse","mae","accuracy"
    )
  
  model
}

lr_sched =  list(
  callback_early_stopping(monitor = "val_loss", patience = 100),
  callback_reduce_lr_on_plateau(monitor = "val_loss",patience = 10, factor = 0.8),
  callback_tensorboard("logs/run_a", histogram_freq = 5)
  # callback_learning_rate_scheduler(
  #   tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .8))
)

model <- build_model()


#tr_d$label <-   

history <- model %>% fit(
  x = tr_d %>% select(-label),
  y = tr_d$label,
  epochs = 1e4,
  validation_split = 0.2,
  verbose = 2,
  batch_size = 32L,
  #shuffle = FALSE,
  view_metrics = FALSE,
  callbacks = lr_sched
)
#tensorboard("logs/run_a")

hist(unlist(get_weights(model)))

p1 <- model %>% predict( tr_d %>% select(-label))
mean((p1 > 0.5) ==  as.logical(tr_d$label)) # 0.67
plot(p1, tr_d$label)
abline(0,1)

#library(ggplot2)
plot(history)

cat("MLP RSS:", min(history$metrics$val_mse),"\n") # 17940.87 mit 8 relu ,8 relu, 0.1 dropout nach beiden hidden layern
gam_fom <- as.formula(paste0("label~",paste0("s(",feat,")",collapse="+"))) # TODO: Try gam with two way interactions
m <- mgcv::gam(gam_fom, data = tr_d, family = binomial(link = "logit")); cat("GAM RSS:",mean(resid(m)^2),"\n")
mean((fitted(m) > 0.5) ==  as.logical(tr_d$label))
#m <- lm(label ~ ., data = tr_d); cat("LM RSS:",mean(resid(m)^2))
#mean((fitted(m) > 0.5) ==  as.logical(tr_d$label))
