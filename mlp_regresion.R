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
bad_vars <- c("V5_C_avg.3","V5_C_avg.4") # linear dependence
v2 <- c("n.patients.3", "V1_avg.3", "V2_avg.3",  "V3_avg.3", "V4_avg.3", "V5_A_avg.3",  "V5_B_avg.3",  "V5_C_avg.3",
         "n.patients.4", "V1_avg.4", "V2_avg.4",  "V3_avg.4", "V4_avg.4", "V5_A_avg.4",  "V5_B_avg.4", "V5_C_avg.4")
v2 <- v2[!v2 %in% bad_vars]
feat <- c(v2)
label <- c("Y.3") # Y.4

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

# c(train_data, train_labels) %<-% boston_housing$train
# c(test_data, test_labels) %<-% boston_housing$test
# paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))
# train_data[1, ] # Display sample features, notice the different scales
# 
# column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
#                   'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
# 
# train_df <- train_data %>% 
#   as_tibble(.name_repair = "minimal") %>% 
#   setNames(column_names) %>% 
#   mutate(label = train_labels)
# 
# test_df <- test_data %>% 
#   as_tibble(.name_repair = "minimal") %>% 
#   setNames(column_names) %>% 
#   mutate(label = test_labels)

spec <- feature_spec(tr_d, label ~ . ) %>%
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

#spec
#train_data <- dict(train_data)
#test_data <- dict(test_data)

# layer <- layer_dense_features(
#   feature_columns = dense_features(spec), 
#   dtype = tf$float32
# )
# 
# layer(dict(tr_d))
# 
# # #input <- train_data
# input <- layer_input_from_dataset(tr_d %>% select(-label))
# output <- input %>% 
# #   layer_dense_features(dense_features(spec)) %>% 
# #   layer_dense(units = 16, activation = "relu") %>%
# #   %layer_dense(units = 16, activation = "relu") %>%
# #   layer_dense(units = 1) 
# 
# model <- keras_model(input, output)
# 
# summary(model)
# 
# model %>% 
#   compile(
#     loss = "mse",
#     optimizer = optimizer_rmsprop(),
#     metrics = list("mean_absolute_error")
#   )

build_model <- function() {
  
  input <- layer_input_from_dataset(tr_d %>% select(-label))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    #layer_dropout(rate = 0.1) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dropout(rate = 0.1) %>%
    #layer_dense(units = 8, activation = "relu") %>%
    #layer_dense(units = 5, activation = "relu", kernel_regularizer = regularizer_l1(l = 0.5)) %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = loss_mean_squared_error(), #loss_huber(),loss_mean_squared_error
      optimizer = optimizer_rmsprop(learning_rate = 0.05),
      metrics = list("mse")
    )
  
  model
}

# print_dot_callback <- callback_lambda(
#   on_epoch_end = function(epoch, logs) {
#     if (epoch %% 80 == 0) cat("\n")
#     cat(".")
#   }
# )    

lr_sched =  list(
  callback_early_stopping(patience = 50),
  callback_reduce_lr_on_plateau(patience = 5, factor = 0.8)
  # callback_learning_rate_scheduler(
  #   tf$keras$experimental$CosineDecayRestarts(.02, 10, t_mul = 2, m_mul = .8))
)

model <- build_model()

history <- model %>% fit(
  x = tr_d %>% select(-label),
  y = tr_d$label,
  epochs = 1e4,
  validation_split = 0.2,
  verbose = 2,
  batch_size = 32L,
  shuffle = FALSE,
  view_metrics = FALSE,
  callbacks = lr_sched
)

hist(unlist(get_weights(model)))

p1 <- model %>% predict( tr_d %>% select(-label))
plot(p1, tr_d$label)
abline(0,1)

#library(ggplot2)
plot(history)

cat("MLP RSS:", min(history$metrics$val_mse)) # 17940.87 mit 8 relu ,8 relu, 0.1 dropout nach beiden hidden layern
gam_fom <- as.formula(paste0("label~",paste0("s(",feat,")",collapse="+")))
m <- mgcv::gam(gam_fom, data = tr_d); cat("GAM RSS:",mean(resid(m)^2),"\n")
m <- lm(label ~ ., data = tr_d); cat("LM RSS:",mean(resid(m)^2))


