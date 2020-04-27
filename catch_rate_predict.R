library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(xgboost)
library(glmnet)
library(gamlss)
library(mlr3verse)

pokemon <- read.csv("pokemon_training_data.csv",sep=",")
set.seed(42)
sample_indx <- sample.int(nrow(pokemon),size=10000,replace = FALSE)
pokemon <- pokemon[sample_indx,]


#get rid of fully correlated features
full_data <- pokemon %>% dplyr::select(individual_ability,egg_group_1,egg_group_2,color,body_style,height_m,weight_kg,catch_rate,base_egg_steps,base_friendship,experience_growth,type_1,type_2,gender,generation,has_mega_evolution,is_legendary,hp,attack,defense,sp_attack,sp_defense,speed)
full_data$is_legendary <- as.double(full_data$is_legendary)
task_catch_rate_full <- TaskRegr$new(id="predict_full_catch_rate",backend=full_data,target="catch_rate")

##########
#SVR-Approach
#reduce factor levels and transform to binary variables
fact_reducer = PipeOpCollapseFactors$new(id="collapse",param_vals=list(target_level_count=20))
task_catch_rate_full <- fact_reducer$train(list(task_catch_rate_full))$output
fact_encoder = po("encode", method = "treatment",affect_columns = selector_type("factor"))
task_catch_rate_full <- fact_encoder$train(list(task_catch_rate_full))$output

#calculate information_gain for each variable and sepparate according to factor 
information_gain <- flt("information_gain")
information_gain_scores <- information_gain$calculate(task_catch_rate_full) %>% as.data.table()
information_gain_scores$individual_ability <- str_detect(information_gain_scores$feature,"individual_ability")
information_gain_scores$type_1 <- str_detect(information_gain_scores$feature,"type_1")
information_gain_scores$type_2 <- str_detect(information_gain_scores$feature,"type_2")
information_gain_scores$body_style <- str_detect(information_gain_scores$feature,"body_style")
information_gain_scores$egg_group_1 <- str_detect(information_gain_scores$feature,"egg_group_1")
information_gain_scores$egg_group_2 <- str_detect(information_gain_scores$feature,"egg_group_2")
information_gain_scores$color <- str_detect(information_gain_scores$feature,"color")
information_gain_scores$has_mega_evolution <- str_detect(information_gain_scores$feature,"has_mega_evolution")
information_gain_scores$gender <- str_detect(information_gain_scores$feature,"gender")

individual_ability_scores <- information_gain_scores %>% filter(individual_ability == TRUE)
type_1_scores <- information_gain_scores %>% filter(type_1 == TRUE)
type_2_scores <- information_gain_scores %>% filter(type_2== TRUE)
body_style_scores <- information_gain_scores %>% filter(body_style == TRUE)
egg_group_1_scores <- information_gain_scores %>% filter(egg_group_1 == TRUE)
egg_group_2_scores <- information_gain_scores %>% filter(egg_group_2 == TRUE)
color_scores <- information_gain_scores %>% filter(color == TRUE)
has_mega_evolution_scores <- information_gain_scores %>% filter(has_mega_evolution == TRUE)
gender_scores <- information_gain_scores %>% filter(gender == TRUE)
remaining_scores <- setdiff(information_gain_scores,rbind(individual_ability_scores,type_1_scores,type_2_scores,
                                                          body_style_scores,egg_group_1_scores, egg_group_2_scores,
                                                          color_scores,has_mega_evolution_scores,gender_scores))

factor_names<- list("individual_ability","type_1","type_2","body_style","egg_group_1",
                    "egg_group_2","color","has_mega_evolution","gender")

factor_scores <- list(individual_ability_scores,type_1_scores,type_2_scores,body_style_scores,egg_group_1_scores,
                      egg_group_2_scores,color_scores,has_mega_evolution_scores,gender_scores)

all_scores <- remaining_scores %>% dplyr::select(feature,score)

# --> information_gain for factor variables can be calculated by summing the scores for binary variables
for (i in 1:length(factor_scores)){
  new_score <- data.frame(feature = factor_names[[i]], score = factor_scores[[i]]$score %>% sum())
  all_scores<- rbind(all_scores,new_score)
}

View(all_scores) # --> choose variables with highest overall score
# --> top 5: weight_kg, defense, hp, attack, sp_defense


# create data for svr

data_catch_rate_original <- pokemon  %>% dplyr::select(weight_kg, defense, hp, attack, sp_defense,catch_rate)
task_catch_rate_original <-  TaskRegr$new(id="catch_rate_predict",backend=data_catch_rate_original,target="catch_rate") 

# binary factor levels
fact_reducer = PipeOpCollapseFactors$new(id="collapse",param_vals=list(target_level_count=20))
task_catch_rate_reduced <- fact_reducer$train(list(task_catch_rate_original))$output
fact_encoder = po("encode", method = "treatment",affect_columns = selector_type("factor"))
task_catch_rate_reduced <- fact_encoder$train(list(task_catch_rate_reduced))$output

# find indices for test and train data
all_indx <- 1:nrow(full_data)
train_indx <- all_indx[full_data$generation != 6]
test_indx <- all_indx[full_data$generation == 6]

## radial kernel svr
learner_svr_reduced_radial <- lrn("regr.svm",kernel="radial",type="eps-regression",epsilon=0.5,cost=9,gamma=0.9)
model_catch_rate_svr_reduced_radial <- learner_svr_reduced_radial$train(task_catch_rate_reduced,row_ids=train_indx)

# prediction
prediction_catch_rate_svr_reduced_radial <- model_catch_rate_svr_reduced_radial$predict(task_catch_rate_reduced,row_ids=test_indx)
prediction_catch_rate_svr_reduced_radial$score()  
autoplot(prediction_catch_rate_svr_reduced_radial)

## linear kernel svr
learner_svr_reduced_linear <- lrn("regr.svm",kernel="linear",type="eps-regression",epsilon=0.6,cost=2)
model_catch_rate_svr_reduced_linear <- learner_svr_reduced_linear$train(task_catch_rate_reduced,row_ids=train_indx)

# prediction
prediction_catch_rate_svr_reduced_linear <- model_catch_rate_svr_reduced_linear$predict(task_catch_rate_reduced,row_ids=test_indx)
prediction_catch_rate_svr_reduced_linear$score()  
autoplot(prediction_catch_rate_svr_reduced_linear)

## do tuning for radial and linear kernel
# without gamma tuning:
possible_c <- 1:10
possible_epsilon <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.999,0.9999)
tuning_results <- data.frame(row.names=1:(length(possible_c)*length(possible_epsilon)))
tuning_results$c <- rep(possible_c,each=length(possible_epsilon))
tuning_results$epsilon <- rep(possible_epsilon,times=length(possible_c))
tuning_results$mse <- 0
for (j in 1:length(possible_c)){
  for (i in 1:length(possible_epsilon)){
    learner <- lrn("regr.svm",kernel="linear",type="eps-regression",epsilon=possible_epsilon[i],cost=possible_c[j])
    model <- learner$train(task_catch_rate_reduced,row_ids=train_indx)
    prediction <- model$predict(task_catch_rate_reduced,row_ids=test_indx)
    tuning_results[length(possible_epsilon)*(j-1)+i,]$mse <- prediction$score()
    print(length(possible_epsilon)*(j-1)+i)
  }
}

tuning_results <- tuning_results[order(tuning_results$mse),]
View(tuning_results)

# c=2  and epsilon=0.9 = 1: mse = 2766.911 for radial kernel
# c= 10 and epsilon=0.999: mse = 2790.949 for linear kernel
 
# with gamma tuning (just for radial kernel):
possible_gamma <- c(1,0.5,0.1,0.01,0.001,0.0001)
possible_c <- 1:10
possible_epsilon <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.999,0.9999)
tuning_results <- data.frame(row.names=1:(length(possible_c)*length(possible_epsilon)*length(possible_gamma)))
tuning_results$gamma <- rep(possible_gamma,each=length(possible_epsilon)*length(possible_c))
tuning_results$c <- rep(rep(possible_c,each=length(possible_epsilon)),times=length(possible_gamma))
tuning_results$epsilon <- rep(possible_epsilon,times=length(possible_c)*length(possible_gamma))
tuning_results$mse <- 0
for (k in 1:length(possible_gamma)){
  for (j in 1:length(possible_c)){
    for (i in 1:length(possible_epsilon)){
      learner <- lrn("regr.svm",kernel="radial",type="eps-regression",epsilon=possible_epsilon[i],cost=possible_c[j],gamma=possible_gamma[k])
      model <- learner$train(task_catch_rate_reduced,row_ids=train_indx)
      prediction <- model$predict(task_catch_rate_reduced,row_ids=test_indx)
      tuning_results[(k-1)*length(possible_c)*length(possible_epsilon)+length(possible_epsilon)*(j-1)+i,]$mse <- prediction$score()
      print((k-1)*length(possible_c)*length(possible_epsilon)+length(possible_epsilon)*(j-1)+i)
    }
  }
}

tuning_results <- tuning_results[order(tuning_results$mse),]
View(tuning_results)
# --> best results for:
# gamma = 1
# epsilon = 0.5
# c = 8-10 
# --> mse = 2594.413

###############
# KKNN Approach

## define learner with epsilon =0.999  and cost parameter = 5
learner_kknn <- lrn("regr.kknn",distance=1,k=150)
model_catch_rate_kknn <- learner_kknn$train(task_catch_rate_reduced,row_ids=train_indx)

# prediction
prediction_catch_rate_kknn <- model_catch_rate_kknn$predict(task_catch_rate_reduced,row_ids=test_indx)
prediction_catch_rate_kknn$score() 
autoplot(prediction_catch_rate_kknn)

# do tuning for kknn
possible_k <- seq(10,500,by=10)
possible_distance <- c(1,2,3)
tuning_results <- data.frame(row.names=1:(length(possible_k)*length(possible_distance)))
tuning_results$k <- rep(possible_k,each=length(possible_distance))
tuning_results$distance <- rep(possible_distance,times=length(possible_k))
tuning_results$mse <- 0
for (j in 1:length(possible_k)){
  for (i in 1:length(possible_distance)){
    learner <- lrn("regr.kknn",k=possible_k[j],distance=possible_distance[i])
    model <- learner$train(task_catch_rate_reduced,row_ids=train_indx)
    prediction <- model$predict(task_catch_rate_reduced,row_ids=test_indx)
    tuning_results[length(possible_distance)*(j-1)+i,]$mse <- prediction$score()
    print(length(possible_distance)*(j-1)+i)
  }
}


tuning_results <- tuning_results[order(tuning_results$mse),]
View(tuning_results)
# k = 140-160, distance = 1, mse: 2548.170

# visualisation


