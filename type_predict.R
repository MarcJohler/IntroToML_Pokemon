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
task_type_1_full <- TaskClassif$new(id="predict_full_type",backend=full_data,target="type_1")

##########
#SVR-Approach
#reduce factor levels and transform to binary variables
fact_reducer = PipeOpCollapseFactors$new(id="collapse",param_vals=list(target_level_count=20))
task_type_1_full <- fact_reducer$train(list(task_type_1_full))$output
fact_encoder = po("encode", method = "treatment",affect_columns = selector_type("factor"))
task_type_1_full <- fact_encoder$train(list(task_type_1_full))$output

#calculate information_gain for each variable and sepparate according to factor 
information_gain <- flt("information_gain")
information_gain_scores <- information_gain$calculate(task_type_1_full) %>% as.data.table()
information_gain_scores$individual_ability <- str_detect(information_gain_scores$feature,"individual_ability")
information_gain_scores$type_2 <- str_detect(information_gain_scores$feature,"type_2")
information_gain_scores$body_style <- str_detect(information_gain_scores$feature,"body_style")
information_gain_scores$egg_group_1 <- str_detect(information_gain_scores$feature,"egg_group_1")
information_gain_scores$egg_group_2 <- str_detect(information_gain_scores$feature,"egg_group_2")
information_gain_scores$color <- str_detect(information_gain_scores$feature,"color")
information_gain_scores$has_mega_evolution <- str_detect(information_gain_scores$feature,"has_mega_evolution")
information_gain_scores$gender <- str_detect(information_gain_scores$feature,"gender")

individual_ability_scores <- information_gain_scores %>% filter(individual_ability == TRUE)
type_2_scores <- information_gain_scores %>% filter(type_2== TRUE)
body_style_scores <- information_gain_scores %>% filter(body_style == TRUE)
egg_group_1_scores <- information_gain_scores %>% filter(egg_group_1 == TRUE)
egg_group_2_scores <- information_gain_scores %>% filter(egg_group_2 == TRUE)
color_scores <- information_gain_scores %>% filter(color == TRUE)
has_mega_evolution_scores <- information_gain_scores %>% filter(has_mega_evolution == TRUE)
gender_scores <- information_gain_scores %>% filter(gender == TRUE)
remaining_scores <- setdiff(information_gain_scores,rbind(individual_ability_scores,type_2_scores,
                                                          body_style_scores,egg_group_1_scores, egg_group_2_scores,
                                                          color_scores,has_mega_evolution_scores,gender_scores))

factor_names<- list("individual_ability","type_2","body_style","egg_group_1",
                    "egg_group_2","color","has_mega_evolution","gender")

factor_scores <- list(individual_ability_scores,type_2_scores,body_style_scores,egg_group_1_scores,
                      egg_group_2_scores,color_scores,has_mega_evolution_scores,gender_scores)

all_scores <- remaining_scores %>% dplyr::select(feature,score)

# --> information_gain for factor variables can be calculated by summing the scores for binary variables
for (i in 1:length(factor_scores)){
  new_score <- data.frame(feature = factor_names[[i]], score = factor_scores[[i]]$score %>% sum())
  all_scores<- rbind(all_scores,new_score)
}

View(all_scores) 
# --> choose variables with the highest overall scores
# --> exclude egg_groups since they are often called like the type
# --> exclude type_2 since type_2 maybe shall be predicted too
# --> top 10 with this conditions:

# according to information_gain:
#weight_kg, color, body_style, speed, catch_rate,base_egg_steps,attack,sp_attack,height_m,defense

# according to performance:
# base_egg_steps, sp_attack, experience_growth, defense, base_friendship, height_m, speed, weight_kg, generation, catch_rate

# create data for svr

data_type_1_original <- pokemon  %>% dplyr::select(weight_kg, color, body_style, speed, catch_rate,base_egg_steps,attack,sp_attack,height_m,defense,type_1)
task_type_1_original <-  TaskClassif$new(id="type_1_predict",backend=data_type_1_original,target="type_1") 

# binary factor levels
fact_reducer = PipeOpCollapseFactors$new(id="collapse",param_vals=list(target_level_count=20))
task_type_1_reduced <- fact_reducer$train(list(task_type_1_original))$output
fact_encoder = po("encode", method = "treatment",affect_columns = selector_type("factor"))
task_type_1_reduced <- fact_encoder$train(list(task_type_1_reduced))$output

# find indices for test and train data
all_indx <- 1:nrow(full_data)
train_indx <- all_indx[full_data$generation != 6]
test_indx <- all_indx[full_data$generation == 6]

## radial kernel svr
learner_svr_reduced_radial <- lrn("classif.svm",kernel="radial",type="C-classification")
model_type_1_svr_reduced_radial <- learner_svr_reduced_radial$train(task_type_1_reduced,row_ids=train_indx)

# prediction
prediction_type_1_svr_reduced_radial <- model_type_1_svr_reduced_radial$predict(task_type_1_reduced,row_ids=test_indx)
prediction_type_1_svr_reduced_radial$score()  
prediction_type_1_svr_reduced_radial$confusion

# information_gain: ce - 0.6833
# performance: ce - 0.8881 
# --> use features of information_gain

## linear kernel svr
learner_svr_reduced_linear <- lrn("classif.svm",kernel="linear",type="C-classification")
model_type_1_svr_reduced_linear <- learner_svr_reduced_linear$train(task_type_1_reduced,row_ids=train_indx)

# prediction
prediction_type_1_svr_reduced_linear <- model_type_1_svr_reduced_linear$predict(task_type_1_reduced,row_ids=test_indx)
prediction_type_1_svr_reduced_linear$score()  
prediction_type_1_svr_reduced_linear$confusion

## do tuning for radial and linear kernel
# without gamma tuning (for linear kernel)
possible_c <- 1:10
tuning_results <- data.frame(row.names=1:(length(possible_c)))
tuning_results$c <- possible_c
tuning_results$ce <- 0
for (j in 1:length(possible_c)){
  learner <- lrn("classif.svm",kernel="linear",cost=possible_c[j],type="C-classification")
  model <- learner$train(task_type_1_reduced,row_ids=train_indx)
  prediction <- model$predict(task_type_1_reduced,row_ids=test_indx)
  tuning_results[j,]$ce <- prediction$score()
  print(j)
}

tuning_results <- tuning_results[order(tuning_results$ce),]
View(tuning_results)

# best results for:
# c= 2
# --> ce = 0.7132867

# with gamma tuning (for radial kernel)
possible_c <- 1:10
possible_gamma <- c(0.0001,0.001,0.01,0.1,1,10,100,1000,10000)
tuning_results <- data.frame(row.names=1:(length(possible_c)*length(possible_gamma)))
tuning_results$c <- rep(possible_c,each=length(possible_gamma))
tuning_results$gamma <- rep(possible_gamma,times=length(possible_c))
tuning_results$ce <- 0
for (j in 1:length(possible_c)){
  for (i in 1:length(possible_gamma)){
    learner <- lrn("classif.svm",kernel="radial",cost=possible_c[j],gamma=possible_gamma[i],type="C-classification")
    model <- learner$train(task_type_1_reduced,row_ids=train_indx)
    prediction <- model$predict(task_type_1_reduced,row_ids=test_indx)
    tuning_results[length(possible_gamma)*(j-1)+i,]$ce <- prediction$score()
    print(length(possible_gamma)*(j-1)+i)
  }
}

tuning_results <- tuning_results[order(tuning_results$ce),]
View(tuning_results)
# best results for:
# c = 9 oder 10
# gamma = 0.0001
# -->ce = 0.6433566

###############
# KKNN Approach
data_type_1_kknn <- pokemon %>% dplyr::select(weight_kg, color, body_style, speed, catch_rate, type_1)
task_type_1_kknn <- TaskClassif$new(id="predict_total_kknn",backend=data_type_1_kknn,target="type_1")

## define learner
learner_kknn <- lrn("classif.kknn",distance=1,k=150)
model_type_1_kknn <- learner_kknn$train(task_type_1_reduced,row_ids=train_indx)

# prediction
prediction_type_1_kknn <- model_type_1_kknn$predict(task_type_1_reduced,row_ids=test_indx)
prediction_type_1_kknn$score() 
prediction_type_1_kknn$confusion

# do tuning for kknn
possible_k <- seq(10,500,by=10)
possible_distance <- c(1,2,3)
tuning_results <- data.frame(row.names=1:(length(possible_k)*length(possible_distance)))
tuning_results$k <- rep(possible_k,each=length(possible_distance))
tuning_results$distance <- rep(possible_distance,times=length(possible_k))
tuning_results$ce <- 0
for (j in 1:length(possible_k)){
  for (i in 1:length(possible_distance)){
    learner <- lrn("classif.kknn",k=possible_k[j],distance=possible_distance[i])
    model <- learner$train(task_type_1_reduced,row_ids=train_indx)
    prediction <- model$predict(task_type_1_reduced,row_ids=test_indx)
    tuning_results[length(possible_distance)*(j-1)+i,]$ce <- prediction$score()
    print(length(possible_distance)*(j-1)+i)
  }
}


tuning_results <- tuning_results[order(tuning_results$ce),]
View(tuning_results)
# k = 240-260
# distance = 2
# --> ce = 0.6603397
