library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(xgboost)
library(glmnet)
library(gamlss)
library(mlr3verse)

classif_data <- read.csv("combat_prediction_data.csv",sep=",")
classif_data$First_wins <- classif_data$First_wins  %>% as.factor()
classif_data <- classif_data[!classif_data$First_pokemon_faster == 0,]

complete_data <- classif_data %>% dplyr::select(attackVSattack_diff,defenseVSdefense_diff,sp_atkVSsp_atk_diff,sp_defVSsp_def_diff,First_pokemon_faster,HPVSHP_diff,First_wins) 

task_winner<- TaskClassif$new(id="predict_winner",backend=complete_data,target="First_wins")


#kknn learner
learner_kknn<- lrn("classif.kknn")
learner_kknn$predict_type <- "prob"
model_winner_kknn <- learner_kknn$train(task_winner)

prediction_winner_kknn <- model_winner_kknn$predict(task_winner)
prediction_winner_kknn$score(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.9248061  (both effectivities, w.o. legendary, untuned) VS. 0.8714248 (tuned)
#ACC: 0.9625000 (both effectivities, w.o. legendary, untuned) VS. 0.9358600 (tuned)
prediction_winner_kknn$confusion 



#svm learner
learner_svm<- lrn("classif.svm")
learner_svm$predict_type <- "prob"
model_winner_svm <- learner_svm$train(task_winner)

prediction_winner_svm <- model_winner_svm$predict(task_winner)
prediction_winner_svm$score(c(msr("classif.mcc"),msr("classif.acc"))) 
# MCC: 0.8737174(both effectivities, w.o. legendary) VS. 
# ACC: 0.9370600 (both effectivities, w.o. legendary) VS. 
prediction_winner_svm$confusion 

# --> svm takes very long and doesn't have better results --> focus on KKNN


###########################
#KKNN-Approach

#split data into train and test
set.seed(42)
all_indx <- 1:nrow(complete_data)
train_indx <- sample.int(nrow(complete_data),round(0.8*nrow(complete_data)))
test_indx <- setdiff(all_indx,train_indx)

learner_kknn<- lrn("classif.kknn")
learner_kknn$predict_type <- "prob"
model_winner_kknn <- learner_kknn$train(task_winner,row_ids = train_indx)

#train performance
prediction_winner_kknn_train <- model_winner_kknn$predict(task_winner,row_ids = train_indx)
prediction_winner_kknn_train$score(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.9222829  (untuned) VS. 0.8685786 (tuned)
#ACC: 0.9612500 (untuned) VS. 0.9344500 (tuned)
prediction_winner_kknn_train$confusion 


task_winner_holdout <- TaskClassif$new(id="predict_winner2",backend=complete_data[train_indx,],target="First_wins")
#CV
resample <- rsmp(.key="cv")
cv_results_kknn <- resample(task_winner_holdout, learner_kknn, resample)
cv_results_kknn$aggregate(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.7871992 (untuned) VS. 0.836161
#ACC: 0.8938750 (untuned) VS. 0.918300




# untuned for new stat compare 0.7950244   0.8978500
#Tune
# for distance we use most common distances for kknn 
# for k we assume that the chance for each pokemon being in a battle is 1/800
# ... this will lead to 1/800*40000 = 50 expected battles of each pokemon
# ... so try with a number higher than 50 as maximum possible k

resample_inner <- rsmp("cv",folds=10)
measure <- msr("classif.acc")
param_set <- ParamSet$new(
  params = list(ParamInt$new("k",lower=1,upper=200),
                ParamInt$new("distance",lower=1,upper=3)
  )
)

terminator <- term("evals",n_evals=300)
tuner <- tnr("grid_search")
at_kknn <- AutoTuner$new(learner_kknn,resample_inner,measure,param_set,terminator,tuner)
resampling_outer = rsmp("cv",folds=3)
rr_kknn <- resample(task = task_winner_holdout, learner = at_kknn, resampling = resampling_outer)


rr_kknn$aggregate(msr("classif.acc"))




#test performance 
prediction_winner_kknn_test <- model_winner_kknn$predict(task_winner,row_ids = test_indx)
prediction_winner_kknn_test$score(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.7847276  (both effectivities, w.o. legendary, untuned) VS. 0.8324419 (tuned)
#ACC: 0.8926000 (both effectivities, w.o. legendary, untuned) VS. 0.9164000  (tuned)
prediction_winner_kknn_test$confusion 



########
#SVM-Modell
learner_svm<- lrn("classif.svm",type="C-classification",kernel="radial")
learner_svm$predict_type <- "prob"

model_winner_svm <- learner_svm$train(task_winner,row_ids = train_indx)

#train performance
prediction_winner_svm_train <- model_winner_svm$predict(task_winner,row_ids = train_indx)
prediction_winner_svm_train$score(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.8710978 (untuned)
#ACC: 0.9357750 (untuned)

prediction_winner_svm_train$confusion 

#CV
resample <- rsmp(.key="cv")
cv_results_svm <- resample(task_winner_holdout, learner_svm, resample)
cv_results_svm$aggregate(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.8563248 (untuned)
#ACC: 0.9284000 (untuned)


param_set_svm <- ParamSet$new(
  params = list(ParamDbl$new("cost",lower=10^(-5),upper=10^5),
                ParamDbl$new("gamma",lower=10^(-5),upper=10^5)
  )
)

possible_values <- c(10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),10^(0),10^(1),10^(2),10^(3),10^(4),10^(5))
no_possible_values <- length(possible_values)

design <- generate_design_grid(param_set_svm,resolution=11)
design$data$cost <-rep(possible_values,times=no_possible_values)
design$data$gamma <- rep(possible_values,each=no_possible_values)

terminator_svm <- term("evals",n_evals=150)
tuner <- tnr("design_points",design=design$data)
at_svm <- AutoTuner$new(learner_svm,resample_inner,measure,param_set_svm,terminator_svm,tuner)
resampling_outer = rsmp("cv",folds=3)
rr_svm <- resample(task = task_winner_holdout, learner = at_svm, resampling = resampling_outer)

######################
#VS model from keggle
combats_keggle <- read.csv("FightPokemon.csv",sep=",")
combats_keggle <- dplyr::select(combats_keggle,-c(X,Second_pokemon_legendary,First_pokemon_legendary))

task_winner2<- TaskClassif$new(id="predict_winner2",backend=combats_keggle,target="winner_first_label")
#kknn learner
learner_kknn2<- lrn("classif.kknn")
learner_kknn2$predict_type <- "prob"
model_winner_kknn2 <- learner_kknn$train(task_winner2)

prediction_winner2 <- model_winner_kknn2 $predict(task_winner2)
prediction_winner2$score(msr("classif.mcc")) # 0.9161688 
prediction_winner2$score(msr("classif.acc")) # 0.04184
prediction_winner2$confusion 




