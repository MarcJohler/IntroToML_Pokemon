remotes::install_github("mlr-org/mlr3verse")

library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(mlbench)
library(splines)
library(mlr3verse)
library(ggplot2)
library(mlr3)
library(mlbench)
install.packages('e1071', dependencies=TRUE)









####preparation data 


pokemon <- read.csv("combat_prediction_data.csv",sep=",")


new_data <- pokemon %>% dplyr::select(attackVSattack_diff,defenseVSdefense_diff,sp_atkVSsp_atk_diff,sp_defVSsp_def_diff,speedVSspeed_diff,
                                      HPVSHP_diff, First_wins,First_pokemon_legendary,Second_pokemon_legendary)

new_data$First_wins<-as.factor(new_data$First_wins)

new_data$First_pokemon_legendary<-as.logical(new_data$First_pokemon_legendary)
new_data$Second_pokemon_legendary<-as.logical(new_data$Second_pokemon_legendary)
#transform task features so that there are no more factors
newtask<-TaskClassif$new(id="First_wins",backend=new_data,target="First_wins")


set.seed(42)
all_indx <- 1:nrow(new_data)
train_indx <- sample.int(nrow(new_data),round(0.8*nrow(new_data)))
test_indx <- setdiff(all_indx,train_indx)

task_train<-TaskClassif$new(id="First_wins",backend=new_data[train_indx,],target="First_wins")




learner_glmnet <- lrn("classif.glmnet")
learner_glmnet$train(task_train,row_ids = train_indx)

pred_glmnet <- learner_glmnet$predict(task_train)
learner_glmnet$param_set$ids()
autoplot(pred_glmnet)
pred_glmnet$score()


set.seed(555)

# Tuning<<
resample_inner <- rsmp("cv",folds=5)
measures <- msr("classif.acc")

# tune minsplit and maxdepth and cp
param_set <- ParamSet$new(
  params = list(ParamDbl$new("alpha",lower=0.8,upper=0.9),
                ParamDbl$new("s",lower=0.05,upper=0.07),
                ParamDbl$new("eps",lower = 0.0000000000000004,upper=0.00000005)
  )
)


terminator <- term("evals",n_evals=100)
tuner <- tnr("grid_search")
at <- AutoTuner$new(learner_glmnet,resample_inner,measures,param_set,terminator,tuner)

resampling_outer = rsmp("cv",folds=3)
rr <- resample(task = task_train, learner = at, resampling = resampling_outer)
####cross validation : 
resample <- rsmp(.key="cv")
cv_results_svm <- resample(task_train, learner_glmnet, resample)
cv_results_svm$aggregate(msr("classif.acc")) 














####model
learner_baies <- lrn("classif.naive_bayes")

learner_baies$train(task_train,row_ids = train_indx)

pred_baies <- learner_baies$predict(task_train)
autoplot(pred_baies)


pred_baies$score()
learner_baies$param_set
set.seed(55445)
# Tuning
resample_inner <- rsmp("cv",folds=5)
measure <- msr("classif.acc")
# tune minsplit and maxdepth and cp
param_set <- ParamSet$new(
  params = list(ParamDbl$new("laplace",lower = 0,upper=1),
                ParamDbl$new("threshold",lower=0.04444,upper=1),
                ParamDbl$new("eps",lower=0.00000000001,upper = 0.000000001)
  )
)

terminator <- term("evals",n_evals=50)
tuner <- tnr("grid_search")
at <- AutoTuner$new(learner_baies,resample_inner,measure,param_set,terminator,tuner)

resampling_outer = rsmp("cv",folds=3)
rr <- resample(task = task_train, learner = at, resampling = resampling_outer)


resample <- rsmp(.key="cv")
cv_results_svm <- resample(task_train, learner_baies, resample)
cv_results_svm$aggregate(msr("classif.acc")) 









####### log reg : 





####model
learner_log <- lrn("classif.log_reg")

learner_log$train(task_train,row_ids = train_indx)

pred_log <- learner_log$predict(task_train)
autoplot(pred_log)


pred_log$score()
learner_log$param_set
set.seed(555)
# Tuning
resample_inner1 <- rsmp("cv",folds=5)
measures1 <- msr("classif.acc")
# tune minsplit and maxdepth and cp
param_set1 <- ParamSet$new(
  params = list(ParamInt$new("maxit",lower=10,upper=30),
                ParamDbl$new("epsilon",lower=0.000000000001,upper=0.000000000011)
  )
)

terminator1 <- term("evals",n_evals=20)
tuner1 <- tnr("grid_search")
at1 <- AutoTuner$new(learner_log,resample_inner1,measures1,param_set1,terminator1,tuner1)

resampling_outer1 = rsmp("cv",folds=3)
rr1 <- resample(task = task_train, learner = at1, resampling = resampling_outer1)


resample <- rsmp(.key="cv")
cv_results_svm <- resample(task_train, learner_log, resample)
cv_results_svm$aggregate(msr("classif.acc")) 




learner_glm<- lrn("classif.glmnet",alpha=0.85,s=0.06,eps=0.0000000045)
learner_logistic<-lrn("classif.log_reg",maxit=20,epsilon=0.0000000001)
learner_naive<-  lrn("classif.naive_bayes",laplace=0.5,threshold=0.5,eps=0.0000000001)
learner_kknn<- lrn("classif.kknn",k=45, distance=1)
learners<-list(learner_glm,learner_logistic,learner_naive,learner_kknn)

design<-benchmark_grid(tasks = task_train,learners = learners,resamplings =rsmp("cv", folds = 3))
bmr <-benchmark(design)
autoplot(bmr)

design$task


design$resampling

