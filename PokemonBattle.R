library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(xgboost)
library(glmnet)
library(gamlss)
library(mlr3verse)
library(plotly)
library(precrec)

classif_data <- read.csv("combat_prediction_data.csv",sep=",")
classif_data <- classif_data %>% dplyr::select(attackVSattack_diff,sp_atkVSsp_atk_diff,sp_defVSsp_def_diff,speedVSspeed_diff,HPVSHP_diff,First_wins) 
classif_data$First_wins <- as.factor(classif_data$First_wins)

task_winner<- TaskClassif$new(id="predict_winner",backend=classif_data,target="First_wins")


#kknn learner
learner_kknn<- lrn("classif.kknn",k=23,distance=1)
learner_kknn$predict_type <- "prob"
model_winner_kknn <- learner_kknn$train(task_winner)

prediction_winner_kknn <- model_winner_kknn$predict(task_winner)
prediction_winner_kknn$score(c(msr("classif.mcc"),msr("classif.acc"))) 
# 0.9206860 (untuned) VS. 0.8875478 (tuned)
# 0.9603057 (untuned) VS. 0.9436226 (tuned)
prediction_winner_kknn$confusion 



#svm learner
learner_svm<- lrn("classif.svm")
learner_svm$predict_type <- "prob"
model_winner_svm <- learner_svm$train(task_winner)

prediction_winner_svm <- model_winner_svm$predict(task_winner)
prediction_winner_svm$score(c(msr("classif.mcc"),msr("classif.acc"))) 
prediction_winner_svm$confusion 

# --> svm takes very long and doesn't have better results --> focus on KKNN


###########################
#KKNN-Approach

#split data into train and test
set.seed(42)
all_indx <- 1:nrow(classif_data)
train_indx <- sample.int(nrow(classif_data),round(0.8*nrow(classif_data)))
test_indx <- setdiff(all_indx,train_indx)

model_winner_kknn <- learner_kknn$train(task_winner,row_ids = train_indx)

#train performance
prediction_winner_kknn_train <- model_winner_kknn$predict(task_winner,row_ids = train_indx)
prediction_winner_kknn_train$score(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.9190261 (untuned) VS. 0.8875035 (tuned)
#ACC: 0.9594740 (untuned) VS. 0.9436027 (tuned)
prediction_winner_kknn_train$confusion 


task_winner_holdout <- TaskClassif$new(id="predict_winner2",backend=classif_data[train_indx,],target="First_wins")
#CV
resample <- rsmp(.key="cv")
cv_results_kknn <- resample(task_winner_holdout, learner_kknn, resample)
cv_results_kknn$aggregate(c(msr("classif.mcc"),msr("classif.acc"))) 
#MCC: 0.8648811 (untuned) VS. 0.8794818 (tuned)
#ACC: 0.9324308 (untuned) VS. 0.9395962 (tuned)


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

terminator <- term("none")
tuner <- tnr("grid_search")
at_kknn <- AutoTuner$new(learner_kknn,resample_inner,measure,param_set,terminator,tuner)
resampling_outer = rsmp("cv",folds=3)
rr_kknn <- resample(task = task_winner_holdout, learner = at_kknn, resampling = resampling_outer)
#k=23, distance=1 
#classif.acc=0.9398

#oder Hyeoung dataset
#k=23, distance=2
#classif.acc=0.9398 

rr_kknn$aggregate(msr("classif.acc"))
#0.9392881
#0.9392624 (hyeyoung dataset)


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
# Visualisation

#visualisation approach

prediction_winner_kknn_test <- model_winner_kknn$predict(task_winner,row_ids = train_indx)

compare_prob <- function(prediction){
  plot_data <- data.frame(truth=prediction$truth,prob=prediction$prob[,2])
  prob_classes_levels <- c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")
  plot_data$prob_classes <- factor(NA,levels=prob_classes_levels)
  plot_data$mapping <- ceiling(plot_data$prob*10)
  plot_data[plot_data$mapping==0,]$mapping <- 1
  for (i in 1:nrow(plot_data)){
    plot_data$prob_classes[i]<- prob_classes_levels[plot_data$mapping[i]]
  }
  
  histogram_data <- plot_data %>% group_by(prob_classes) %>%
    mutate(proportion_First_wins=mean(as.numeric(truth)-1),
           estimated_First_wins=mean(prob),
           class_size=length(prob)) %>%
    dplyr::select(prob_classes,class_size,estimated_First_wins,proportion_First_wins) %>%
    unique()
  
  histogram_data <- histogram_data[order(histogram_data$prob_classes),]
  
  plot_result <- ggplot(histogram_data) + geom_point(aes(x=estimated_First_wins,y=proportion_First_wins)) +
    geom_line(aes(x=proportion_First_wins,y=proportion_First_wins,group=1),color="red")+
    geom_line(aes(x=proportion_First_wins,y=proportion_First_wins+0.1,group=1),color="orange")+
    geom_line(aes(x=proportion_First_wins,y=proportion_First_wins-0.1,group=1),color="orange")

  return(list(histogram_data,plot_result))
}

## visualisation of percentag for single battle
pokemon <- read.csv("pokemon_battle_stats.csv",sep=",")

# missing Name value for pokemon
pokemon$Name <- as.character(pokemon$Name)
pokemon[pokemon$Name=="",]$Name <- "Primeape"
pokemon$Name <- as.factor(pokemon$Name)

#use better format for variable names
names(pokemon) <- str_replace_all(names(pokemon),fixed("."),"")
pokemonA <- 122
pokemonB <- 457

remember_prob <- as.double(NA)
percentage_plot <- function(pokemonA,pokemonB){
  data_for_classification <- data.frame(row.names=1)
  data_for_classification$First_pokemon <- pokemonA
  data_for_classification$Second_pokemon <- pokemonB
  
  # calculate stat differences
  data_for_classification$First_pokemon_attack<-sapply(data_for_classification$First_pokemon, function(x) pokemon$Attack[match(x, pokemon$X)])
  data_for_classification$Second_pokemon_attack<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$Attack[match(x, pokemon$X)])
  data_for_classification$First_pokemon_HP<-sapply(data_for_classification$First_pokemon, function(x) pokemon$HP[match(x, pokemon$X)])
  data_for_classification$Second_pokemon_HP<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$HP[match(x, pokemon$X)])
  data_for_classification$First_pokemon_defense<-sapply(data_for_classification$First_pokemon, function(x) pokemon$Defense[match(x, pokemon$X)])
  data_for_classification$Second_pokemon_defense<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$Defense[match(x, pokemon$X)])
  data_for_classification$First_pokemon_sp_atk<-sapply(data_for_classification$First_pokemon, function(x) pokemon$SpAtk[match(x, pokemon$X)])
  data_for_classification$Second_pokemon_sp_atk<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$SpAtk[match(x, pokemon$X)])
  data_for_classification$First_pokemon_sp_def<-sapply(data_for_classification$First_pokemon, function(x) pokemon$SpDef[match(x, pokemon$X)])
  data_for_classification$Second_pokemon_sp_def<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$SpDef[match(x, pokemon$X)])
  data_for_classification$First_pokemon_speed<-sapply(data_for_classification$First_pokemon, function(x) pokemon$Speed[match(x, pokemon$X)])
  data_for_classification$Second_pokemon_speed<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$Speed[match(x, pokemon$X)])
  
  data_for_classification$attackVSattack_diff <- data_for_classification$First_pokemon_attack-data_for_classification$Second_pokemon_attack
  data_for_classification$defenseVSdefense_diff <- data_for_classification$First_pokemon_defense-data_for_classification$Second_pokemon_defense
  data_for_classification$sp_atkVSsp_atk_diff <- data_for_classification$First_pokemon_sp_atk-data_for_classification$Second_pokemon_sp_atk
  data_for_classification$sp_defVSsp_def_diff <- data_for_classification$First_pokemon_sp_def-data_for_classification$Second_pokemon_sp_def
  data_for_classification$speedVSspeed_diff <- data_for_classification$First_pokemon_speed-data_for_classification$Second_pokemon_speed
  data_for_classification$HPVSHP_diff <- data_for_classification$First_pokemon_HP-data_for_classification$Second_pokemon_HP
  
  #first Pokemon faster?
  data_for_classification$First_pokemon_faster <- sign(data_for_classification$speedVSspeed_diff)
  
  #add legendary status
  data_for_classification$First_pokemon_legendary<-sapply(data_for_classification$First_pokemon, function(x) pokemon$Legendary[match(x, pokemon$X)])%>%as.logical()%>%as.factor()
  data_for_classification$Second_pokemon_legendary<-sapply(data_for_classification$Second_pokemon, function(x) pokemon$Legendary[match(x, pokemon$X)])%>%as.logical()%>%as.factor()
  
  #select the variables you need for your specific model
  data_for_classification <- data_for_classification %>% dplyr::select(attackVSattack_diff,defenseVSdefense_diff,sp_atkVSsp_atk_diff,sp_defVSsp_def_diff,HPVSHP_diff,First_pokemon_faster)
  
  prediction <- model_winner_kknn$predict_newdata(newdata=data_for_classification)
  prob <- prediction$prob[,2] 
  
  
  fig <- plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = prob,
    title = list(text = "Probability for First Pokemon to win", font = list(size = 30)),
    delta = list(reference = remember_prob, increasing = list(color = "lightgreen"),decreasing=list(color="red")),
    gauge = list(
      axis = list(range = list(0, 1), tickwidth = 1, tickcolor = "black"),
      bar = list(color = "black"),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "gray",
      steps = list(
        list(range = c(0, 0.2), color = "red"),
        list(range = c(0.2,0.4), color = "orange"),
        list(range = c(0.4,0.6), color = "yellow"),
        list(range = c(0.6,0.8), color = "chartreuse"),                                   
        list(range = c(0.8,1), color = "darkgreen")))) 
  fig <- fig %>%
    layout(
      margin = list(l=20,r=30),
      paper_bgcolor = "lavender",
      font = list(color = "black", family = "Arial",size=20))
  
  remember_prob <<- prob[1] %>% as.numeric()
  fig
}

percentage_plot(17,788)# >0
percentage_plot(144,218) # < 1
percentage_plot(566,23) # = 0


## ROC-Curves of different learners

autoplot(prediction_winner_kknn_test,type="roc")

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




