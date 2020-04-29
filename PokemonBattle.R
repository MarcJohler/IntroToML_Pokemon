library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(xgboost)
library(glmnet)
library(gamlss)
library(mlr3verse)

combats <- read.csv("combats.csv",sep=",")
pokemon <- read.csv("pokemon_battle_stats.csv",sep=",")

names(pokemon) <- str_replace_all(names(pokemon),fixed("."),"")

#find names
combats$First_pokemon_name<-sapply(combats$First_pokemon, function(x) pokemon$Name[match(x, pokemon$X)])
combats$Second_pokemon_name<-sapply(combats$Second_pokemon, function(x) pokemon$Name[match(x, pokemon$X)])
combats$Winner_name<-sapply(combats$Winner, function(x) pokemon$Name[match(x, pokemon$X)])
combats$First_wins <- combats$First_pokemon == combats$Winner

# calculate stat differences
combats$First_pokemon_attack<-sapply(combats$First_pokemon, function(x) pokemon$Attack[match(x, pokemon$X)])
combats$Second_pokemon_attack<-sapply(combats$Second_pokemon, function(x) pokemon$Attack[match(x, pokemon$X)])
combats$First_pokemon_HP<-sapply(combats$First_pokemon, function(x) pokemon$HP[match(x, pokemon$X)])
combats$Second_pokemon_HP<-sapply(combats$Second_pokemon, function(x) pokemon$HP[match(x, pokemon$X)])
combats$First_pokemon_defense<-sapply(combats$First_pokemon, function(x) pokemon$Defense[match(x, pokemon$X)])
combats$Second_pokemon_defense<-sapply(combats$Second_pokemon, function(x) pokemon$Defense[match(x, pokemon$X)])
combats$First_pokemon_sp_atk<-sapply(combats$First_pokemon, function(x) pokemon$SpAtk[match(x, pokemon$X)])
combats$Second_pokemon_sp_atk<-sapply(combats$Second_pokemon, function(x) pokemon$SpAtk[match(x, pokemon$X)])
combats$First_pokemon_sp_def<-sapply(combats$First_pokemon, function(x) pokemon$SpDef[match(x, pokemon$X)])
combats$Second_pokemon_sp_def<-sapply(combats$Second_pokemon, function(x) pokemon$SpDef[match(x, pokemon$X)])
combats$First_pokemon_speed<-sapply(combats$First_pokemon, function(x) pokemon$Speed[match(x, pokemon$X)])
combats$Second_pokemon_speed<-sapply(combats$Second_pokemon, function(x) pokemon$Speed[match(x, pokemon$X)])

combats$attackVSdefense <- combats$First_pokemon_attack - combats$Second_pokemon_defense
combats$defenseVSattack <- combats$First_pokemon_defense - combats$Second_pokemon_attack
combats$sp_atkVSsp_def <- combats$First_pokemon_sp_atk - combats$Second_pokemon_sp_def
combats$sp_defVSsp_atk <- combats$First_pokemon_sp_def - combats$Second_pokemon_sp_atk
combats$speedVSspeed <- combats$First_pokemon_speed - combats$Second_pokemon_speed
combats$HPVSHP <- combats$First_pokemon_HP - combats$Second_pokemon_HP

#add legendary status
combats$First_pokemon_legendary<-sapply(combats$First_pokemon, function(x) pokemon$Legendary[match(x, pokemon$X)])
combats$Second_pokemon_legendary<-sapply(combats$Second_pokemon, function(x) pokemon$Legendary[match(x, pokemon$X)])

#add types and mapping
combats$First_pokemon_type1<-sapply(combats$First_pokemon, function(x) pokemon$Type1[match(x, pokemon$X)])
combats$First_pokemon_type1_indx <- sapply(combats$First_pokemon_type1, function(x) type_mapping$index[match(x, type_mapping$type)])

combats$First_pokemon_type2<-sapply(combats$First_pokemon, function(x) pokemon$Type2[match(x, pokemon$X)])
empty_type_2 <- combats$First_pokemon_type2 ==""
combats$First_pokemon_type2 <- as.character(combats$First_pokemon_type2)
combats[empty_type_2,]$First_pokemon_type2  <- "None"
combats$First_pokemon_type2 <- as.factor(combats$First_pokemon_type2)
combats$First_pokemon_type2_indx <- sapply(combats$First_pokemon_type2, function(x) type_mapping$index[match(x, type_mapping$type)])

combats$Second_pokemon_type1<-sapply(combats$Second_pokemon, function(x) pokemon$Type1[match(x, pokemon$X)])
combats$Second_pokemon_type1_indx <- sapply(combats$Second_pokemon_type1, function(x) type_mapping$index[match(x, type_mapping$type)])

combats$Second_pokemon_type2<-sapply(combats$Second_pokemon, function(x) pokemon$Type2[match(x, pokemon$X)])
empty_type_2 <- combats$Second_pokemon_type2 ==""
combats$Second_pokemon_type2 <- as.character(combats$Second_pokemon_type2)
combats[empty_type_2,]$Second_pokemon_type2  <- "None"
combats$Second_pokemon_type2 <- as.factor(combats$Second_pokemon_type2)
combats$Second_pokemon_type2_indx <- sapply(combats$Second_pokemon_type2, function(x) type_mapping$index[match(x, type_mapping$type)])

#create double type and mapping
combats$First_pokemon_double_type <- paste(combats$First_pokemon_type1,combats$First_pokemon_type2,sep="_")
combats$First_pokemon_double_type_indx <- sapply(combats$First_pokemon_double_type, function(x) double_type_mapping$index[match(x, double_type_mapping$double_type)])
combats$Second_pokemon_double_type <- paste(combats$Second_pokemon_type1,combats$Second_pokemon_type2,sep="_")
combats$Second_pokemon_double_type_indx <- sapply(combats$Second_pokemon_double_type, function(x) double_type_mapping$index[match(x, double_type_mapping$double_type)])

#lookup attack effectivity for each pokemon of each battle
combats$First_pokemon_effectivity <- 99
for (i in 1:nrow(combats)){
  combats$First_pokemon_effectivity[i] <- double_type_table[combats$First_pokemon_type1_indx,combats$Second_pokemon_double_type_indx]
  if(combats$First_pokemon_type2!="None"){
    alt_effectivity <- double_type_table[combats$First_pokemon_type2_indx,combats$Second_pokemon_double_type_indx]
    if (alt_effectivity > combats$First_pokemon_effectivity[i]){
      combats$First_pokemon_effectivity[i] <- alt_effectivity
    }
  }
  print(i)
}

combats$Second_pokemon_effectivity <- 99
for (i in 1:nrow(combats)){
  combats$Second_pokemon_effectivity[i] <- double_type_table[combats$Second_pokemon_type1_indx,combats$First_pokemon_double_type_indx]
  if(combats$Second_pokemon_type2!="None"){
    alt_effectivity <- double_type_table[combats$Second_pokemon_type2_indx,combats$First_pokemon_double_type_indx]
    if (alt_effectivity > combats$Second_pokemon_effectivity[i]){
      combats$Second_pokemon_effectivity[i] <- alt_effectivity
    }
  }
  print(i)
}

#####################
#try out
classif_data <- combats %>% dplyr::select(attackVSdefense,defenseVSattack,sp_atkVSsp_def,sp_defVSsp_atk,speedVSspeed,HPVsHP,First_wins)
classif_data$First_wins <- as.factor(classif_data$First_wins)

task_winner<- TaskClassif$new(id="predict_winner",backend=classif_data,target="First_wins")

#kknn learner
learner_kknn<- lrn("classif.kknn")
learner_kknn$predict_type <- "prob"
model_winner_kknn <- learner_kknn$train(task_winner)

prediction_winner <- model_winner_kknn$predict(task_winner)
prediction_winner$score(msr("classif.mcc")) # 0.9149449 
prediction_winner$score() #0.04244 
prediction_winner$confusion 

#VS model from keggle
combats_keggle <- read.csv("FightPokemon.csv",sep=",")
combats_keggle <- dplyr::select(combats_keggle,-c(X,Second_pokemon_legendary,First_pokemon_legendary))

task_winner2<- TaskClassif$new(id="predict_winner2",backend=combats_keggle,target="winner_first_label")
#kknn learner
learner_kknn<- lrn("classif.kknn")
learner_kknn$predict_type <- "prob"
model_winner_kknn2 <- learner_kknn$train(task_winner2)

prediction_winner2 <- model_winner_kknn2 $predict(task_winner2)
prediction_winner2$score(msr("classif.mcc")) # 0.9161688 
prediction_winner2$score() # 0.04184
prediction_winner2$confusion 


##compare importance measures
information_gain1 <- flt("importance")
information_gain2 <- flt("importance")
my_scores <- information_gain1$calculate(task_winner)
keggle_scores <- information_gain2$calculate(task_winner2)

########################


#find the type/legendary for the winner

combats$winner_legendary<-sapply(combats$Winner_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])

