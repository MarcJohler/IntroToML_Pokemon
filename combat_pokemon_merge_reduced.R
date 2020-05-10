library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)

# read data sets from keggle
combats <- read.csv("combats.csv",sep=",")
pokemon <- read.csv("pokemon_battle_stats.csv",sep=",")

# missing Name value for pokemon
pokemon$Name <- as.character(pokemon$Name)
pokemon[pokemon$Name=="",]$Name <- "Primeape"
pokemon$Name <- as.factor(pokemon$Name)

#use better format for variable names
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

combats$attackVSattack_diff <- combats$First_pokemon_attack-combats$Second_pokemon_attack
combats$defenseVSdefense_diff <- combats$First_pokemon_defense-combats$Second_pokemon_defense
combats$sp_atkVSsp_atk_diff <- combats$First_pokemon_sp_atk-combats$Second_pokemon_sp_atk
combats$sp_defVSsp_def_diff <- combats$First_pokemon_sp_def-combats$Second_pokemon_sp_def
combats$speedVSspeed_diff <- combats$First_pokemon_speed-combats$Second_pokemon_speed
combats$HPVSHP_diff <- combats$First_pokemon_HP-combats$Second_pokemon_HP

#first Pokemon faster?
combats$First_pokemon_faster <- sign(combats$speedVSspeed_diff)

#add legendary status
combats$First_pokemon_legendary<-sapply(combats$First_pokemon, function(x) pokemon$Legendary[match(x, pokemon$X)])%>%as.logical()%>%as.factor()
combats$Second_pokemon_legendary<-sapply(combats$Second_pokemon, function(x) pokemon$Legendary[match(x, pokemon$X)])%>%as.logical()%>%as.factor()

# full data - only necessary for shiny dashboard
path <- getwd()
write.csv(combats,paste(path,"combat_full_data.csv",sep="/"),row.names=TRUE)

#drop helping variables
combats$First_wins <- combats$First_wins  %>% as.factor()
combats <- combats[!combats$First_pokemon_faster == 0,]

combats <- combats %>% dplyr::select(attackVSattack_diff,defenseVSdefense_diff,sp_atkVSsp_atk_diff,sp_defVSsp_def_diff,speedVSspeed_diff,HPVSHP_diff,First_pokemon_faster,First_pokemon_legendary,Second_pokemon_legendary,First_wins)
combats$First_wins <- as.factor(combats$First_wins)


#save data set for prediction
path <- getwd()
write.csv(combats,paste(path,"combat_prediction_data.csv",sep="/"),row.names=TRUE)
