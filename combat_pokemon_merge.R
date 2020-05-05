library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)

atk<-c('Normal','Fire','Water','Electric','Grass','Ice','Fighting','Poison','Ground','Flying','Psychic','Bug','Rock','Ghost','Dragon','Dark','Steel','Fairy')
normal<-c(1,1,1,1,1,1,2,1,1,1,1,1,1,0,1,1,1,1)
fire<-c(1,0.5,2,1,0.5,0.5,1,1,2,1,1,0.5,2,1,1,1,0.5,0.5)
water<-c(1,0.5,0.5,2,2,0.5,1,1,1,1,1,1,1,1,1,1,0.5,1)
elec<-c(1,1,1,0.5,1,1,1,1,2,0.5,1,1,1,1,1,1,0.5,1)
grass<-c(1,2,0.5,0.5,0.5,2,1,2,0.5,2,1,2,1,1,1,1,1,1)
ice<-c(1,2,1,1,1,0.5,2,1,1,1,1,1,2,1,1,1,2,1)
fighting<-c(1,1,1,1,1,1,1,1,1,2,2,0.5,0.5,1,1,0.5,1,2)
poison<-c(1,1,1,1,0.5,1,0.5,0.5,2,1,2,0.5,1,1,1,1,1,0.5)
ground<-c(1,1,2,0,2,2,1,0.5,1,1,1,1,0.5,1,1,1,1,1)
flying<-c(1,1,1,2,0.5,2,0.5,1,0,1,1,0.5,2,1,1,1,1,1)
psychic<-c(1,1,1,1,1,1,0.5,1,1,1,0.5,2,1,2,1,2,1,1)
bug<-c(1,2,1,1,0.5,1,0.5,1,0.5,2,1,1,2,1,1,1,1,1)
rock<-c(0.5,0.5,2,1,2,1,2,0.5,2,0.5,1,1,1,1,1,1,2,1)
ghost<-c(0,1,1,1,1,1,0,0.5,1,1,1,0.5,1,2,1,2,1,1)
dragon<-c(1,0.5,0.5,0.5,0.5,2,1,1,1,1,1,1,1,1,2,1,1,2)
dark<-c(1,1,1,1,1,1,2,1,1,1,0,2,1,0.5,1,0.5,1,2)
steel<-c(0.5,2,1,1,0.5,0.5,2,0,2,0.5,0.5,0.5,0.5,1,0.5,1,0.5,0.5)
fairy<-c(1,1,1,1,1,1,0.5,2,1,1,1,0.5,1,1,0,0.5,2,1)
none <- rep(1.0,18)
mytable<-data.frame(Attacking=atk,Normal=normal,Fire=fire,Water=water,Electric=elec,Grass=grass,Ice=ice,Fighting=fighting,Poison=poison,Ground=ground,Flying=flying,Psychic=psychic,Bug=bug,Rock=rock,Ghost=ghost,Dragon=dragon,Dark=dark,Steel=steel,Fairy=fairy,None=none)

#t1 <- ttheme_default(base_size=8,core=list(bg_params = list(fill=c('#A8A77A','#EE8130','#6390F0','#F7D02C','#7AC74C','#96D9D6','#C22E28','#A33EA1','#E2BF65','#A98FF3','#F95587','#A6B91A','#B6A136','#735797','#6F35FC','#705746','#B7B7CE','#D685AD','white'))))

#grid.table(mytable,theme=t1)

all_types <-names(mytable)[-1]
double_types <- data.frame(type_1=rep(all_types[1:18],each=length(all_types)),type_2=rep(all_types,times=length(all_types)-1))

double_types$dropit <- as.character(double_types$type_1) == as.character(double_types$type_2)
double_types <- double_types %>% filter(dropit==FALSE) %>% dplyr::select(type_1,type_2)
double_types_names <- paste(double_types$type_1,double_types$type_2,sep="_")

# calculate resistances of double types
double_type_table <- mytable %>% dplyr::select(Attacking)
double_type_pointer <- 1

for (i in 1:(ncol(mytable)-2)){
  for (j in 1:(ncol(mytable)-1)){
    if (i != j){
      double_type_table$newdoubletype <- mytable[,i+1]*mytable[,j+1]
      names(double_type_table)[ncol(double_type_table)] <- double_types_names[double_type_pointer]
      double_type_pointer <- double_type_pointer + 1
    }
  }
}

# create mappings for types and double types
double_type_mapping <- data.frame(double_type=double_types_names,index=2:(length(double_types_names)+1))
type_mapping <- data.frame(type=all_types,index=1:length(all_types))

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

combats$attackVSdefense <- combats$First_pokemon_attack/combats$Second_pokemon_defense
combats$defenseVSattack <- combats$First_pokemon_defense/combats$Second_pokemon_attack
combats$sp_atkVSsp_def <- combats$First_pokemon_sp_atk/combats$Second_pokemon_sp_def
combats$sp_defVSsp_atk <- combats$First_pokemon_sp_def/combats$Second_pokemon_sp_atk
combats$speedVSspeed <- combats$First_pokemon_speed/combats$Second_pokemon_speed
combats$HPVSHP <- combats$First_pokemon_HP/combats$Second_pokemon_HP

combats$attackVSattack_diff <- combats$First_pokemon_attack-combats$Second_pokemon_attack
combats$defenseVSdefense_diff <- combats$First_pokemon_defense-combats$Second_pokemon_defense
combats$sp_atkVSsp_atk_diff <- combats$First_pokemon_sp_atk-combats$Second_pokemon_sp_atk
combats$sp_defVSsp_def_diff <- combats$First_pokemon_sp_def-combats$Second_pokemon_sp_def
combats$speedVSspeed_diff <- combats$First_pokemon_speed-combats$Second_pokemon_speed
combats$HPVSHP_diff <- combats$First_pokemon_HP-combats$Second_pokemon_HP

#first Pokemon faster?
combats$First_pokemon_faster <- sign(combats$speedVSspeed_diff)

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
  combats$First_pokemon_effectivity[i] <- double_type_table[combats$First_pokemon_type1_indx[i],combats$Second_pokemon_double_type_indx[i]]
  if(combats$First_pokemon_type2[i]!="None"){
    alt_effectivity <- double_type_table[combats$First_pokemon_type2_indx[i],combats$Second_pokemon_double_type_indx[i]]
    if (alt_effectivity > combats$First_pokemon_effectivity[i]){
      combats$First_pokemon_effectivity[i] <- alt_effectivity
    }
  }
}

combats$Second_pokemon_effectivity <- 99
for (i in 1:nrow(combats)){
  combats$Second_pokemon_effectivity[i] <- double_type_table[combats$Second_pokemon_type1_indx[i],combats$First_pokemon_double_type_indx[i]]
  if(combats$Second_pokemon_type2[i]!="None"){
    alt_effectivity <- double_type_table[combats$Second_pokemon_type2_indx[i],combats$First_pokemon_double_type_indx[i]]
    if (alt_effectivity > combats$Second_pokemon_effectivity[i]){
      combats$Second_pokemon_effectivity[i] <- alt_effectivity
    }
  }
}

# other attempt. just calculate average resistance with included STAB-bonus
combats$First_pokemon_avg_weakness <- 99
for (i in 1:nrow(combats)){
  calculation_vector <- double_type_table[combats$First_pokemon_double_type_indx[i]]
  calculation_vector[combats$Second_pokemon_type1_indx[i],] <- calculation_vector[combats$Second_pokemon_type1_indx[i],] * 1.5
  if(combats$Second_pokemon_type2[i]!="None"){
    calculation_vector[combats$Second_pokemon_type2_indx[i],] <- calculation_vector[combats$Second_pokemon_type2_indx[i],] * 1.5
  }
  combats$First_pokemon_avg_weakness[i] <- calculation_vector[[1]] %>% mean()
}

combats$Second_pokemon_avg_weakness <- 99
for (i in 1:nrow(combats)){
  calculation_vector <- double_type_table[combats$Second_pokemon_double_type_indx[i]]
  calculation_vector[combats$First_pokemon_type1_indx[i],] <- calculation_vector[combats$First_pokemon_type1_indx[i],] * 1.5
  if(combats$First_pokemon_type2[i]!="None"){
    calculation_vector[combats$First_pokemon_type2_indx[i],] <- calculation_vector[combats$First_pokemon_type2_indx[i],] * 1.5
  }
  combats$Second_pokemon_avg_weakness[i] <- calculation_vector [[1]] %>% mean()
}


#combats$effectivity_advantage <- (combats$First_pokemon_effectivity-combats$Second_pokemon_effectivity)

# full data - only necessary for shiny dashboard
path <- getwd()
write.csv(combats,paste(path,"combat_full_data.csv",sep="/"),row.names=TRUE)

#drop helping variables
combats$First_wins <- combats$First_wins  %>% as.factor()
combats <- combats[!combats$First_pokemon_faster == 0,]

complete_data <- combats %>% dplyr::select(attackVSattack_diff,defenseVSdefense_diff,sp_atkVSsp_atk_diff,sp_defVSsp_def_diff,speedVSspeed_diff,First_pokemon_faster,HPVSHP_diff,First_pokemon_legendary,Second_pokemon_legendary,First_wins)

#save data set for prediction
path <- getwd()
write.csv(combats,paste(path,"combat_prediction_data.csv",sep="/"),row.names=TRUE)
