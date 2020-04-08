pokemon_large <- read.csv("pokemon.csv")
pokemon_small <- read.csv("pokemon_alopez247.csv")
  
library(dplyr)
library(tidyr)
library(stringr)


#######################################
#Cleaning and Merging of both data sets
######################################
########
# 1: check if datasets are ordered the same way
pokemon_large_reduced <- pokemon_large[1:nrow(pokemon_small),]

sum(as.character(pokemon_large_reduced$name) != as.character(pokemon_small$Name)) # --> two names differ

pokemon_large_reduced$name_different <- as.character(pokemon_large_reduced$name) != as.character(pokemon_small$Name)
pokemon_small$name_different <- as.character(pokemon_large_reduced$name) != as.character(pokemon_small$Name)

pokemon_large_reduced %>% filter(name_different==TRUE)
pokemon_small %>% filter(name_different==TRUE) 
# --> same Pokemons, only format of name is different

######
# 2: select the required columns for both data sets 
# basic principle: if available in both data sets use data of pokemon_large_reduced since it's newer. 
# Exception: percentage_male - values are wrong in pokemon_large_reduced
pokemon_large_reduced <- pokemon_large_reduced %>% select(abilities,attack,base_egg_steps,base_happiness,base_total,capture_rate,classfication,defense,experience_growth,height_m,hp,name,pokedex_number,sp_attack,sp_defense,speed,type1,type2,weight_kg,generation,is_legendary)
pokemon_small <- pokemon_small %>% select(Color,hasGender,Pr_Male,Egg_Group_1,Egg_Group_2,hasMegaEvolution,Body_Style)

pokemon <- cbind(pokemon_large_reduced,pokemon_small)

######
# 3: standardise variable naming style
names(pokemon) <- c("abilities","attack","base_egg_steps","base_friendship","total","catch_rate","category","defense","experience_growth","height_m","hp","name","pokedex_number","sp_attack","sp_defense","speed","type_1","type_2","weight_kg","generation","is_legendary","color","has_gender","prob_male","egg_group_1","egg_group_2","has_mega_evolution","body_style")


#####
# 4: create new variables 

pokemon$catch_rate <- as.character(pokemon$catch_rate) %>% as.numeric()

# abilities
# has multiple possible values --> split it into different columns
abilities <- as.character(pokemon$abilities)
ability_list <- strsplit(abilities,",") 

for (i in 1:length(ability_list)){
  ability_list[[i]] <- str_remove_all(ability_list[[i]],fixed("['")) %>% str_remove_all(fixed("']")) %>% str_remove_all(fixed(" '")) %>% str_remove_all(fixed("'"))
}

no_of_abilities <- lapply(ability_list,length) %>% as.data.frame() %>% as.integer()

for (i in 1:max(no_of_abilities)){
  pokemon$new <- "none"
  names <- names(pokemon)
  names[length(names)] <- paste("ability",i,sep="_")
  names(pokemon) <- names
}


for (i in 1:nrow(pokemon)){
  k <- ability_list[[i]]
  n <- no_of_abilities[i]
  pokemon$ability_1[i] <- k[1]
  if (n>1){
    pokemon$ability_2[i] <- k[2]
    if (n>2){
      pokemon$ability_3[i] <- k[3]
      if (n>3){
        pokemon$ability_4[i] <- k[4]
        if (n>4){
          pokemon$ability_5[i] <- k[5]
          if (n>5){
            pokemon$ability_6[i] <- k[6]
          }
        }
      }
    }
  }
}

pokemon$ability_1 <- as.factor(pokemon$ability_1)
pokemon$ability_2 <- as.factor(pokemon$ability_2)
pokemon$ability_3 <- as.factor(pokemon$ability_3)
pokemon$ability_4 <- as.factor(pokemon$ability_4)
pokemon$ability_5 <- as.factor(pokemon$ability_5)
pokemon$ability_6 <- as.factor(pokemon$ability_6)

pokemon$no_of_abilities <- no_of_abilities

# category
pokemon$category <- pokemon$category %>% as.character() %>% str_remove_all(" PokÃ©mon") %>% as.factor()

# type_2
empty_type_2 <- pokemon$type_2 ==""
pokemon$type_2 <- as.character(pokemon$type_2)
pokemon[empty_type_2,]$type_2 <- "none"
pokemon$type_2 <- as.factor(pokemon$type_2)
pokemon$has_type_2 <- (!empty_type_2) %>% as.factor()

# Since hasGender is a probability for non-genders. we can just assume that Pr_Male for non_genders is 0, Pr_Female is redundant because it can be caculated with Pr_Male and hasGender
pokemon$prob_male <- pokemon$prob_male %>% replace_na(0)

# egg_group_2
empty_egg_group_2 <- pokemon$egg_group_2 ==""
pokemon$egg_group_2 <- as.character(pokemon$egg_group_2)
pokemon[empty_egg_group_2,]$egg_group_2 <- "none"
pokemon$egg_group_2 <- as.factor(pokemon$egg_group_2)
pokemon$has_egg_group_2 <- (!empty_egg_group_2) %>% as.factor()

#is_legendary
pokemon$is_legendary <- pokemon$is_legendary %>% as.logical() %>% as.factor()

######
# 5: re-order variables for easier usage

pokemon <- pokemon %>% select("pokedex_number","name","generation","category","egg_group_1","egg_group_2","has_egg_group_2","color","body_style","height_m","weight_kg","abilities","ability_1","ability_2","ability_3","ability_4","ability_5","ability_6","no_of_abilities","has_mega_evolution","catch_rate","base_egg_steps","base_friendship","experience_growth","has_gender","prob_male","type_1","type_2","has_type_2","is_legendary","hp","attack","defense","sp_attack","sp_defense","speed","total")



