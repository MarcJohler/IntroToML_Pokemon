library(dplyr)
library(tidyr)
library(stringr)
library(mlr3verse)

#get rid of fully correlated features
full_data <- pokemon %>% dplyr::select(individual_ability,egg_group_1,egg_group_2,color,body_style,height_m,weight_kg,catch_rate,base_egg_steps,base_friendship,experience_growth,type_1,type_2,gender,generation,has_mega_evolution,is_legendary,total)
full_data$is_legendary <- as.double(full_data$is_legendary)
task_total_full <- TaskRegr$new(id="predict_full",backend=full_data,target="total")

#reduce factor levels and transform to binary variables
fact_reducer = PipeOpCollapseFactors$new(id="collapse",param_vals=list(target_level_count=20))
task_total_full <- fact_reducer$train(list(task_total_full))$output
fact_encoder = po("encode", method = "treatment",affect_columns = selector_type("factor"))
task_total_full <- fact_encoder$train(list(task_total_full))$output

#calculate information_gain for each variable and sepparate according to factor 
information_gain <- flt("information_gain")
information_gain_scores <- information_gain$calculate(task_total_full) %>% as.data.table()
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