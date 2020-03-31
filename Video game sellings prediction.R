library(dplyr)
library(tidyr)
library(stringr)
library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlbench)
library(mlr3viz)
library(kknn)
library(ggplot2)


data <- read.csv("vgsales.csv")

# global sales volume is skewed to the right --> log trafo
data$Global_Sales_log <- log2(data$Global_Sales)

# normal distributed 
hist(data$Global_Sales,breaks=seq(min(data$Global_Sales),max(data$Global_Sales)+0.05,0.05))

data$no_of_platforms <- 0
namefreq <- table(data$Name)
namefreq_names <- names(namefreq)

for (i in 1:length(namefreq_names)){
  name_indx <- data$Name==namefreq_names[i]
  data[name_indx,]$no_of_platforms <- namefreq[i] %>% as.integer()
}

#too many publishers

data$Publisher <- as.character(data$Publisher)

publisher_na <- data$Publisher == "N/A"
data$Publisher <- replace(data$Publisher,publisher_na,"other")

publisherfreq <- table(data$Publisher)
other_indx <- publisherfreq < 50
publisher_other <- publisherfreq[other_indx]
publisherfreq_names <- names(publisher_other)

for (i in 1:length(publisherfreq_names)){
  name_indx <- data$Publisher==publisherfreq_names[i]
  data[name_indx,]$Publisher <- rep("other",publisher_other[i])
}

data$Publisher <- as.factor(data$Publisher)

#NAs in Year variable --> assume missing at random --> naive approach sampling from a uniform distribution within the observed values

data$Year <- as.character(data$Year)

year_na <- data$Year == "N/A"
data$Year <- replace(data$Year,year_na,NA)

data$Year <- as.integer(data$Year)
known_year <- (data %>% filter(!is.na(Year)))$Year
unknown_years <- nrow(data)-length(known_year)

randomized_year <- sample(min(known_year):max(known_year),replace=TRUE,size=unknown_years)
year_na_indx <- (1:nrow(data))[is.na(data$Year)]

j <- 1
for (i in year_na_indx){
  data[i,]$Year <- randomized_year[j]
  j <- j+1
}


# Feature transformations
data$no_of_platforms_sqrd <- data$no_of_platforms^2
data$Year_sqrd <- data$Year^2


# key-words in name
sum(str_detect(data$Name,"Mario")) #113
sum(str_detect(data$Name,"Pokemon")) #35
sum(str_detect(data$Name,"PokÃ©mon")) #13
sum(str_detect(data$Name,"Final Fantasy"))#89 #89
sum(str_detect(data$Name,"Elder Scrolls"))#10
sum(str_detect(data$Name,"Zelda"))#27
sum(str_detect(data$Name,"Soccer"))#222
sum(str_detect(data$Name,"Tetris"))#29
sum(str_detect(data$Name,"Grand Theft Auto"))#28
sum(str_detect(data$Name,"Kinect"))#13
sum(str_detect(data$Name,"Call of Duty"))#59
sum(str_detect(data$Name,"Mario Kart"))#9
sum(str_detect(data$Name,"Super Mario"))#34
sum(str_detect(data$Name,"Battlefield"))#28
sum(str_detect(data$Name,"Animal Crossing"))#6
sum(str_detect(data$Name,"Halo"))#14
sum(str_detect(data$Name,"Gran Turismo"))#10
#...

#as vector
keywords <- c("Mario","Pokemon","Final Fantasy","Elder Scrolls","Zelda","Soccer","Tetris","Grand Theft Auto","Kinect","Call of Duty","Mario Kart","Super Mario","Battlefield","Animal Crossing","Halo","Gran Turismo")

#replace strange Pokemon label
data$Name <- as.character(data$Name)
pokemon_indx <- str_detect(data$Name,"PokÃ©mon")
pokemon_name <- data[pokemon_indx,]$Name %>% str_replace_all("PokÃ©mon","Pokemon")
data[pokemon_indx,]$Name <- pokemon_name

#binary variables for keyword appearance
for (i in 1:length(keywords)){
  data$dummy <- 0
  indx <- str_detect(data$Name,keywords[i])
  data[indx,]$dummy <- 1
  names <- names(data)
  names[length(names)] <- str_replace_all(keywords[i]," ","_")
  names(data) <- names
}

keywords <- str_replace_all(keywords," ","_")

# explore data
plot(data$no_of_platforms,data$Global_Sales)
plot(data$Year,data$Global_Sales)
plot(data$Genre,data$Global_Sales)
plot(data$Publisher,data$Global_Sales)
plot(data$Platform,data$Global_Sales)


#regression_data and task
regression_data <- data %>% select(-c("Rank","Name","NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales"))

task <- TaskRegr$new(id="videogames",backend=regression_data,target="Global_Sales_log10")
# resample
subset1 <- sample.int(task$nrow, size = 0.8 * task$nrow)
testdata <- setdiff(1:task$nrow,subset1)

learner <- lrn("regr.lm")
model <- learner$train(task,row_ids = subset1)
model$predict(task,row_ids = testdata)$score()

autoplot(model$predict(task,row_ids = testdata))


# what is definitely missing?
# - interaction effect of Year and Platform
# - keywords of games outside Top-50
# - variable selection to get rid of noise variables
