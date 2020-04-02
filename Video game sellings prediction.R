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

# less extremely distributed
hist(data$Global_Sales_log,breaks=seq(min(data$Global_Sales_log),max(data$Global_Sales_log)+0.1,0.1))

# calculate number of platforms per game Name
data$no_of_platforms <- 0
namefreq <- table(data$Name)
namefreq_names <- names(namefreq)

for (i in 1:length(namefreq_names)){
  name_indx <- data$Name==namefreq_names[i]
  data[name_indx,]$no_of_platforms <- namefreq[i] %>% as.integer()
}

#exclude irrelevant platforms --> declare as "other"

data$Platform <- as.character(data$Platform)

platformfreq <- table(data$Platform)
other_indx <- platformfreq < 25
platform_other <- platformfreq[other_indx]
platformfreq_names <- names(platform_other)

for (i in 1:length(platformfreq_names)){
name_indx <- data$Platform==platformfreq_names[i]
data[name_indx,]$Platform <- rep("other",platform_other[i])
}

platform_na <- data$Platform == "N/A"
data$Platform <- replace(data$Platform,platform_na,"other")

data$Platform <- as.factor(data$Platform)

#count number of games per publisher --> has to be done this way because there are different publishers for same games on different platforms
#data$Publisher_no_of_games <- 0
#publisherfreq <- table(data$Publisher)
#publisherfreq_names <- names(publisherfreq)

#for (i in 1:length(publisherfreq_names)){
  #name_indx <- ifelse(data$Publisher %in% publisherfreq_names[i],TRUE,FALSE)
  #data[name_indx,]$Publisher_no_of_games <- publisherfreq[i] %>% as.integer()
#}


#alternative approach: use publisher as categorial variable

data$Publisher <- as.character(data$Publisher)

publisherfreq <- table(data$Publisher)
other_indx <- publisherfreq < 25
publisher_other <- publisherfreq[other_indx]
publisherfreq_names <- names(publisher_other)

for (i in 1:length(publisherfreq_names)){
  name_indx <- data$Publisher==publisherfreq_names[i]
  data[name_indx,]$Publisher <- rep("other",publisher_other[i])
}

publisher_na <- data$Publisher == "N/A"
data$Publisher <- replace(data$Publisher,publisher_na,"other")

data$Publisher <- as.factor(data$Publisher)


#NAs in Year variable --> check how the known years are distributed
data$Year <- as.character(data$Year)

year_na <- data$Year == "N/A"
data$Year <- replace(data$Year,year_na,NA)

data$Year <- as.integer(data$Year)
known_year <- (data %>% filter(!is.na(Year)))$Year
unknown_years <- nrow(data)-length(known_year)

h <- hist(known_year,breaks=seq(1980,2020,1))
xfit<-seq(min(known_year),max(known_year),length=40)
yfit<-dnorm(xfit,mean=mean(known_year),sd=sd(known_year))
yfit <- yfit*diff(h$mids[1:2])*length(known_year) 
lines(xfit, yfit, col="blue", lwd=2) 

# seems like Year is normal-distributed but outliers in early years lead to bias
# since we don't have further information we'll compute naive estimations first

randomized_year <- rnorm(unknown_years,mean(known_year),sd(known_year))
year_na_indx <- (1:nrow(data))[is.na(data$Year)]

j <- 1
for (i in year_na_indx){
  data[i,]$Year <- randomized_year[j]
  j <- j+1
}

# Scale Year and Platforms
data$Year <- scale(data$Year,scale=FALSE)
data$no_of_platforms <- scale(data$no_of_platforms,scale=FALSE)

# Feature transformations
#data$no_of_platforms_sqrd <- data$no_of_platforms^2
#data$Year_sqrd <- data$Year^2

# Year interaction variables
#Year-Platform
platforms <- levels(data$Platform)

for (i in platforms){
  indx <- data$Platform == i 
  data$thisstringpatternshallbeunique <- indx * data$Year
  names(data) <- str_replace(names(data),"thisstringpatternshallbeunique",paste("Year_interaction_","platform_",i,sep=""))
  #data$thisstringpatternshallbeunique <- indx * data$Year_sqrd
  #names(data) <- str_replace(names(data),"thisstringpatternshallbeunique",paste("Year_sqrd_interaction_","platform_",i,sep=""))
}

#Year-Publisher (only if used as categorical variable)
publishers <- levels(data$Publisher)

for (i in publishers){
  indx <- data$Publisher == i 
  data$thisstringpatternshallbeunique <- indx * data$Year
  names(data) <- str_replace(names(data),"thisstringpatternshallbeunique",paste("Year_interaction_","publisher_",i,sep=""))
  #data$thisstringpatternshallbeunique <- indx * data$Year_sqrd
  #names(data) <- str_replace(names(data),"thisstringpatternshallbeunique",paste("Year_sqrd_interaction_","publisher_",i,sep=""))
}

#Year-Genre
genres <- levels(data$Genre)

for (i in genres){
  indx <- data$Genre == i 
  data$thisstringpatternshallbeunique <- indx * data$Year
  names(data) <- str_replace(names(data),"thisstringpatternshallbeunique",paste("Year_interaction_","genre_",i,sep=""))
  #data$thisstringpatternshallbeunique <- indx * data$Year_sqrd
  #names(data) <- str_replace(names(data),"thisstringpatternshallbeunique",paste("Year_sqrd_interaction_","genre_",i,sep=""))
}


# key-words in name
#sum(str_detect(data$Name,"Mario")) #113
#sum(str_detect(data$Name,"Pokemon")) #35
#sum(str_detect(data$Name,"PokÃ©mon")) #13
#sum(str_detect(data$Name,"Final Fantasy"))#89 #89
#sum(str_detect(data$Name,"Elder Scrolls"))#10
#sum(str_detect(data$Name,"Zelda"))#27
#sum(str_detect(data$Name,"Soccer"))#222
#sum(str_detect(data$Name,"Tetris"))#29
#sum(str_detect(data$Name,"Grand Theft Auto"))#28
#sum(str_detect(data$Name,"Kinect"))#13
#sum(str_detect(data$Name,"Call of Duty"))#59
#sum(str_detect(data$Name,"Mario Kart"))#9
#sum(str_detect(data$Name,"Super Mario"))#34
#sum(str_detect(data$Name,"Battlefield"))#28
#sum(str_detect(data$Name,"Animal Crossing"))#6
#sum(str_detect(data$Name,"Halo"))#14
#sum(str_detect(data$Name,"Gran Turismo"))#10
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
  names[length(names)] <- keywords[i]
  names(data) <- names
}

keywords <- str_replace_all(keywords," ","_")
names(data) <- str_replace_all(names(data)," ","_")
names(data) <- str_replace_all(names(data),fixed("."),"")
names(data) <- str_replace_all(names(data),fixed("-"),"")
names(data) <- str_replace_all(names(data),fixed("!"),"")

# explore data
#plot(data$no_of_platforms,data$Global_Sales)
#plot(data$Year,data$Global_Sales)
#plot(data$Genre,data$Global_Sales)
#plot(data$Publisher,data$Global_Sales)
#plot(data$Platform,data$Global_Sales)


#regression_data and task
regression_data <- data %>% select(-c("Rank","Name","NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales"))

task <- TaskRegr$new(id="videogames",backend=regression_data,target="Global_Sales_log")
# resample
trainingdata <- sample.int(task$nrow, size = 0.8 * task$nrow)
testdata <- setdiff(1:task$nrow,trainingdata)
learner <- lrn("regr.ranger")
model <- learner$train(task,row_ids = trainingdata)
model$predict(task,row_ids = trainingdata)$score()
model$predict(task,row_ids = testdata)$score()
autoplot(model$predict(task,row_ids = testdata))

set.seed(42)
rdesc <- rsmp("cv", folds = 10)
res <- resample(task, learner, rdesc)
res$score()
res$aggregate()

###CV-Score (for log2 global sales prediction):
## Publisher as Publisher_no_of_games
# - LM: 3.01976
# - KKNN: 3.217357
# - rpart: 3.604366
# - ranger: 2.555515
## Publisher as categorical variable
# - LM: 2.657638
# - KKNN: 2.934456
# - rpart: 3.170245
# - ranger: 2.460697


# what is definitely missing?
# - interaction effect of Year and Platform --> working on it
# - keywords of games outside Top-50
# - variable selection to get rid of noise variables --> started filtering

#variable selection

filter_info <- flt("information_gain")
filter_info$calculate(task)

filter_scores <- as.data.table(filter_info)
head(filter_scores,100)





