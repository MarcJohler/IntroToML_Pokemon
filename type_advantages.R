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

