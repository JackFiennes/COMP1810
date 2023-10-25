install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)            
library(tidyverse)
starwars
superbowl <- read.table(
  "https://www.portfolioprobe.com/R/blog/dowjones_super.csv", sep="", header=TRUE)
superbowl
install.packages("readr")
library(readr)
data2 <- read_csv("missinglemonade2016.csv")
ncol(data2)
is.na(data2)
sum(is.na(data2))
colSums(is.na(data2))
data2 <- read_csv("missinglemonade2016.(2).csv-UTF8")
data2$Lemon[is.na(data2$Lemon)]<-round(mean(data2$Lemon, na.rm=TRUE))
colSums(is.na(data2))
install.packages("imputeTS")
library(imputeTS)
data3<-na_mean(data2)
colSums(is.na(data2))
data2_new<-data2[, colSums(is.na(data2))<nrow(data2)]
library(dplyr)
library(tidyr)
library(skimr)
starwars
install.packages("skimr")
library(skimr)
skim(starwars)
data<-starwars%>%
  select(height, mass, gender)
data
install.packages("rsample")
library(rsample)
data_split<-initial_split(data)
data_train<-training(data_split)
data_test<-testing(data_split)
dim(data_split)
data_split
data_train
data_train<-data_train%>%
  mutate(bmi=mass/(height*height))
data_train
skim(data_train)
any(is.na(data_train))
colSums(is.na(data_train))
data_train<-data_train%>%
  drop_na(height, gender)#remove the missing values from height and gender
data_train
data_tr_imputed<-data_train%>%
  mutate(mass=ifelse(is.na(mass), mean(mass,na.rm=TRUE),mass),
         bmi=ifelse(is.na(bmi),mean(bmi,na.rm=TRUE),bmi))
data_tr_imputed
skim(data_tr_imputed)
data_tr_imputed_encoded<-data_tr_imputed%>%
  mutate(gender_masculine=ifelse(gender=="masculine",1,0))%>%
  select(-gender)
data_tr_imputed_encoded
normalize<-function(feature){
  (feature=mean(feature))/sd(feature)
}
data_tr_imputed_encoded_normalized<-data_tr_imputed_encoded%>%
  mutate_all(normalize)
data_tr_imputed_encoded_normalized
#error message, return to
data_train%>%
  mutate(bmi=mass/(height*height))%>%
  mutate(mass=ifelse(is.na(mass),mean(mass,na.rm = TRUE),mass),
         bmi=ifelse(is.na(bmi),mean(bmi,na.rm = TRUE),bmi))%>%
  mutate(gender_masculine=ifelse(gender=="masculine",1,0))%>%
  select(-gender)%>%
  mutate_all(normalize)
install.packages("recipes")
library(recipes)
data_recipe<-data_train%>%
  recipe()%>%
  step_mutate(BMI=mass/(height*height))%>%
  step_naomit(height,gender)%>%
  step_impute_mean(mass,BMI)%>%
  step_dummy(gender)%>%
  step_normalize(everything())%>%
  prep()
juice(data_recipe)
data_recipe<-data_recipe%>%
  drop_na(mass,bmi)#remove the missing values from height and gender
data_recipe
iris
skim(iris)
iris%>%
  mutate(Species_versicolor=ifelse(Species=="vericolor",1,0))
         Species_virginica = ifelse(Species=="virginia",1,0)%>%#remove the Species
  select(-iris$Species)#error message
