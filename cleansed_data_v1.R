install.packages("stringr")
install.packages("tidyverse")
install.packages("splitstackshape")
library(tidyverse)
library(stringr)
library(splitstackshape)
library(dplyr)


PATH <- "C:/Users/User/Desktop/Master/Principles of Data Science/Group Project/RAW_recipes.csv"
df <- read.csv(PATH)
df<-filter(df,df$n_ingredients<=5)
glimpse(df)


#Recipe Name
df$name = gsub("\\s+", " ", df$name)
sum(duplicated(df$name))
df <- df[order(df[,'name'],df[,'minutes']),]
df = df[!duplicated(df$name),]
df = df[-1,]



#Recipe Duration
summary(df$minutes)
quantile(df$minutes, probs = seq(0, 1, by= 0.1))
typeof(df$minutes)
df$minutes = sapply(df$minutes, as.numeric) 
df$time_temp_001 = floor(df$minutes / 60)
df$time_temp_002 = df$minutes %% 60
typeof(df$time_temp_001)
df$hours = paste(df$time_temp_001,"Hours",df$time_temp_002,"Minutes")

#Grouping for time
df$g_minutes <- ifelse(df$minutes<=5, "a.<=5 minutes", 
                       ifelse(df$minutes<=15, "b.>5-15 minutes", 
                              ifelse(df$minutes<=30, "c.>15-30 minutes", 
                                     ifelse(df$minutes<=60, "d.>30 minutes to 1 hour", "e.>1 hour"))))



# Nutrition Value

df$nutrition_new<-strsplit(gsub("\\[","",gsub("\\]","",df$nutrition)),",")

for (i in 1:nrow(df))
{
  df$nutrition_calorie[i]=df$nutrition_new[[i]][1]
  df$nutrition_fat[i]=df$nutrition_new[[i]][2]
  df$nutrition_sugar[i]=df$nutrition_new[[i]][3]
  df$nutrition_sodium[i]=df$nutrition_new[[i]][4]
  df$nutrition_protein[i]=df$nutrition_new[[i]][5]
  df$nutrition_sat_fat[i]=df$nutrition_new[[i]][6]
  df$nutrition_carbohydrate[i]=df$nutrition_new[[i]][7]
}


#Define quantile for nutrition

df$nutrition_calorie = sapply(df$nutrition_calorie, as.numeric) 
quan_calorie<-quantile(df$nutrition_calorie, c(.25,.5,.75,1)) 
quan_calorie_25<-quan_calorie[[1]]
quan_calorie_50<-quan_calorie[[2]]
quan_calorie_75<-quan_calorie[[3]]

df$nutrition_carbohydrate = sapply(df$nutrition_carbohydrate, as.numeric) 
quan_carbohydrate<-quantile(df$nutrition_carbohydrate, c(.25,.5,.75,1)) 
quan_carbohydrate_25<-quan_carbohydrate[[1]]
quan_carbohydrate_50<-quan_carbohydrate[[2]]
quan_carbohydrate_75<-quan_carbohydrate[[3]]

#Grouping for Nutrition Value
df$g_calorie <- ifelse(df$nutrition_calorie<=quan_calorie_25, "a.low (<=108 cal)", 
                       ifelse(df$nutrition_calorie<=quan_calorie_50, "b.mid (<=211 cal)", 
                              ifelse(df$nutrition_calorie<=quan_calorie_75, "c.high (<=399 cal)", "d.very high (>399 cal)")))

df$g_carbohydrate <- ifelse(df$nutrition_carbohydrate<=quan_carbohydrate_25, "a.low (<=2g)", 
                       ifelse(df$nutrition_carbohydrate<=quan_carbohydrate_50, "b.mid (<=6g)", 
                              ifelse(df$nutrition_carbohydrate<=quan_carbohydrate_75, "c.high (<=13g)", "d.very high (>13g)")))

#End Grouping Nutrition Value


#Drop intermediate field
df = subset(df, select = -c(time_temp_001,time_temp_002,nutrition_new))


#split tags by comma and drop extra tags

df = df %>%
  cSplit("tags",",") %>%
  select(-("tags_02":"tags_04")) %>%
  select(-("tags_07":"tags_65")) %>%
  rename(tags_02 = tags_05, tags_03 = tags_06)

df$tags_01 <- gsub("[[:punct:][:blank:]]+", " ",df$tags_01)
df$tags_02 <- gsub("[[:punct:][:blank:]]+", " ",df$tags_02)
df$tags_03 <- gsub("[[:punct:][:blank:]]+", " ",df$tags_03)

#remove special character for ingredients
df$ingredients <-gsub("\\[","",gsub("\\]","",gsub("\\'","",gsub('\\"',"",df$ingredients))))

#remove special character for steps
df$steps <- gsub("\\[","",gsub("\\]","",gsub("\\'","",df$steps)))

#remove redundant columns
df<-select(df,-c(2,3,4,5,6,9))

write.csv(df,"part1_v2.csv")
