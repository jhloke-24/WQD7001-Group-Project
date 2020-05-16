getwd()
setwd("C:/Users/jhlok/Desktop/Lecture Slide/WQD7001-Principles of Data Science/Group Assignment/Data")

df = read.csv("RAW_recipes.csv",header=TRUE)
#,nrows=10000

start.time <- Sys.time()

#Recipe Name
df$name = gsub("\\s+", " ", df$name)
sum(duplicated(df$name))
df <- df[order(df[,'name'],df[,'minutes']),]
df = df[!duplicated(df$name),]
df = df[-1,]

name.time <- Sys.time()

#Recipe Duration
summary(df$minutes)
quantile(df$minutes, probs = seq(0, 1, by= 0.1))
typeof(df$minutes)
df$minutes = sapply(df$minutes, as.numeric) 
df$time_temp_001 = floor(df$minutes / 60)
df$time_temp_002 = df$minutes %% 60
typeof(df$time_temp_001)
df$hours = paste(df$time_temp_001,"Hours",df$time_temp_002,"Minutes")

duration.time <- Sys.time()

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

end.time <- Sys.time()

#Drop intermediate field
df = subset(df, select = -c(time_temp_001,time_temp_002,nutrition_new))

#Time Elapse

time.taken <- end.time - start.time
nutrition.taken <- end.time - duration.time
time.taken
nutrition.taken

save(df, file = "recipe_001.rda")
