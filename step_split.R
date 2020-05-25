#call the library package
library(stringr)
library(tidyverse)

#read the dataset
df<-read.csv(file='RAW_recipes.csv')
head(df)

#View the class of the columns
class(df$steps)

#change the class of the columns from factor to character
df$steps<-as.character(df$steps)
class(df$steps) #verify column class

#inititalize new vaiable called recipeStep --> NEWLY ADDED LINE AS OF 25TH May 2020
recipeStep<-str_split(df$steps[1], "', '")

#Looping
x<-1
while (x < 231638) {
  recipeStep[x]<-str_split(df$steps[x], "', '")
  x<-x+1
}

#accessing rows in new, printing the splitted strings without quotation marks
noquote(recipeStep[1])


# potential issues:
# data inputted is not uniform, such as ',' versus ' ,' or ' , ' 
