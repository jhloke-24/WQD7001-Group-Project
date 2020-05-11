#call the library package
library(stringr)

#read the dataset
df<-read.csv(file='RAW_recipes.csv')
head(df)

#View the class of the columns
class(df$steps)

#change the class of the columns from factor to character
df$steps<-as.character(df$steps)
class(df$steps) #verify column class

#splitting, the output is stored in a new variable called new
x<-1
while (x < 231638) {
  new[[x]]<-str_split(df$steps[x], "', '")
  x<-x+1
}

#accessing rows in new, printing the splitted strings without quotation marks
noquote(new[1])


# potential issues:
# data inputted is not uniform, such as ',' versus ' ,' or ' , ' 