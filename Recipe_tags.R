install.packages("tidyverse")
install.packages("splitstackshape")
library(tidyverse)
library(splitstackshape)

recipe_data <- read_csv("C:/Users/user/Downloads/RAW_recipes.csv")

#create name and tags data frame and split tags by comma
recipe_data %<>%
  select(name, tags) %>%
  cSplit("tags",",")

#remove punctuations and extra blank space
recipe_data <- gsub("[[:punct:][:blank:]]+", " ", as.matrix(recipe_data))
recipe_data <- data.frame(recipe_data)

View(recipe_data)
