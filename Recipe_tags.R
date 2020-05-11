install.packages("tidyverse")
install.packages("splitstackshape")
library(tidyverse)
library(splitstackshape)

recipe_data <- read_csv("C:/Users/user/Downloads/RAW_recipes.csv")
recipe_data
recipe_data2 <- select(recipe_data, name, tags)

split_tags <- cSplit(recipe_data2,"tags",",")
View(split_tags)

split_tags2 <- gsub("[[:punct:][:blank:]]+", " ", as.matrix(split_tags))
View(split_tags2)

split_tags <- tibble(split_tags2)
split_tags

rm(recipe_data, recipe_data2, split_tags2)

#split_tags %>%
  #filter_at(vars(tags_01:tags_73)
