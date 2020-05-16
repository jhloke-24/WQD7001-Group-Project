install.packages("tidyverse")
install.packages("splitstackshape")
library(tidyverse)
library(splitstackshape)

recipe_data <- read_csv("C:/Users/user/Downloads/RAW_recipes.csv")

recipe_data %<>%
  select(name, tags) %>%
  cSplit("tags",",")
recipe_tags <- gsub("[[:punct:][:blank:]]+", " ", as.matrix(recipe_data))
recipe_tags <- data.frame(recipe_tags)
recipe_tags <- select(recipe_tags, name, tags_01:tags_06)

rm(recipe_data)
View(recipe_tags)
