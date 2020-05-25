install.packages("tidyverse")
install.packages("splitstackshape")
library(tidyverse)
library(splitstackshape)

recipe_data <- read_csv("C:/Users/user/Downloads/RAW_recipes.csv")

#split tags by comma and drop extra tags
recipe_data = recipe_data %>%
  cSplit("tags",",") %>%
  select(-("tags_07":"tags_73"))

#remove punctuations and extra blank space
recipe_data <- gsub("[[:punct:][:blank:]]+", " ", as.matrix(recipe_data))
recipe_data <- data.frame(recipe_data)

View(recipe_data)
