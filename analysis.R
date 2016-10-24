# Load some packages
library(dplyr)
library(tidyr)
library(ggplot2)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"

# Load the train dataset
df <- read.csv2(file_train, stringsAsFactors = F, header = T, sep = ",")

# Load the data into dplyr's tbl_df
train_data <- tbl_df(df)
train_data <- train_data %>%
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked), Age = as.integer(Age), Fare = as.numeric(Fare)) %>%
  separate(Name, into = c("Surname", "FirstName"), sep = ",") %>%
  separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>%
  mutate(Title = as.factor(Title))

# Percentage of survivors per title
test<-train_data %>%
  select(Title, Survived) %>%
  group_by(Title) %>%
  summarize(count = n(), S = sum(Survived), percent = S/count) %>%
  sum(count) %>%
  print