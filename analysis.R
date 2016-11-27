# Load some packages
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(mice)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"

# Load the train dataset
train_data <- read_csv(file_train)

# Load the test dataset
test_data <- read_csv(file_test)

# Fix missing data
# Look for NA's in test_data
test_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Look for NA's in train_data
train_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Fix NA's in Embarked
medianEmbarked <- arrange((count(train_data, Embarked)), desc(n))[1,1][[1]]
train_data <- train_data %>%
  mutate(Embarked = replace(Embarked, is.na(Embarked), medianEmbarked))

train_data <- train_data %>%
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked)) %>%
  separate(Name, into = c("Surname", "FirstName"), sep = ",") %>%
  separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>%
  mutate(Title = as.factor(Title), Survived = as.factor(Survived), Pclass = as.factor(Pclass)) %>%
  select(-Surname, -FirstName, -Ticket, -Cabin)

# Fix NA's in Age
imputed_data <- complete(mice(train_data))
train_data$Age <- imputed_data$Age

# Check again for NA's
train_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Prepare test set
test_data <- test_data %>%
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked)) %>%
  separate(Name, into = c("Surname", "FirstName"), sep = ",") %>%
  separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>%
  mutate(Title = as.factor(Title), Pclass = as.factor(Pclass)) %>%
  select(-Surname, -FirstName, -Ticket, -Cabin)

imputed_test <- complete(mice(test_data))
test_data$Age <- imputed_test$Age
test_data$Fare <- imputed_test$Fare

# Time for some plots

# Percentage of survivors per title
train_data %>%
  select(Title, Survived) %>%
  group_by(Title) %>%
  summarize(count = n(), S = sum(as.integer(Survived)-1), P = S/count) %>%
  arrange(desc(S)) %>%
  ggplot(aes(x=Title, y=P)) + 
    geom_bar(stat="Identity") +
    labs(title = "Survival of Titles", x = "Title", y = "Percent Survived")

# Percentage of survivors per Age
train_data %>%
  select(Age, Survived) %>%
  ggplot(aes(x=Age)) + 
    geom_bar(aes(fill=Survived), color = "black", binwidth = 4, position = 'fill') +
    labs(title = "Survival per Age", x = "Title", y = "Percent Survived")

# Percentage of survivors per Sex
train_data %>%
  select(Sex, Survived) %>%
  ggplot(aes(x=Sex)) + 
    geom_bar(aes(fill=Survived), position = 'fill') +
    labs(title = "Survival per Sex", x = "Sex", y = "Percent Survived")

# Percentage of survivors per Pclass
train_data %>%
  select(Pclass, Survived) %>%
  ggplot(aes(x=Pclass)) + 
    geom_bar(aes(fill=Survived), position = 'fill') +
    labs(title = "Survival per Pclass", x = "Pclass", y = "Percent Survived")

# Fare per class
train_data %>%
  select(Fare, Pclass) %>%
  ggplot(aes(x=Pclass, y=Fare)) + 
  geom_boxplot() +
  labs(title = "Cost of Pclass", x = "Pclass", y = "Fare")

# Simple model

glm_fit<-glm(Survived~.,data=select(train_data, -PassengerId, -Title, -Embarked),family=binomial(link='logit'))
summary(glm_fit)

# Write submission file
Prediction <- predict(glm_fit, select(test_data, -PassengerId, -Title, -Parch, -Fare, -Embarked), type = "response")
Survived <- round(Prediction)
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Survived)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)


