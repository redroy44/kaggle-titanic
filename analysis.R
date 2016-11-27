# Load some packages
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(mice)
library(randomForest)
set.seed(29082012)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"

# Load the train dataset
train_data <- read_csv(file_train)

# Load the test dataset
test_data <- read_csv(file_test)

full_data = bind_rows(train_data, test_data)

# Fix missing data
# Look for NA's in full_data
full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Fix NA's in Embarked
#medianEmbarked <- arrange((count(train_data, Embarked)), desc(n))[1,1][[1]]
#train_data <- train_data %>%
#  mutate(Embarked = replace(Embarked, is.na(Embarked), medianEmbarked))

full_data <- full_data %>%
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked)) %>%
  separate(Name, into = c("Surname", "FirstName"), sep = ",") %>%
  separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>%
  mutate(Title = as.factor(Title), Survived = as.factor(Survived), Pclass = as.factor(Pclass)) %>%
  select(-Surname, -FirstName, -Ticket, -Cabin)

# Fix NA's in Age Fare Embarked
imputed_data <- complete(mice(select(full_data, -Survived)))
full_data <- round(imputed_data$Age)
full_data$Fare=imputed_data$Fare
full_data$Embarked=imputed_data$Embarked

# Check again for NA's
full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

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

rf <- randomForest(x=select(train_data, -Survived, -PassengerId, -Title), y=train_data$Survived, ntree=100, importance=TRUE)
imp <- importance(rf, type=1)


# Write submission file
Prediction <- predict(rf, select(test_data, -PassengerId, -Title), type = "response")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)


