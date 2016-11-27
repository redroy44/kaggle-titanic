# Load some packages
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(gridExtra)
library(mice)

# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"

# Load the train dataset
train_data <- read_csv(file_train)

# Load the test dataset
test_data <- read_csv(file_test)

# Fix missing data
# Look for NA's
test_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Fix missing data
# Look for NA's
train_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Fix NA's in Embarked
medianEmbarked <- arrange((count(train_data, Embarked)), desc(n))[1,1][[1]]
train_data <- train_data %>%
  mutate(Embarked = replace(Embarked, is.na(Embarked), medianEmbarked))

train_data <- train_data %>%
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked), Age = as.integer(Age), Fare = as.numeric(Fare)) %>%
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
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked), Age = as.integer(Age), Fare = as.numeric(Fare)) %>%
  separate(Name, into = c("Surname", "FirstName"), sep = ",") %>%
  separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>%
  mutate(Title = as.factor(Title), Pclass = as.factor(Pclass)) %>%
  select(-Surname, -FirstName, -Ticket, -Cabin)

imputed_test <- complete(mice(test_data))
test_data$Age <- imputed_test$Age
test_data$Fare <- imputed_test$Fare
# Time for some plots

# Percentage of survivors per title
p1 <- train_data %>%
  select(Title, Survived) %>%
  group_by(Title) %>%
  summarize(count = n(), S = sum(as.integer(Survived)-1), P = S/count) %>%
  arrange(desc(S)) %>%
  ggplot(aes(x=Title, y=P)) + geom_bar(stat="Identity") +
  labs(title = "Survival of Titles", x = "Title", y = "Percent Survived")
plot(p1)

# Percentage of survivors per Age
p2 <- train_data %>%
  select(Age, Survived) %>%
  group_by(Age) %>%
  #summarize(count = n(), S = sum(Survived), P = S/count) %>%
  ggplot(aes(x=Survived, y=Age)) + geom_boxplot() +
  labs(title = "Survival per Age", x = "Title", y = "Percent Survived")
plot(p2)

# Percentage of survivors per Sex
p3 <- train_data %>%
  select(Sex, Survived) %>%
  group_by(Sex) %>%
  summarize(count = n(), S = sum(as.integer(Survived)-1), P = S/count) %>%
  ggplot(aes(x=Sex, y=P)) + geom_bar(stat="Identity") +
  labs(title = "Survival per Sex", x = "Sex", y = "Percent Survived")
plot(p3)

# Percentage of survivors per Pclass
p4 <- train_data %>%
  select(Pclass, Survived) %>%
  group_by(Pclass) %>%
  summarize(count = n(), S = sum(as.integer(Survived)-1), P = S/count) %>%
  ggplot(aes(x=Pclass, y=P)) + geom_bar(stat="Identity") +
  labs(title = "Survival per Pclass", x = "Pclass", y = "Percent Survived")
plot(p4)

# Fare per class
p5 <- train_data %>%
  select(Fare, Pclass) %>%
  group_by(Fare) %>%
  ggplot(aes(x=Pclass, y=Fare)) + geom_boxplot() +
  labs(title = "Cost of Pclass", x = "Pclass", y = "Fare")
plot(p5)

grid.arrange(p4, p5, ncol=2)

# Simple model

glm_fit<-glm(Survived~.,data=select(train_data, -PassengerId, -Title, -Embarked),family=binomial(link='logit'))
summary(glm_fit)

# Write submission file
Prediction <- predict(glm_fit, select(test_data, -PassengerId, -Title, -Parch, -Fare, -Embarked), type = "response")
Survived <- round(Prediction)
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Survived)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)

