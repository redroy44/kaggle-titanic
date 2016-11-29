# Load some packages ------------------------------------------------------
library(checkpoint)
checkpoint("2016-11-15")
library(tidyverse)
library(forcats)
library(stringr)
library(caret)
library(kernlab)
library(mice)
library(randomForest)
library(doMC)
registerDoMC(cores = 8)
set.seed(29082012)


# dataset path
file_train <- "data/train.csv"
file_test <- "data/test.csv"


# Load the train & test dataset -------------------------------------------
train_data <- read_csv(file_train)
test_data <- read_csv(file_test)

full_data = bind_rows(train_data, test_data)

# Look for NA's in full_data ----------------------------------------------
full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print

# Extract useful data -----------------------------------------------------
full_data <- full_data %>%
  mutate(Sex = as.factor(Sex), Embarked = as.factor(Embarked)) %>%
  separate(Name, into = c("Surname", "FirstName"), sep = ",") %>%
  separate(FirstName, into = c("Title", "FirstName"), sep = "\\.", extra = "merge") %>%
  mutate(Title = as.factor(str_trim(Title)), Survived = as.factor(Survived), Pclass = as.factor(Pclass)) %>%
  mutate(FamilySize = Parch + SibSp) %>%
  select(-Surname, -FirstName, -Ticket, -Cabin, -Parch, -SibSp)

# Fix NA's in Age Fare Embarked -------------------------------------------
imputed_data <- complete(mice(select(full_data, -Survived)))
full_data$Age <- round(imputed_data$Age)
full_data$Fare <- imputed_data$Fare
full_data$Embarked <- imputed_data$Embarked

# Check again for NA's in full_data ---------------------------------------
full_data %>%
  summarise_each(funs(sum(is.na(.)))) %>%
  print


# Explore Title column ----------------------------------------------------
full_data %>% 
  select(Title) %>%
  group_by(Title) %>%
  summarize(count = n()) %>%
  print()

# titles <- full_data %>%
#   select(Title) %>%
#   mutate(Title = fct_collapse(full_data$Title,
#          yes = c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess"),
#          no = c("Miss", "Mr", "Mrs", "Ms", "Mlle", "Mme", "Master")))

titles <- full_data %>%
  select(Title) %>%
  mutate(Title = fct_collapse(full_data$Title,
                              "Sir" = c("Don", "Jonkheer", "Sir"),
                              "Lady" = c("Dona", "Lady", "the Countess")))

full_data$Title <- titles$Title

# Resplit train and test data ---------------------------------------------

train_data <- full_data %>%
  filter(!is.na(Survived))

test_data <- full_data %>%
  filter(is.na(Survived)) %>%
  select(-Survived)

# Time for some plots -----------------------------------------------------

# Percentage of survivors per title
full_data %>%
  filter(!is.na(Survived)) %>%
  select(Title, Survived) %>%
  ggplot(aes(x=Title)) + 
    geom_bar(aes(fill=Survived), position = 'fill') +
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

# Percentage of survivors per FamilySize
train_data %>%
  select(FamilySize, Survived) %>%
  ggplot(aes(x=FamilySize)) + 
  geom_bar(aes(fill=Survived), color = "black", binwidth = 1, position = 'fill') +
  labs(title = "Survival per FamilySize", x = "Title", y = "Percent Survived")

# Model fitting and validation --------------------------------------------
inTrain <- createDataPartition(train_data$Survived, p = 0.8, list = F)

training <- train_data %>%
  slice(inTrain)
testing <- train_data %>%
  slice(-inTrain)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

rfGrid <-  expand.grid(mtry=c(2,3,4,5,6,8,10))

modelFit<-train(Survived~., data=select(training, -PassengerId),
                method='rf',
                trControl = fitControl,
                ntree=1000,
                tuneGrid = rfGrid)
modelFit
ggplot(modelFit)
varImp(modelFit, scale = F)

predictions<- predict(modelFit, newdata=testing)
confusionMatrix(predictions, testing$Survived)

# glm - old
# glm_fit<-glm(Survived~.,data=select(train_data, -PassengerId),family=binomial(link='logit'))
# summary(glm_fit)

# Write submission file ---------------------------------------------------
model <- modelFit

Prediction <- predict(model, newdata = select(test_data, -PassengerId))
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)


