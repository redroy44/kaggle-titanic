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
registerDoMC(cores = 4)
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
  mutate(Title = as.factor(str_trim(Title)), Survived = as.factor(Survived)) %>%
  mutate(FamilySize = Parch + SibSp + 1, Pclass = as.factor(Pclass)) %>%
  mutate(Pclass = fct_recode(Pclass,first = "1", second = "2", third = "3")) %>%
  mutate(Survived = fct_recode(Survived, "no" = "0", "yes" = "1")) %>%
  select(-FirstName, -Parch, -SibSp, -Cabin)

# Fix NA's in Age Fare Embarked -------------------------------------------
imputed_data <- complete(mice(select(full_data, -Survived)))
full_data$Age <- round(imputed_data$Age)
full_data$Fare <- imputed_data$Fare
full_data$Embarked <- imputed_data$Embarked
# Drop "contrasts" attribute
attr(full_data$Embarked, "contrasts") <- NULL

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

# male - female - child ----------------------------------------------------
sex_age <- full_data %>% 
  select(Sex, Age)

sex_age <- sex_age %>%
  mutate(mfc = as.factor(ifelse(Age < 18, "child", as.character(Sex))))

levels(sex_age$mfc)

full_data <- full_data %>%
  mutate(mfc = sex_age$mfc)

# Family group -----------------------------------------------------------
family_unique <- full_data %>% 
  select(Surname) %>%
  unique()

full_data <-full_data %>%
  mutate(group = as.factor(Surname), ticket_group = as.factor(Ticket)) %>%
  select(-Ticket, -Surname)

# Cabin group ------------------------------------------------------------
# cabin_group <- full_data %>% 
#   select(Cabin
#   unique()

# cabin_group %>%
#   group_by(Surname, Ticket) %>%
#   summarise(n = n())
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
inTrain <- createDataPartition(train_data$Survived, p = 0.75, list = F)

training <- train_data %>%
  slice(inTrain)
testing <- train_data %>%
  slice(-inTrain)

# Random Forest -----------------------------------------------
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  classProbs = TRUE
)

rfGrid <-  expand.grid(mtry=1:5 *2)

rfFit<-train(Survived~., data=select(training, -PassengerId),
                method='rf',
                trControl = fitControl,
                metric="Accuracy",
                ntree = 1000,
                tuneGrid = rfGrid
                )

rfFit
ggplot(rfFit)
plot(varImp(rfFit,scale=F))

predictions<- predict(rfFit, select(testing, -PassengerId))
confusionMatrix(predictions, testing$Survived)

# glmnet -------------------------------------------------------------------
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  classProbs = TRUE)

trGrid <-  expand.grid(.alpha = c(0.01, 0.03, 0.04, 0.05, 0.06, 0.07,0.08, 0.1),
                       .lambda = (1:10) * 0.1)

glmFit<-train(Survived~., data=select(training, -PassengerId),
                method='glmnet',
                trControl = fitControl,
                metric = "Accuracy",
                tuneGrid=trGrid
                )

glmFit
ggplot(glmFit)
plot(varImp(glmFit,scale=F))

predictions<- predict(glmFit, select(testing, -PassengerId))
confusionMatrix(predictions, testing$Survived)

# Write submission file ---------------------------------------------------
model <- glmFit

Prediction <- predict(model, newdata = select(test_data, -PassengerId))
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = ifelse(Prediction == "yes",1,0))
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)


