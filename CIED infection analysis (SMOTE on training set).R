# Implementation of SMOTE solely on train split for RF, Linear SVM and Logistic Regression

# Note: many of the same chunks of code are repeated. If need comments for further understanding/clarification 
# please find first appearance of chunk in script 

rm(list=ls())

# Random Forest

Comfort_Q18<-read.csv("Comfort_Q11.csv")# loading in file from preprocessing 

#View(Comfort_Q18)

Comfort_Q20<- Comfort_Q18[-c(1,51)]# removing x column present due to loading in csv file and feature with only one level/value

Comfort_Q20$Deviceremovedduetoinfection1_yes <- as.factor(Comfort_Q20$Deviceremovedduetoinfection1_yes) #converting to a factor so that implement as a classification problem

# Splitting data into train and test data

library(caret)

set.seed(67489483)
trainIndex <- createDataPartition(Comfort_Q20$Deviceremovedduetoinfection1_yes, p = .5, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

Train <- Comfort_Q20[ trainIndex,]
Test  <- Comfort_Q20[-trainIndex,]

library(dplyr)

Train %>% count(Deviceremovedduetoinfection1_yes)
Test %>% count(Deviceremovedduetoinfection1_yes)

# SMOTE implementation 

library(smotefamily)

class(Train$Deviceremovedduetoinfection1_yes)

Train$Deviceremovedduetoinfection1_yes <-as.numeric(Train$Deviceremovedduetoinfection1_yes)

DATA <-SMOTE(Train, Train$Deviceremovedduetoinfection1_yes, K=5, dup_size = 30) #implementing SMOTE


Comfort_Q12 <- DATA$data #extracting SMOTE dataset

print(prop.table(table(Comfort_Q12$Deviceremovedduetoinfection1_yes)))

print(table(Comfort_Q12$Deviceremovedduetoinfection1_yes))

#View(Comfort_Q12)

Comfort_Q12 <- Comfort_Q12[-c(55)] # removing class column at end, presence due to SMOTE implemntation 

#Reformatting some of the binary/one hot encoded variables

Comfort_Q12$Mild_LV<-factor(ifelse(Comfort_Q12$Mild_LV<0.5,0,1))
Comfort_Q12$Moderate_LV<-factor(ifelse(Comfort_Q12$Moderate_LV<0.5,0,1))
Comfort_Q12$Normal_LV<-factor(ifelse(Comfort_Q12$Normal_LV<0.5,0,1))
Comfort_Q12$Severe_LV<-factor(ifelse(Comfort_Q12$Severe_LV<0.5,0,1))
Comfort_Q12$Gender.M.0.F.1<-factor(ifelse(Comfort_Q12$Gender.M.0.F.1<0.5,0,1))
Comfort_Q12$P...Single<-factor(ifelse(Comfort_Q12$P...Single<0.5,0,1))
Comfort_Q12$P...Dual<-factor(ifelse(Comfort_Q12$P...Dual<0.5,0,1))
Comfort_Q12$P.CRT<-factor(ifelse(Comfort_Q12$P.CRT<0.5,0,1))
Comfort_Q12$P...LL<-factor(ifelse(Comfort_Q12$P...LL<0.5,0,1))
Comfort_Q12$D...Single<-factor(ifelse(Comfort_Q12$D...Dual<0.5,0,1))
Comfort_Q12$CRT.D<-factor(ifelse(Comfort_Q12$CRT.D<0.5,0,1))
Comfort_Q12$D...SQ.ICD<-factor(ifelse(Comfort_Q12$D...SQ.ICD<0.5,0,1))
Comfort_Q12$New<-factor(ifelse(Comfort_Q12$New<0.5,0,1))
Comfort_Q12$MRA<-factor(ifelse(Comfort_Q12$MRA<0.5,0,1))
Comfort_Q12$Statins<-factor(ifelse(Comfort_Q12$Statins<0.5,0,1))
Comfort_Q12$Aspirin<-factor(ifelse(Comfort_Q12$Aspirin<0.5,0,1))
Comfort_Q12$Clopidogrel<-factor(ifelse(Comfort_Q12$Clopidogrel<0.5,0,1))
Comfort_Q12$Other.antiplatelets<-factor(ifelse(Comfort_Q12$Other.antiplatelets<0.5,0,1))
Comfort_Q12$Beta.blockers<-factor(ifelse(Comfort_Q12$Beta.blockers<0.5,0,1))
Comfort_Q12$Ace.Inhibitors<-factor(ifelse(Comfort_Q12$Ace.Inhibitors<0.5,0,1))
Comfort_Q12$ARBs<-factor(ifelse(Comfort_Q12$ARBs<0.5,0,1))
Comfort_Q12$Sacubitril.valsartan<-factor(ifelse(Comfort_Q12$Sacubitril.valsartan<0.5,0,1))
Comfort_Q12$Loop.diuretics<-factor(ifelse(Comfort_Q12$Loop.diuretics<0.5,0,1))
Comfort_Q12$Amiodarone<-factor(ifelse(Comfort_Q12$Amiodarone<0.5,0,1))
Comfort_Q12$Anticoagulant<-factor(ifelse(Comfort_Q12$Anticoagulant<0.5,0,1))
Comfort_Q12$Antibiotics<-factor(ifelse(Comfort_Q12$Antibiotics<0.5,0,1))
Comfort_Q12$Immunosuppressants<-factor(ifelse(Comfort_Q12$Immunosuppressants<0.5,0,1))
Comfort_Q12$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q12$Number.of.Box.Changes.in.Study.Period<0.5,0,1))
Comfort_Q12$Upgrade<-factor(ifelse(Comfort_Q12$Upgrade<0.5,0,1))
Comfort_Q12$Group<-factor(ifelse(Comfort_Q12$Group<1.01,1,2))
Comfort_Q12$CRT_DFollowUp<-factor(ifelse(Comfort_Q12$CRT_DFollowUp<0.5,0,1))
Comfort_Q12$CRT_PFollowUp<-factor(ifelse(Comfort_Q12$CRT_PFollowUp<0.5,0,1))
Comfort_Q12$ICD_DualFollowUp<-factor(ifelse(Comfort_Q12$ICD_DualFollowUp<0.5,0,1))
Comfort_Q12$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q12$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_DualFollowUp<-factor(ifelse(Comfort_Q12$PPM_DualFollowUp<0.5,0,1))
Comfort_Q12$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q12$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q12$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q12$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q12$SQ_ICDFollowUp<0.5,0,1))

#Reformatting levels of outcome variable 

Comfort_Q12$Deviceremovedduetoinfection1_yes[Comfort_Q12$Deviceremovedduetoinfection1_yes == 1] <- 0

Comfort_Q12$Deviceremovedduetoinfection1_yes[Comfort_Q12$Deviceremovedduetoinfection1_yes == 2] <- 1

Comfort_Q12 %>% count(Deviceremovedduetoinfection1_yes)

#Reformatting multi-categorical variables

# Charlston index

library(dplyr)

Comfort_Q12<- Comfort_Q12 %>% mutate(across(starts_with("Charlson"), round))

# Fried score

Comfort_Q12 %>% count(TOTAL....Fried.Score)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q12 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

values_count <- sapply(lapply(Comfort_Q12, unique), length) # checking levels
values_count

Train <- Comfort_Q12 #creating train variable for machine learning implementation 

Train$Deviceremovedduetoinfection1_yes <-as.factor(Train$Deviceremovedduetoinfection1_yes) # converting outcome variable to a factor so that in right format for ML implementation


# Constructing random forest model  

folds <- 10 #number of folds 
cvIndex <- createFolds(factor(Train$Deviceremovedduetoinfection1_yes), folds, returnTrain = T) # splitting the data into 10 folds. Proportions of outcome variable preserved in folds

# setting the properties of the train function -> repeated stratified k-fold cross-validation 

repeat_cv <- trainControl(index = cvIndex,method='repeatedcv', number=folds, repeats=10)

set.seed(87)

# Training
forest <- train(
  
  
  Deviceremovedduetoinfection1_yes~., 
  
  
  data=Train, 
  
  
  method='rf', 
  
  
  trControl=repeat_cv,
  
  
  metric='Accuracy')

Comfort_Q12 <- Test #converting to new variable for variable formatting 

#Reformatting some of the binary/one hot encoded variables 

Comfort_Q12$Mild_LV<-factor(ifelse(Comfort_Q12$Mild_LV<0.5,0,1))
Comfort_Q12$Moderate_LV<-factor(ifelse(Comfort_Q12$Moderate_LV<0.5,0,1))
Comfort_Q12$Normal_LV<-factor(ifelse(Comfort_Q12$Normal_LV<0.5,0,1))
Comfort_Q12$Severe_LV<-factor(ifelse(Comfort_Q12$Severe_LV<0.5,0,1))
Comfort_Q12$Gender.M.0.F.1<-factor(ifelse(Comfort_Q12$Gender.M.0.F.1<0.5,0,1))
Comfort_Q12$P...Single<-factor(ifelse(Comfort_Q12$P...Single<0.5,0,1))
Comfort_Q12$P...Dual<-factor(ifelse(Comfort_Q12$P...Dual<0.5,0,1))
Comfort_Q12$P.CRT<-factor(ifelse(Comfort_Q12$P.CRT<0.5,0,1))
Comfort_Q12$P...LL<-factor(ifelse(Comfort_Q12$P...LL<0.5,0,1))
Comfort_Q12$D...Single<-factor(ifelse(Comfort_Q12$D...Dual<0.5,0,1))
Comfort_Q12$CRT.D<-factor(ifelse(Comfort_Q12$CRT.D<0.5,0,1))
Comfort_Q12$D...SQ.ICD<-factor(ifelse(Comfort_Q12$D...SQ.ICD<0.5,0,1))
Comfort_Q12$New<-factor(ifelse(Comfort_Q12$New<0.5,0,1))
Comfort_Q12$MRA<-factor(ifelse(Comfort_Q12$MRA<0.5,0,1))
Comfort_Q12$Statins<-factor(ifelse(Comfort_Q12$Statins<0.5,0,1))
Comfort_Q12$Aspirin<-factor(ifelse(Comfort_Q12$Aspirin<0.5,0,1))
Comfort_Q12$Clopidogrel<-factor(ifelse(Comfort_Q12$Clopidogrel<0.5,0,1))
Comfort_Q12$Other.antiplatelets<-factor(ifelse(Comfort_Q12$Other.antiplatelets<0.5,0,1))
Comfort_Q12$Beta.blockers<-factor(ifelse(Comfort_Q12$Beta.blockers<0.5,0,1))
Comfort_Q12$Ace.Inhibitors<-factor(ifelse(Comfort_Q12$Ace.Inhibitors<0.5,0,1))
Comfort_Q12$ARBs<-factor(ifelse(Comfort_Q12$ARBs<0.5,0,1))
Comfort_Q12$Sacubitril.valsartan<-factor(ifelse(Comfort_Q12$Sacubitril.valsartan<0.5,0,1))
Comfort_Q12$Loop.diuretics<-factor(ifelse(Comfort_Q12$Loop.diuretics<0.5,0,1))
Comfort_Q12$Amiodarone<-factor(ifelse(Comfort_Q12$Amiodarone<0.5,0,1))
Comfort_Q12$Anticoagulant<-factor(ifelse(Comfort_Q12$Anticoagulant<0.5,0,1))
Comfort_Q12$Antibiotics<-factor(ifelse(Comfort_Q12$Antibiotics<0.5,0,1))
Comfort_Q12$Immunosuppressants<-factor(ifelse(Comfort_Q12$Immunosuppressants<0.5,0,1))
Comfort_Q12$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q12$Number.of.Box.Changes.in.Study.Period<0.5,0,1))
Comfort_Q12$Upgrade<-factor(ifelse(Comfort_Q12$Upgrade<0.5,0,1))
Comfort_Q12$Group<-factor(ifelse(Comfort_Q12$Group<1.01,1,2))
Comfort_Q12$CRT_DFollowUp<-factor(ifelse(Comfort_Q12$CRT_DFollowUp<0.5,0,1))
Comfort_Q12$CRT_PFollowUp<-factor(ifelse(Comfort_Q12$CRT_PFollowUp<0.5,0,1))
Comfort_Q12$ICD_DualFollowUp<-factor(ifelse(Comfort_Q12$ICD_DualFollowUp<0.5,0,1))
Comfort_Q12$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q12$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_DualFollowUp<-factor(ifelse(Comfort_Q12$PPM_DualFollowUp<0.5,0,1))
Comfort_Q12$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q12$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q12$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q12$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q12$SQ_ICDFollowUp<0.5,0,1))

# Reformatting mutli-categorical variables 

# Charlston index

library(dplyr)

Comfort_Q12<- Comfort_Q12 %>% mutate(across(starts_with("Charlson"), round))

# Fried score

Comfort_Q12 %>% count(TOTAL....Fried.Score)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q12 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Test <- Comfort_Q12 #converting back to test variable to get prediction on test data

y_hats <- predict(object=forest, newdata=Test[, -55])

# Obtaining accuracy 
accuracy <- mean(y_hats == Test$Deviceremovedduetoinfection1_yes)*100
cat('Accuracy on testing data: ', round(accuracy, 3), '%',  sep='')

# Confusion Matrix for model 
confusionMatrix(as.factor(y_hats), as.factor(Test$Deviceremovedduetoinfection1_yes))



# Linear Support Vector machine 

rm(list=ls())

library(caret)

library(ggrepel)

Comfort_QSVM<-read.csv("Comfort_Q11.csv") 

Comfort_QSVM<- Comfort_QSVM[-c(1)]

Comfort_QSVM$Deviceremovedduetoinfection1_yes <- as.factor(Comfort_QSVM$Deviceremovedduetoinfection1_yes) #converting to a factor so that implement as a classification problem

# Splitting data in train and test data

set.seed(178293)
trainIndex <- createDataPartition(Comfort_QSVM$Deviceremovedduetoinfection1_yes, p = .5, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

Train <- Comfort_QSVM[ trainIndex,]
Test  <- Comfort_QSVM[-trainIndex,]

library(dplyr)

Train %>% count(Deviceremovedduetoinfection1_yes)
Test %>% count(Deviceremovedduetoinfection1_yes)

# SMOTE implementation 

library(smotefamily)

class(Train$Deviceremovedduetoinfection1_yes)

Train$Deviceremovedduetoinfection1_yes <-as.numeric(Train$Deviceremovedduetoinfection1_yes)

DATA <-SMOTE(Train, Train$Deviceremovedduetoinfection1_yes, K=5, dup_size = 30) #implementing SMOTE

Comfort_Q12 <- DATA$data #extracting SMOTE dataset

print(prop.table(table(Comfort_Q12$Deviceremovedduetoinfection1_yes)))

print(table(Comfort_Q12$Deviceremovedduetoinfection1_yes))

Comfort_Q12 <- Comfort_Q12[-c(56)]

# Reformatting some of the binary/one hot encoded variables 

Comfort_Q12$Mild_LV<-factor(ifelse(Comfort_Q12$Mild_LV<0.5,0,1))
Comfort_Q12$Moderate_LV<-factor(ifelse(Comfort_Q12$Moderate_LV<0.5,0,1))
Comfort_Q12$Normal_LV<-factor(ifelse(Comfort_Q12$Normal_LV<0.5,0,1))
Comfort_Q12$Severe_LV<-factor(ifelse(Comfort_Q12$Severe_LV<0.5,0,1))

Comfort_Q12$Gender.M.0.F.1<-factor(ifelse(Comfort_Q12$Gender.M.0.F.1<0.5,0,1))
Comfort_Q12$P...Single<-factor(ifelse(Comfort_Q12$P...Single<0.5,0,1))
Comfort_Q12$P...Dual<-factor(ifelse(Comfort_Q12$P...Dual<0.5,0,1))
Comfort_Q12$P.CRT<-factor(ifelse(Comfort_Q12$P.CRT<0.5,0,1))
Comfort_Q12$P...LL<-factor(ifelse(Comfort_Q12$P...LL<0.5,0,1))
Comfort_Q12$D...Single<-factor(ifelse(Comfort_Q12$D...Dual<0.5,0,1))
Comfort_Q12$CRT.D<-factor(ifelse(Comfort_Q12$CRT.D<0.5,0,1))
Comfort_Q12$D...SQ.ICD<-factor(ifelse(Comfort_Q12$D...SQ.ICD<0.5,0,1))
Comfort_Q12$New<-factor(ifelse(Comfort_Q12$New<0.5,0,1))
Comfort_Q12$MRA<-factor(ifelse(Comfort_Q12$MRA<0.5,0,1))
Comfort_Q12$Statins<-factor(ifelse(Comfort_Q12$Statins<0.5,0,1))
Comfort_Q12$Aspirin<-factor(ifelse(Comfort_Q12$Aspirin<0.5,0,1))
Comfort_Q12$Clopidogrel<-factor(ifelse(Comfort_Q12$Clopidogrel<0.5,0,1))
Comfort_Q12$Other.antiplatelets<-factor(ifelse(Comfort_Q12$Other.antiplatelets<0.5,0,1))
Comfort_Q12$Beta.blockers<-factor(ifelse(Comfort_Q12$Beta.blockers<0.5,0,1))
Comfort_Q12$Ace.Inhibitors<-factor(ifelse(Comfort_Q12$Ace.Inhibitors<0.5,0,1))
Comfort_Q12$ARBs<-factor(ifelse(Comfort_Q12$ARBs<0.5,0,1))
Comfort_Q12$Sacubitril.valsartan<-factor(ifelse(Comfort_Q12$Sacubitril.valsartan<0.5,0,1))
Comfort_Q12$Loop.diuretics<-factor(ifelse(Comfort_Q12$Loop.diuretics<0.5,0,1))
Comfort_Q12$Amiodarone<-factor(ifelse(Comfort_Q12$Amiodarone<0.5,0,1))
Comfort_Q12$Anticoagulant<-factor(ifelse(Comfort_Q12$Anticoagulant<0.5,0,1))
Comfort_Q12$Antibiotics<-factor(ifelse(Comfort_Q12$Antibiotics<0.5,0,1))
Comfort_Q12$Immunosuppressants<-factor(ifelse(Comfort_Q12$Immunosuppressants<0.5,0,1))
Comfort_Q12$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q12$Number.of.Box.Changes.in.Study.Period<0.5,0,1))
Comfort_Q12$Upgrade<-factor(ifelse(Comfort_Q12$Upgrade<0.5,0,1))
Comfort_Q12$Group<-factor(ifelse(Comfort_Q12$Group<1.01,1,2))
Comfort_Q12$CRT_DFollowUp<-factor(ifelse(Comfort_Q12$CRT_DFollowUp<0.5,0,1))
Comfort_Q12$CRT_PFollowUp<-factor(ifelse(Comfort_Q12$CRT_PFollowUp<0.5,0,1))
Comfort_Q12$ICD_DualFollowUp<-factor(ifelse(Comfort_Q12$ICD_DualFollowUp<0.5,0,1))
Comfort_Q12$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q12$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q12$MicraImplantFollowUp<-factor(ifelse(Comfort_Q12$MicraImplantFollowUp<0.5,0,1))
Comfort_Q12$PPM_DualFollowUp<-factor(ifelse(Comfort_Q12$PPM_DualFollowUp<0.5,0,1))
Comfort_Q12$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q12$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q12$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q12$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q12$SQ_ICDFollowUp<0.5,0,1))

#Reformatting levels of outcome variable 

Comfort_Q12$Deviceremovedduetoinfection1_yes[Comfort_Q12$Deviceremovedduetoinfection1_yes == 1] <- 0

Comfort_Q12$Deviceremovedduetoinfection1_yes[Comfort_Q12$Deviceremovedduetoinfection1_yes == 2] <- 1

Comfort_Q12 %>% count(Deviceremovedduetoinfection1_yes)

#Reformatting multi-categorical variables 

# Charlston index

library(dplyr)

Comfort_Q12<- Comfort_Q12 %>% mutate(across(starts_with("Charlson"), round))

# Fried score

Comfort_Q12 %>% count(TOTAL....Fried.Score)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q12 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Train <- Comfort_Q12 # converting back to train variable for training of model 

Train$Deviceremovedduetoinfection1_yes <-as.factor(Train$Deviceremovedduetoinfection1_yes)

folds <- 10 #number of folds 
cvIndex <- createFolds(factor(Train$Deviceremovedduetoinfection1_yes), folds, returnTrain = T) # splitting the data into 10 folds. Proportions of outcome variable preserved in folds

# setting the properties of the train function -> repeated stratified k-fold cross-validation 

tc <- trainControl(index = cvIndex,
                   method = 'repeatedcv', 
                   number = folds,
                   repeats = 10)

# training our model


svm_Linear <- train(Deviceremovedduetoinfection1_yes ~., data = Train, method = "svmLinear",
                    trControl=tc,
                    preProcess = c("center", "scale"), #help with centering and scaling the data
                    tuneLength = 10) #this is for tuning our algorithm
svm_Linear

# assessing model performance on test data

Comfort_Q12 <- Test #converting to new variable for variable formatting 

#Reformatting some of the binary/one hot encoded variables 

Comfort_Q12$Mild_LV<-factor(ifelse(Comfort_Q12$Mild_LV<0.5,0,1))
Comfort_Q12$Moderate_LV<-factor(ifelse(Comfort_Q12$Moderate_LV<0.5,0,1))
Comfort_Q12$Normal_LV<-factor(ifelse(Comfort_Q12$Normal_LV<0.5,0,1))
Comfort_Q12$Severe_LV<-factor(ifelse(Comfort_Q12$Severe_LV<0.5,0,1))
Comfort_Q12$Gender.M.0.F.1<-factor(ifelse(Comfort_Q12$Gender.M.0.F.1<0.5,0,1))
Comfort_Q12$P...Single<-factor(ifelse(Comfort_Q12$P...Single<0.5,0,1))
Comfort_Q12$P...Dual<-factor(ifelse(Comfort_Q12$P...Dual<0.5,0,1))
Comfort_Q12$P.CRT<-factor(ifelse(Comfort_Q12$P.CRT<0.5,0,1))
Comfort_Q12$P...LL<-factor(ifelse(Comfort_Q12$P...LL<0.5,0,1))
Comfort_Q12$D...Single<-factor(ifelse(Comfort_Q12$D...Dual<0.5,0,1))
Comfort_Q12$CRT.D<-factor(ifelse(Comfort_Q12$CRT.D<0.5,0,1))
Comfort_Q12$D...SQ.ICD<-factor(ifelse(Comfort_Q12$D...SQ.ICD<0.5,0,1))
Comfort_Q12$New<-factor(ifelse(Comfort_Q12$New<0.5,0,1))
Comfort_Q12$MRA<-factor(ifelse(Comfort_Q12$MRA<0.5,0,1))
Comfort_Q12$Statins<-factor(ifelse(Comfort_Q12$Statins<0.5,0,1))
Comfort_Q12$Aspirin<-factor(ifelse(Comfort_Q12$Aspirin<0.5,0,1))
Comfort_Q12$Clopidogrel<-factor(ifelse(Comfort_Q12$Clopidogrel<0.5,0,1))
Comfort_Q12$Other.antiplatelets<-factor(ifelse(Comfort_Q12$Other.antiplatelets<0.5,0,1))
Comfort_Q12$Beta.blockers<-factor(ifelse(Comfort_Q12$Beta.blockers<0.5,0,1))
Comfort_Q12$Ace.Inhibitors<-factor(ifelse(Comfort_Q12$Ace.Inhibitors<0.5,0,1))
Comfort_Q12$ARBs<-factor(ifelse(Comfort_Q12$ARBs<0.5,0,1))
Comfort_Q12$Sacubitril.valsartan<-factor(ifelse(Comfort_Q12$Sacubitril.valsartan<0.5,0,1))
Comfort_Q12$Loop.diuretics<-factor(ifelse(Comfort_Q12$Loop.diuretics<0.5,0,1))
Comfort_Q12$Amiodarone<-factor(ifelse(Comfort_Q12$Amiodarone<0.5,0,1))
Comfort_Q12$Anticoagulant<-factor(ifelse(Comfort_Q12$Anticoagulant<0.5,0,1))
Comfort_Q12$Antibiotics<-factor(ifelse(Comfort_Q12$Antibiotics<0.5,0,1))
Comfort_Q12$Immunosuppressants<-factor(ifelse(Comfort_Q12$Immunosuppressants<0.5,0,1))
Comfort_Q12$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q12$Number.of.Box.Changes.in.Study.Period<0.5,0,1))
Comfort_Q12$Upgrade<-factor(ifelse(Comfort_Q12$Upgrade<0.5,0,1))
Comfort_Q12$Group<-factor(ifelse(Comfort_Q12$Group<1.01,1,2))
Comfort_Q12$CRT_DFollowUp<-factor(ifelse(Comfort_Q12$CRT_DFollowUp<0.5,0,1))
Comfort_Q12$CRT_PFollowUp<-factor(ifelse(Comfort_Q12$CRT_PFollowUp<0.5,0,1))
Comfort_Q12$ICD_DualFollowUp<-factor(ifelse(Comfort_Q12$ICD_DualFollowUp<0.5,0,1))
Comfort_Q12$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q12$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q12$MicraImplantFollowUp<-factor(ifelse(Comfort_Q12$MicraImplantFollowUp<0.5,0,1))
Comfort_Q12$PPM_DualFollowUp<-factor(ifelse(Comfort_Q12$PPM_DualFollowUp<0.5,0,1))
Comfort_Q12$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q12$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q12$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q12$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q12$SQ_ICDFollowUp<0.5,0,1))

#Reformatting outcome variable levels 

Comfort_Q12 %>% count(Deviceremovedduetoinfection1_yes)

Comfort_Q12$Deviceremovedduetoinfection1_yes[Comfort_Q12$Deviceremovedduetoinfection1_yes == 1] <- 0

Comfort_Q12$Deviceremovedduetoinfection1_yes[Comfort_Q12$Deviceremovedduetoinfection1_yes == 2] <- 1

Comfort_Q12 %>% count(Deviceremovedduetoinfection1_yes)

# Reformatting mutli-categorical variables 

# Charlston index

library(dplyr)

Comfort_Q12<- Comfort_Q12 %>% mutate(across(starts_with("Charlson"), round))

# Fried score

Comfort_Q12 %>% count(TOTAL....Fried.Score)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q12 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Test <- Comfort_Q12 #converting back to test variable to get prediction on test data 

test_pred <- predict(svm_Linear, newdata = Test)
test_pred

confusionMatrix(table(test_pred, Test$Deviceremovedduetoinfection1_yes))

# Logistic regression 

rm(list=ls())

# Loading in relevant librarties 

library(caret) # one hot encoding 
library(dplyr)
library(tidyverse)

Comfort_Q18 <- read.csv("Comfort_Q11.csv")

View(Comfort_Q18)

Comfort_Q20<- Comfort_Q18[-c(1,51)] #removing x column present due to loading in csv file and feature with one value

set.seed(5422)

# Splitting into train and test splits 

trainIndex <- createDataPartition(Comfort_Q20$Deviceremovedduetoinfection1_yes, p = .5, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

Train <- Comfort_Q20[ trainIndex,]
Test  <- Comfort_Q20[-trainIndex,]

table(Test$Deviceremovedduetoinfection1_yes)
table(Train$Deviceremovedduetoinfection1_yes)

# SMOTE implementation 

library(smotefamily)

class(Train$Deviceremovedduetoinfection1_yes)

Train$Deviceremovedduetoinfection1_yes <-as.numeric(Train$Deviceremovedduetoinfection1_yes)

DATA <-SMOTE(Train, Train$Deviceremovedduetoinfection1_yes, K=5, dup_size = 30) #implementing SMOTE

Comfort_Q12 <- DATA$data #extracting SMOTE dataset

print(prop.table(table(Comfort_Q12$Deviceremovedduetoinfection1_yes)))

print(table(Comfort_Q12$Deviceremovedduetoinfection1_yes))

Comfort_Q12 <- Comfort_Q12[-c(55)]

Comfort_Q12$Mild_LV<-factor(ifelse(Comfort_Q12$Mild_LV<0.5,0,1))
Comfort_Q12$Moderate_LV<-factor(ifelse(Comfort_Q12$Moderate_LV<0.5,0,1))
Comfort_Q12$Normal_LV<-factor(ifelse(Comfort_Q12$Normal_LV<0.5,0,1))
Comfort_Q12$Severe_LV<-factor(ifelse(Comfort_Q12$Severe_LV<0.5,0,1))
Comfort_Q12$Gender.M.0.F.1<-factor(ifelse(Comfort_Q12$Gender.M.0.F.1<0.5,0,1))
Comfort_Q12$P...Single<-factor(ifelse(Comfort_Q12$P...Single<0.5,0,1))
Comfort_Q12$P...Dual<-factor(ifelse(Comfort_Q12$P...Dual<0.5,0,1))
Comfort_Q12$P.CRT<-factor(ifelse(Comfort_Q12$P.CRT<0.5,0,1))
Comfort_Q12$P...LL<-factor(ifelse(Comfort_Q12$P...LL<0.5,0,1))
Comfort_Q12$D...Single<-factor(ifelse(Comfort_Q12$D...Dual<0.5,0,1))
Comfort_Q12$CRT.D<-factor(ifelse(Comfort_Q12$CRT.D<0.5,0,1))
Comfort_Q12$D...SQ.ICD<-factor(ifelse(Comfort_Q12$D...SQ.ICD<0.5,0,1))
Comfort_Q12$New<-factor(ifelse(Comfort_Q12$New<0.5,0,1))
Comfort_Q12$MRA<-factor(ifelse(Comfort_Q12$MRA<0.5,0,1))
Comfort_Q12$Statins<-factor(ifelse(Comfort_Q12$Statins<0.5,0,1))
Comfort_Q12$Aspirin<-factor(ifelse(Comfort_Q12$Aspirin<0.5,0,1))
Comfort_Q12$Clopidogrel<-factor(ifelse(Comfort_Q12$Clopidogrel<0.5,0,1))
Comfort_Q12$Other.antiplatelets<-factor(ifelse(Comfort_Q12$Other.antiplatelets<0.5,0,1))
Comfort_Q12$Beta.blockers<-factor(ifelse(Comfort_Q12$Beta.blockers<0.5,0,1))
Comfort_Q12$Ace.Inhibitors<-factor(ifelse(Comfort_Q12$Ace.Inhibitors<0.5,0,1))
Comfort_Q12$ARBs<-factor(ifelse(Comfort_Q12$ARBs<0.5,0,1))
Comfort_Q12$Sacubitril.valsartan<-factor(ifelse(Comfort_Q12$Sacubitril.valsartan<0.5,0,1))
Comfort_Q12$Loop.diuretics<-factor(ifelse(Comfort_Q12$Loop.diuretics<0.5,0,1))
Comfort_Q12$Amiodarone<-factor(ifelse(Comfort_Q12$Amiodarone<0.5,0,1))
Comfort_Q12$Anticoagulant<-factor(ifelse(Comfort_Q12$Anticoagulant<0.5,0,1))
Comfort_Q12$Antibiotics<-factor(ifelse(Comfort_Q12$Antibiotics<0.5,0,1))
Comfort_Q12$Immunosuppressants<-factor(ifelse(Comfort_Q12$Immunosuppressants<0.5,0,1))
Comfort_Q12$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q12$Number.of.Box.Changes.in.Study.Period<0.5,0,1))
Comfort_Q12$Upgrade<-factor(ifelse(Comfort_Q12$Upgrade<0.5,0,1))
Comfort_Q12$Group<-factor(ifelse(Comfort_Q12$Group<1.01,1,2))
Comfort_Q12$CRT_DFollowUp<-factor(ifelse(Comfort_Q12$CRT_DFollowUp<0.5,0,1))
Comfort_Q12$CRT_PFollowUp<-factor(ifelse(Comfort_Q12$CRT_PFollowUp<0.5,0,1))
Comfort_Q12$ICD_DualFollowUp<-factor(ifelse(Comfort_Q12$ICD_DualFollowUp<0.5,0,1))
Comfort_Q12$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q12$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_DualFollowUp<-factor(ifelse(Comfort_Q12$PPM_DualFollowUp<0.5,0,1))
Comfort_Q12$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q12$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q12$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q12$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q12$SQ_ICDFollowUp<0.5,0,1))

library(dplyr)

# Charlston index

Comfort_Q12<- Comfort_Q12 %>% mutate(across(starts_with("Charlson"), round))

# Fried score

Comfort_Q12 %>% count(TOTAL....Fried.Score)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q12 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Train <- Comfort_Q12

Train$Deviceremovedduetoinfection1_yes <-as.factor(Train$Deviceremovedduetoinfection1_yes)


values_count <- sapply(lapply(Train, unique), length)  # Identify variables with 1 value so that can be omitted
values_count

# Implementing logistic regression 

set.seed(7859)


model <- glm( Deviceremovedduetoinfection1_yes ~ Age + Gender.M.0.F.1 + BMI + Group + P...Single + P...Dual + P.CRT + P...LL + D...Single + D...Dual + CRT.D + D...SQ.ICD + Sodium + Potassium + Urea + Creatinine + eGFR + Hb + WCC + Plts + Aspirin + Other.antiplatelets + Clopidogrel + Beta.blockers + Ace.Inhibitors + ARBs + Sacubitril.valsartan + MRA + Loop.diuretics + Statins + Anticoagulant + Amiodarone + Immunosuppressants + Charlson.score.TOTAL + TOTAL....Fried.Score + EQ5D5LIndex + Antibiotics + New + Upgrade + Number.of.Box.Changes.in.Study.Period + Number.of.Upgrades...Downgrade.Device.Implant.Procedures + Moderate_LV + Severe_LV + Mild_LV + PPM_DualFollowUp + PPM_LeadlessFollowUp + PPM_SingleFollowUp + CRT_PFollowUp + Normal_LV + CRT_DFollowUp + ICD_DualFollowUp + ICD_SingleFollowUp,data = Train, family = binomial(link='logit'),maxit=100) #correctly excluding MicraImplantFollowUp

summary(model)$coef
summary(model)

# Reformatting Test variable 

Comfort_Q12 <- Test

Comfort_Q12$Mild_LV<-factor(ifelse(Comfort_Q12$Mild_LV<0.5,0,1))
Comfort_Q12$Moderate_LV<-factor(ifelse(Comfort_Q12$Moderate_LV<0.5,0,1))
Comfort_Q12$Normal_LV<-factor(ifelse(Comfort_Q12$Normal_LV<0.5,0,1))
Comfort_Q12$Severe_LV<-factor(ifelse(Comfort_Q12$Severe_LV<0.5,0,1))
Comfort_Q12$Gender.M.0.F.1<-factor(ifelse(Comfort_Q12$Gender.M.0.F.1<0.5,0,1))
Comfort_Q12$P...Single<-factor(ifelse(Comfort_Q12$P...Single<0.5,0,1))
Comfort_Q12$P...Dual<-factor(ifelse(Comfort_Q12$P...Dual<0.5,0,1))
Comfort_Q12$P.CRT<-factor(ifelse(Comfort_Q12$P.CRT<0.5,0,1))
Comfort_Q12$P...LL<-factor(ifelse(Comfort_Q12$P...LL<0.5,0,1))
Comfort_Q12$D...Single<-factor(ifelse(Comfort_Q12$D...Dual<0.5,0,1))
Comfort_Q12$CRT.D<-factor(ifelse(Comfort_Q12$CRT.D<0.5,0,1))
Comfort_Q12$D...SQ.ICD<-factor(ifelse(Comfort_Q12$D...SQ.ICD<0.5,0,1))
Comfort_Q12$New<-factor(ifelse(Comfort_Q12$New<0.5,0,1))
Comfort_Q12$MRA<-factor(ifelse(Comfort_Q12$MRA<0.5,0,1))
Comfort_Q12$Statins<-factor(ifelse(Comfort_Q12$Statins<0.5,0,1))
Comfort_Q12$Aspirin<-factor(ifelse(Comfort_Q12$Aspirin<0.5,0,1))
Comfort_Q12$Clopidogrel<-factor(ifelse(Comfort_Q12$Clopidogrel<0.5,0,1))
Comfort_Q12$Other.antiplatelets<-factor(ifelse(Comfort_Q12$Other.antiplatelets<0.5,0,1))
Comfort_Q12$Beta.blockers<-factor(ifelse(Comfort_Q12$Beta.blockers<0.5,0,1))
Comfort_Q12$Ace.Inhibitors<-factor(ifelse(Comfort_Q12$Ace.Inhibitors<0.5,0,1))
Comfort_Q12$ARBs<-factor(ifelse(Comfort_Q12$ARBs<0.5,0,1))
Comfort_Q12$Sacubitril.valsartan<-factor(ifelse(Comfort_Q12$Sacubitril.valsartan<0.5,0,1))
Comfort_Q12$Loop.diuretics<-factor(ifelse(Comfort_Q12$Loop.diuretics<0.5,0,1))
Comfort_Q12$Amiodarone<-factor(ifelse(Comfort_Q12$Amiodarone<0.5,0,1))
Comfort_Q12$Anticoagulant<-factor(ifelse(Comfort_Q12$Anticoagulant<0.5,0,1))
Comfort_Q12$Antibiotics<-factor(ifelse(Comfort_Q12$Antibiotics<0.5,0,1))
Comfort_Q12$Immunosuppressants<-factor(ifelse(Comfort_Q12$Immunosuppressants<0.5,0,1))
Comfort_Q12$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q12$Number.of.Box.Changes.in.Study.Period<0.5,0,1))
Comfort_Q12$Upgrade<-factor(ifelse(Comfort_Q12$Upgrade<0.5,0,1))
Comfort_Q12$Group<-factor(ifelse(Comfort_Q12$Group<1.01,1,2))
Comfort_Q12$CRT_DFollowUp<-factor(ifelse(Comfort_Q12$CRT_DFollowUp<0.5,0,1))
Comfort_Q12$CRT_PFollowUp<-factor(ifelse(Comfort_Q12$CRT_PFollowUp<0.5,0,1))
Comfort_Q12$ICD_DualFollowUp<-factor(ifelse(Comfort_Q12$ICD_DualFollowUp<0.5,0,1))
Comfort_Q12$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q12$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_DualFollowUp<-factor(ifelse(Comfort_Q12$PPM_DualFollowUp<0.5,0,1))
Comfort_Q12$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q12$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q12$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q12$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q12$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q12$SQ_ICDFollowUp<0.5,0,1))


# Charlston index

library(dplyr)

Comfort_Q12<- Comfort_Q12 %>% mutate(across(starts_with("Charlson"), round))

# Fried score

Comfort_Q12 %>% count(TOTAL....Fried.Score)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q12 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q12 <- Comfort_Q12 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q12 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Test <- Comfort_Q12

# Making test predictions/predict the probabilities of extraction due to infection

probabilities <- model %>% predict(Test, type = "response")
probabilities


# predict the class of individuals/The following R code categorizes individuals into two groups based on their predicted probabilities

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
head(predicted.classes)
table(predicted.classes)

# Assessing model accuracy

mean(predicted.classes == Test$Deviceremovedduetoinfection1_yes)

predicted.classes

# Confusion matrix

confusionMatrix(as.factor(predicted.classes), as.factor(Test$Deviceremovedduetoinfection1_yes))
