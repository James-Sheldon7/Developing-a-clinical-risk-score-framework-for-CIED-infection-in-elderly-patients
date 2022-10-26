# Developing a clinical risk score framework for cardiac implantable electronic device infection in
#elderly patients

#Includes all stages of analysis excluding implementation of SMOTE solely on training split 

# Note: many of the same chunks of code are repeated. If need comments for further understanding/clarification 
# please find first appearance of chunk in script 

rm(list=ls()) # Clearing environment 

# Importing relevant libraries 

library(ggplot2)
library(naniar)
library(visdat)
library(dplyr)
library(caret)
library(cowplot)
library(tidyr)
library(missMDA)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
library("scatterplot3d")

Comfort_Q <- read.csv("Copy of Comffort Q FINAL DATA sheet1csv.csv") #reading in raw dataset

View(Comfort_Q)

#Turning any blank spaces and any abbreviations of NA (e.g. N/A) to NA's 

Comfort_Q2 <- Comfort_Q                                   
Comfort_Q2[Comfort_Q2 == ""] <- NA                    
Comfort_Q2  

Comfort_Q2 <- Comfort_Q2                                  
Comfort_Q2[Comfort_Q2 == "N/A"] <- NA 

Comfort_Q2 <- Comfort_Q2                                    
Comfort_Q2[Comfort_Q2 == "n/a"] <- NA                     
Comfort_Q2   


Comfort_Q2 %>% count(CRP) # check


# Cleaning device removed due to infection column 

Comfort_Q2[Comfort_Q2 == 'no' | Comfort_Q2== 'NO'] <- 'No'
Comfort_Q2[Comfort_Q2 == 'R.I.P 04/06/2018' | Comfort_Q2== 'R.I.P 27/03/2017'] <- 'No'
Comfort_Q2[Comfort_Q2 == 'R.I.P 28/03/2017' | Comfort_Q2== 'RIP'] <- 'No'
Comfort_Q2[Comfort_Q2 == 'NO ' | Comfort_Q2== 'RIP'] <- 'No'

Comfort_Q2[Comfort_Q2 == 'YES'] <- 'Yes'

Comfort_Q2[Comfort_Q2 == 'DNA' | Comfort_Q2== 'Moved area'] <- NA
Comfort_Q2[Comfort_Q2 == 'No recent record' | Comfort_Q2== 'No recent record '] <- NA
Comfort_Q2[Comfort_Q2 == 'Not Known' | Comfort_Q2== 'Unknown'] <- NA
Comfort_Q2[Comfort_Q2 == 'UNKNOWN' | Comfort_Q2 == 'Other'] <- NA

Comfort_Q2 %>% count(Device.Removed.Due.to.Infection.) # check 

# Cleaning unable to follow up column 

Comfort_Q2 %>% count(Unable.to.Follow.Up)

Comfort_Q2[Comfort_Q2 == 'yes'] <- 'Yes'

Comfort_Q2 %>% count(Unable.to.Follow.Up) # check 

# Cleaning upgrades, downgrades column

Comfort_Q2 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures) 

Comfort_Q2[Comfort_Q2 == "0 (lead revision x 1)" ] <- "0"

Comfort_Q2[Comfort_Q2 == " " ] <- NA

Comfort_Q2 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

class(Comfort_Q2$Number.of.Upgrades...Downgrade.Device.Implant.Procedures) #character

Comfort_Q2$Number.of.Upgrades...Downgrade.Device.Implant.Procedures <- as.numeric(as.character(Comfort_Q2$Number.of.Upgrades...Downgrade.Device.Implant.Procedures)) # converting from character to numeric 

# Cleaning eGFR column

Comfort_Q2 %>% count(eGFR)

Comfort_Q2$eGFR[Comfort_Q2$eGFR == ">37"|Comfort_Q2$eGFR == ">60" ] <- NA
Comfort_Q2$eGFR[Comfort_Q2$eGFR == ">60"|Comfort_Q2$eGFR == ">90" ] <- NA
Comfort_Q2$eGFR[Comfort_Q2$eGFR == ">90.0"| Comfort_Q2$eGFR =="-" ] <- NA

Comfort_Q2 %>% count(eGFR)


# Cleaning device type at follow-up death (includes removal of any ILR samples at follow up)

Comfort_Q2 %>% count(Device.Type.at.Follow.Up...Death)

Comfort_Q2[Comfort_Q2 == 'ICD - Single' | Comfort_Q2 == 'ICD-single'] <- 'ICD-Single'
Comfort_Q2[Comfort_Q2 == 'ICD'] <- 'ICD-Single'

Comfort_Q2[Comfort_Q2 == 'ICD - Dual' | Comfort_Q2 == 'ICD-DUAL'] <- 'ICD-Dual'

Comfort_Q2[Comfort_Q2 == 'CRT - D' | Comfort_Q2 == 'CRT -D'] <- 'CRT-D'
Comfort_Q2[Comfort_Q2 == 'CRT-D' | Comfort_Q2 == 'ICD - CRT-D'] <- 'CRT-D'
Comfort_Q2[Comfort_Q2 == 'ICD - CRTD' | Comfort_Q2 == 'CRT -D'] <- 'CRT-D'
Comfort_Q2[Comfort_Q2 == 'CRTD'] <- 'CRT-D'


Comfort_Q2[Comfort_Q2 == 'UNK' | Comfort_Q2 == "Box Change"] <- NA
Comfort_Q2[Comfort_Q2 == 'Did not proceed'] <- NA


Comfort_Q2[Comfort_Q2 == 'PPM - Single' | Comfort_Q2 == 'PPM- Single'] <- 'PPM-Single'
Comfort_Q2[Comfort_Q2 == 'PPM-SINGLE' | Comfort_Q2 == 'PPM- Single'] <- 'PPM-Single'
Comfort_Q2[Comfort_Q2 == 'PPM'] <- 'PPM-Single'


Comfort_Q2[Comfort_Q2 == 'PPM - Dual' | Comfort_Q2 == 'PPM-dual'] <- 'PPM-Dual'
Comfort_Q2[Comfort_Q2 == 'PPM-DUAL' | Comfort_Q2 == 'PPM-Dual'] <- 'PPM-Dual'
Comfort_Q2[Comfort_Q2 == 'PPM - Dual ' | Comfort_Q2 == 'PPM-dual'] <- 'PPM-Dual'

Comfort_Q2[Comfort_Q2 == 'no device' | Comfort_Q2 == 'No device'] <- 'No Device'
Comfort_Q2[Comfort_Q2 == 'No Device ' | Comfort_Q2 == 'No device'] <- 'No Device'

Comfort_Q2[Comfort_Q2 == 'PPM - CRT-P'] <- 'CRT-P'

Comfort_Q2[Comfort_Q2 == 'PPM - Leadless'] <- 'PPM-Leadless'

Comfort_Q2 %>% count(Device.Type.at.Follow.Up...Death)

Comfort_Q2 <- filter(Comfort_Q2, Device.Type.at.Follow.Up...Death != "ILR") ## removing patients that have ILRs 


# Removal of redundant features/columns and additional filtering

Comfort_Q2 <- filter(Comfort_Q2, Unable.to.Follow.Up == "No") # anyone who is unable to follow up will be removed from the dataset

Comfort_Q3 <- Comfort_Q2[, 1:115] # removing completely empty columns at end (of excel file)


# Cleaning EQ-5D-5L Index 

Comfort_Q3 %>% count(EQ.5D.5L.Mobility)

Comfort_Q3$EQ.5D.5L.Anxiety...Depression[Comfort_Q3$EQ.5D.5L.Anxiety...Depression == 'Not recorded']<- NA
Comfort_Q3$EQ.5D.5L.Health.Today[Comfort_Q3$EQ.5D.5L.Health.Today == 'Not recorded']<- NA                                         
Comfort_Q3$EQ.5D.5L.Mobility[Comfort_Q3$EQ.5D.5L.Mobility == 'Not recorded']<- NA
Comfort_Q3$EQ.5D.5L.Health.Today[Comfort_Q3$EQ.5D.5L.Health.Today == 'Not recorded']<- NA
Comfort_Q3$EQ.5D.5L.Pain...Discomfort[Comfort_Q3$EQ.5D.5L.Pain...Discomfort == 'Not recorded']<- NA
Comfort_Q3$EQ.5D.5L.Usual.Activities[Comfort_Q3$EQ.5D.5L.Usual.Activities == 'Not recorded']<- NA
Comfort_Q3$EQ.5D.5L.Self.Care[Comfort_Q3$EQ.5D.5L.Self.Care == 'Not recorded']<- NA


# Differing number of NA's between weight and height  

Comfort_Q3 %>% count(Weight..kg.) # NA = 0
Comfort_Q3 %>% count(Height..cm.) # NA = 59 

# Cleaning potassium column 

Comfort_Q3 %>% count(Potassium)

Comfort_Q3$Potassium[Comfort_Q3$Potassium == '432']<- NA #converting typo/incorrect units entry to NA

Comfort_Q3 %>% count(Potassium)                               

# Cleaning white blood cell counts

Comfort_Q3 %>% count(WCC)

Comfort_Q3$WCC[Comfort_Q3$WCC == '143.0'|Comfort_Q3$WCC == "145.0"]<- NA # converting typo/incorrect units entry to NA
Comfort_Q3$WCC[Comfort_Q3$WCC == '199.0'|Comfort_Q3$WCC == "248.0"]<- NA
Comfort_Q3$WCC[Comfort_Q3$WCC == '280.0'|Comfort_Q3$WCC == "362.0"]<- NA
Comfort_Q3$WCC[Comfort_Q3$WCC == '394.0'|Comfort_Q3$WCC == "740.0"]<- NA
Comfort_Q3$WCC[Comfort_Q3$WCC == '43.0'|Comfort_Q3$WCC == "50.0"]<- NA
Comfort_Q3$WCC[Comfort_Q3$WCC == '64.8'|Comfort_Q3$WCC == "69"]<- NA
Comfort_Q3$WCC[Comfort_Q3$WCC == '72.0'|Comfort_Q3$WCC == "80"]<- NA

Comfort_Q3 %>% count(WCC)

# Filtering redundant columns 

Comfort_Q4 <- Comfort_Q3[ -c(2,3,4,5,8,22,48,50,51:70,72:76,78:89,104,105,106,110,112,115) ] #Removing columns/information that offer no use to the analysis

# Removing empty sample

Comfort_Q5 <- Comfort_Q4[-c(2430),]

# Removing CRP column 

Comfort_Q5 %>% count(CRP) ## NA is 1572, ~80% missing values, needs to be removed 

# Removing additional columns and CRP columns 

Comfort_Q6 <- Comfort_Q5[ -c(1,28,52,54,56) ] ## removing Comfort_ID column, CRP column, Date of Follow Up, Reason for Inability to Follow Up, Date of Removal Procedure for Infection

# One hot encoding 

# One hot encoding CIED.Replacement.or.Upgrade

Replacement <- Comfort_Q6[c(15)] # saving column/feature in variable for one hot encoding 

Replacement %>% count(CIED.New.Replacement.or.Upgrade)

# Additional cleaning before one hot encoding 

Replacement[Replacement == 'new'] <- 'New'
Replacement[Replacement == 'Box Change '] <- 'Replacement'


Replacement %>% count(CIED.New.Replacement.or.Upgrade)

# One hot encoding 
dummy <- dummyVars(" ~ .", data=Replacement)
newdata <- data.frame(predict(dummy, newdata = Replacement)) 
#View(newdata)

colnames(newdata) # Viewing column names 

# Changing column names for clearness 

names(newdata)[1] <- "New"
names(newdata)[2] <- "Replacement"
names(newdata)[3] <- "Upgrade"


# One hot encoding LV.function

LV_function <- Comfort_Q6[c(17)]

# Additional cleaning before one hot encoding 

LV_function %>% count(LV.function.classification..Normal...55...Mild.45.54...moderate.36.44...severe..35.)

LV_function[LV_function == 'Mild to moderate'] <- 'Moderate'
LV_function[LV_function == 'Normal '] <- 'Normal'
LV_function[LV_function == 'Not done'| LV_function == "Not recorded"] <- NA
LV_function[LV_function == 'Not stated'] <- NA


LV_function %>% count(LV.function.classification..Normal...55...Mild.45.54...moderate.36.44...severe..35.)

# One hot encoding 

dummy3 <- dummyVars(" ~ .", data=LV_function)
newdata3 <- data.frame(predict(dummy3, newdata = LV_function)) 

# Changing column names for clearness

names(newdata3)[1] <- "Mild_LV"
names(newdata3)[2] <- "Moderate_LV"
names(newdata3)[3] <- "Normal_LV"
names(newdata3)[4] <- "Severe_LV"


# One hot encoding "Device.Type.at.Follow.Up...Death" 

Device_death <- Comfort_Q6[c(49)] # saving column/feature in variable for one hot encoding 

# One hot encoding 

Device_death %>% count(Device.Type.at.Follow.Up...Death)

dummy4 <- dummyVars(" ~ .", data= Device_death)
newdata4 <- data.frame(predict(dummy4, newdata = Device_death)) 

# Changing column names for clearness
colnames(newdata4)

names(newdata4)[1] <- "CRT_DFollowUp"
names(newdata4)[2] <- "CRT_PFollowUp"
names(newdata4)[3] <- "ICD_DualFollowUp"
names(newdata4)[4] <- "ICD_SingleFollowUp"
names(newdata4)[5] <- "MicraImplantFollowUp"
names(newdata4)[6] <- "NodeviceFollowUp"
names(newdata4)[7] <- "PPM_DualFollowUp"
names(newdata4)[8] <- "PPM_SingleFollowUp"
names(newdata4)[9] <- "PPM_LeadlessFollowUp"
names(newdata4)[10] <- "SQ_ICDFollowUp"

#View(newdata4)

# One hot encoding Device removed due to infection

Device_removed <- Comfort_Q6[c(51)] #saving column/feature in variable for one hot encoding 

Device_removed %>% count(Device.Removed.Due.to.Infection.)

# One hot encoding

dummy <- dummyVars(" ~ .", data=Device_removed)
newdata10 <- data.frame(predict(dummy, newdata = Device_removed)) 

#View(newdata10)

# Changing column name for clearness

names(newdata10)[2] <- "Deviceremovedduetoinfection1_yes"

newdata10 <- newdata10[2] # saving single column to variable (0 -> no, 1-> yes)
# Combining one hot encoded columns 

One_hot_encode <- cbind(newdata, newdata3, newdata4,newdata10)

summary(One_hot_encode)

dim(One_hot_encode)

class(One_hot_encode$New)

# Converting from numeric to integer 

One_hot_encode$New <- as.integer(One_hot_encode$New)
One_hot_encode$Replacement <- as.integer(One_hot_encode$Replacement)
One_hot_encode$Upgrade <- as.integer(One_hot_encode$Upgrade)
One_hot_encode$Mild_LV <- as.integer(One_hot_encode$Mild_LV)
One_hot_encode$Moderate_LV <- as.integer(One_hot_encode$Moderate_LV)
One_hot_encode$Normal_LV <- as.integer(One_hot_encode$Normal_LV)
One_hot_encode$Severe_LV <- as.integer(One_hot_encode$Severe_LV)
One_hot_encode$CRT_DFollowUp <- as.integer(One_hot_encode$CRT_DFollowUp)
One_hot_encode$CRT_PFollowUp <- as.integer(One_hot_encode$CRT_PFollowUp)
One_hot_encode$MicraImplantFollowUp <- as.integer(One_hot_encode$MicraImplantFollowUp)
One_hot_encode$PPM_DualFollowUp <- as.integer(One_hot_encode$PPM_DualFollowUp)
One_hot_encode$PPM_SingleFollowUp <- as.integer(One_hot_encode$PPM_SingleFollowUp)
One_hot_encode$PPM_LeadlessFollowUp <- as.integer(One_hot_encode$PPM_LeadlessFollowUp)
One_hot_encode$NodeviceFollowUp <- as.integer(One_hot_encode$NodeviceFollowUp)
One_hot_encode$SQ_ICDFollowUp <- as.integer(One_hot_encode$SQ_ICDFollowUp)
One_hot_encode$ICD_DualFollowUp <- as.integer(One_hot_encode$ICD_DualFollowUp)
One_hot_encode$ICD_SingleFollowUp <- as.integer(One_hot_encode$ICD_SingleFollowUp)
One_hot_encode$Deviceremovedduetoinfection1_yes <- as.integer(One_hot_encode$Deviceremovedduetoinfection1_yes)


# Removing non one hot encoded columns from the dataframe 

Comfort_Q7 <- Comfort_Q6[-c(15,16,17,18,49,50,51,55:59)]  # this is the right one 

View(Comfort_Q7)

Comfort_Q7 <- Comfort_Q7[-c(47)] #Removing additional column, received appropriate ATP therapy   


# Combining dataframes 

Comfort_Q8 <- cbind(Comfort_Q7, One_hot_encode)

View(Comfort_Q8)

# Filter ILR by samples that are yes (this is at study start rather than at follow up)

Comfort_Q8 %>% count(P...ILR) 

Comfort_Q8 <- filter(Comfort_Q8, P...ILR != 1)

Comfort_Q8 %>% count(P...ILR)

# Cleaning platelets column 

Comfort_Q8 %>% count(Plts)

Comfort_Q8$Plts[Comfort_Q8$Plts== 'clotted'|Comfort_Q8$Plts == 'platelets dumped'] <- NA

Comfort_Q8 %>% count(Plts)

# Remove column as now offers to use to model (Column P...ILR) 

Comfort_Q8 <- Comfort_Q8[-c(10)]

# Removal of one hot encoded column -> no device at follow up as byproduct of the analysis

Comfort_Q8 <- Comfort_Q8[-c(58)]

# Don't want to impute the response/outcome variable -> removal of any patients with NA in outcome variable

Comfort_Q8 %>% count(Deviceremovedduetoinfection1_yes)  # 0-1817 + 1-16 + NA -71

Comfort_Q8 %>% drop_na(Deviceremovedduetoinfection1_yes)

Comfort_Q8 <- Comfort_Q8[!is.na(Comfort_Q8$Deviceremovedduetoinfection1_yes),]

summary(Comfort_Q8)

# Calculating EQ.5D.5L score total

Mobility <- paste0( Comfort_Q8$EQ.5D.5L.Mobility, collapse=",") #creating vectors for EQ-5D-5L index components
Mobility

Pain <- paste0( Comfort_Q8$EQ.5D.5L.Pain...Discomfort, collapse=",")
Pain

Health_today <- paste0( Comfort_Q8$EQ.5D.5L.Health.Today, collapse=",")
Health_today

Anxiety <- paste0( Comfort_Q8$EQ.5D.5L.Anxiety...Depression, collapse=",")
Anxiety

Self_care <- paste0( Comfort_Q8$EQ.5D.5L.Self.Care, collapse=",")
Self_care

Usual_activities <- paste0( Comfort_Q8$EQ.5D.5L.Usual.Activities, collapse=",")
Usual_activities

#creating dataframes for each component 

scores.df1 <- data.frame(MO = c(2,1,4,2,NA,1,1,3,4,3,2,2,4,3,1,3,3,1,2,3,4,3,4,1,1,1,1,3,1,1,2,1,2,4,4,2,3,3,4,3,2,4,2,1,3,5,2,NA,1,1,1,2,1,1,4,2,1,4,2,4,3,2,1,2,2,2,1,1,1,4,3,3,1,4,4,1,4,1,1,2,2,1,2,3,2,1,3,1,1,1,2,1,1,2,4,2,3,2,1,1,3,2,1,4,1,2,4,1,3,1,1,2,4,2,1,1,4,2,3,2,2,1,1,4,3,4,3,3,1,1,1,2,3,3,1,1,2,2,1,1,3,3,1,3,2,1,1,1,2,1,2,3,1,1,1,2,4,2,1,1,2,2,2,1,3,1,1,1,3,2,1,2,1,1,1,1,3,1,1,4,2,2,3,4,1,1,4,1,3,1,3,1,3,1,1,3,1,3,2,1,3,3,1,3,3,3,1,2,3,3,1,1,2,3,3,1,1,1,1,1,1,3,2,1,1,3,3,1,2,1,3,1,1,1,4,1,1,1,1,2,2,1,2,1,3,1,1,4,1,2,3,1,3,3,1,4,3,NA,1,1,1,1,1,1,1,1,1,2,1,1,3,3,2,2,1,1,1,3,1,1,3,3,2,1,2,2,3,1,1,4,1,3,2,2,2,1,1,1,1,3,1,1,1,1,1,3,1,2,3,1,2,1,1,1,3,1,3,1,1,1,1,3,1,2,1,1,4,1,1,4,1,3,2,2,1,3,1,1,1,1,2,1,3,2,4,1,1,1,3,1,1,1,1,1,1,2,2,1,1,3,2,1,2,1,2,1,2,2,2,4,3,1,3,1,1,1,2,2,3,2,1,1,2,2,3,2,1,2,1,2,1,3,1,3,1,2,2,3,1,1,1,1,1,2,3,2,1,2,1,1,2,2,3,1,1,1,1,3,1,1,1,3,1,3,3,1,1,1,2,3,2,2,1,1,2,1,4,3,1,3,3,2,4,1,4,1,2,2,1,1,4,2,1,1,1,1,1,1,3,1,2,1,3,3,4,3,3,3,1,1,2,4,3,2,2,3,1,4,1,1,3,1,1,1,3,3,1,2,4,1,1,1,3,1,3,3,2,1,1,4,1,2,1,1,3,2,1,1,2,2,5,3,4,4,1,3,2,2,1,2,2,2,1,2,1,1,1,1,1,1,1,4,1,1,1,4,1,1,1,3,1,5,2,2,3,3,1,1,1,1,3,1,3,1,1,1,2,1,2,3,2,1,2,3,2,1,4,1,4,5,1,1,3,1,3,1,3,1,4,1,4,3,3,4,3,1,1,1,3,1,3,1,3,2,1,2,1,3,1,2,2,3,1,1,3,2,4,2,1,1,2,1,1,4,4,1,1,1,3,1,1,1,5,4,1,2,2,1,3,NA,3,1,2,1,1,1,3,4,4,1,1,1,3,1,1,3,2,1,1,1,2,2,1,1,1,1,1,3,1,1,1,1,3,3,1,3,3,1,1,1,2,2,3,3,1,1,1,1,4,3,1,1,1,1,1,1,1,1,1,2,1,1,2,2,3,4,1,1,4,1,1,1,1,1,2,3,1,3,3,1,1,1,4,3,1,1,1,3,1,1,2,2,1,3,4,4,3,1,1,1,3,1,5,4,3,1,1,3,2,1,2,2,4,2,2,1,2,1,1,1,1,4,1,3,3,2,3,3,4,1,2,1,1,3,1,3,1,3,2,1,2,3,1,4,1,1,4,1,3,3,2,1,2,1,1,1,3,3,4,2,4,1,2,4,3,1,1,2,3,4,2,3,1,2,1,1,1,1,1,3,2,1,1,3,2,3,2,1,1,2,1,3,1,2,2,4,3,2,2,2,2,1,3,1,2,1,1,2,3,1,1,3,2,1,1,4,4,1,1,1,2,2,1,3,1,1,1,2,4,2,3,1,2,4,2,2,1,2,2,1,3,1,1,4,2,1,4,1,1,1,1,2,1,3,1,2,4,3,4,1,2,2,2,4,3,2,1,1,2,1,2,1,3,2,1,3,3,2,2,2,3,2,3,1,2,1,3,1,NA,3,1,3,3,1,2,4,2,2,3,1,3,2,1,3,3,3,3,3,1,1,1,2,3,4,3,1,1,NA,3,1,3,4,4,5,1,4,1,2,2,1,1,3,3,3,2,1,1,1,3,2,4,1,2,2,2,4,2,1,1,2,2,3,3,3,3,2,2,2,1,1,1,2,2,3,2,2,4,1,3,3,2,4,3,3,2,1,2,3,3,1,2,2,3,2,3,3,4,4,3,2,1,2,2,3,3,2,2,2,3,2,3,4,2,1,2,2,1,2,2,1,1,3,1,4,3,1,3,1,3,3,2,3,1,2,2,3,1,4,1,2,1,1,1,2,1,3,2,2,1,3,1,2,1,1,1,2,4,2,3,1,2,3,3,3,3,1,1,1,4,1,3,1,2,2,5,1,1,2,4,1,2,1,3,4,1,1,1,4,3,3,2,3,1,3,2,1,2,2,1,1,1,1,1,3,1,1,4,1,1,2,2,1,2,4,4,3,2,2,1,2,2,2,2,3,2,3,3,1,2,2,1,4,3,2,2,3,2,1,2,4,3,1,1,1,3,2,3,1,3,2,1,4,1,1,1,3,1,2,2,2,3,1,4,2,1,1,1,3,1,1,2,1,3,1,1,2,2,1,4,3,3,2,2,3,2,4,1,1,1,3,NA,2,1,3,1,1,1,1,1,2,1,1,2,2,2,3,2,1,1,1,1,1,2,3,NA,1,3,3,1,1,1,1,4,1,1,4,1,2,2,1,2,4,1,3,3,2,1,1,1,2,3,3,1,1,1,3,1,2,1,3,1,1,1,3,2,3,1,1,4,3,1,3,1,2,1,1,NA,1,4,3,1,1,4,3,3,1,4,2,3,1,1,1,3,1,2,3,1,3,2,1,2,4,3,1,3,2,1,2,2,3,3,1,3,3,2,3,3,1,1,3,4,1,2,1,1,NA,3,3,1,1,1,2,3,2,4,1,NA,1,3,2,1,1,3,1,4,4,1,2,1,3,NA,4,1,1,2,3,2,1,4,4,1,1,1,1,1,1,2,1,1,3,1,1,3,1,3,4,1,3,1,2,2,1,1,1,2,1,1,4,1,3,3,1,2,3,4,2,2,2,4,1,1,1,1,4,1,2,1,1,2,1,1,1,2,1,1,2,1,3,3,1,3,2,1,1,1,4,4,3,3,4,2,1,1,4,1,1,3,NA,2,2,2,1,2,1,4,1,1,1,3,4,1,3,1,1,1,1,2,3,2,1,3,3,1,4,1,3,4,2,1,2,4,1,1,3,3,1,2,1,2,1,1,1,2,2,3,1,2,2,2,1,1,2,4,1,1,3,2,2,NA,3,1,2,3,1,3,4,1,3,2,1,1,3,2,2,2,1,3,3,3,2,1,4,3,2,2,3,4,1,1,3,1,3,3,1,3,1,1,1,1,3,3,2,2,1,1,3,2,3,1,2,1,2,3,2,1,1,1,3,2,3,1,3,1,1,2,1,1,1,3,2,1,1,2,2,3,1,1,3,1,3,1,3,1,3,1,1,1,1,1,1,1,1,2,1,3,1,1,2,1,1,2,4,1,2,1,1,2,2,1,1,3,1,3,1,2,2,2,1,2,1,2,1,2,3,1,1,1,2,1,2,1,1,3,1,3,1,1,1,1,3,1,3,3,2,2,4,2,4,2,2,4,1,1,3,1,4,1,1,4,3,1,3,1,3,1,2,4,4,2,4,1,1,1,1,3,1,3,1,2,1,2,1,4,2,1,3,4,1,1,1,1,4,2,2,1,1,1,2,1,5,3,1,3,2,4,3,1,3,1,3,2,1,3,3,1,1,2,2,5,3,1,3,3,4,1,3,3,5,1,1,3,3,4,4,1,1,4,3,1,1,1,2,1,3,4,1,2,1,1,3,3,1,1,1,2,4,3,2,1,1,1,3,1,1,2,1,1,3,2,3,3,1,1,3,4,4,1,1,1,4,1,1,1,1,2,1,1,2,2,2,1,4,1,3,2,2,4,3,1,3,3,4,2,3,1,1,3,3,2,3,2,3,2,3,2,1,3,3,2,3,5,2,4))
                                
scores.df2 <- data.frame(SC= c(2,2,1,3,NA,2,1,2,4,2,1,1,2,1,1,1,2,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,2,2,1,1,1,1,2,1,1,1,1,2,2,1,NA,1,1,1,1,1,1,3,1,1,3,1,3,1,1,1,1,1,2,1,1,1,2,2,2,2,2,1,1,3,1,1,2,1,1,1,3,NA,1,2,1,1,1,1,1,1,3,1,1,1,1,1,1,2,1,1,3,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,3,2,1,1,3,3,3,2,2,1,1,1,4,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,2,1,1,1,1,1,3,1,1,1,1,1,1,1,3,3,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,3,1,1,2,1,2,1,1,1,3,1,1,1,1,1,1,1,2,1,1,4,3,2,1,1,1,1,1,1,1,2,2,1,1,1,2,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,1,2,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,4,2,1,1,1,1,1,3,1,1,2,1,1,1,1,1,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,2,1,3,1,1,1,2,1,1,3,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,3,2,1,2,1,1,1,1,1,1,3,2,1,2,1,1,1,1,2,3,2,1,1,1,1,3,1,1,1,1,1,1,2,1,2,1,2,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,3,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,2,1,3,1,1,1,1,1,1,3,3,1,1,2,1,2,1,1,2,1,1,1,1,1,1,2,5,1,1,1,2,1,3,3,1,1,1,1,1,1,1,1,2,1,1,1,1,2,2,1,1,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,3,1,1,2,4,1,3,2,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,1,1,1,2,2,1,3,1,1,3,1,1,2,2,1,1,1,1,1,1,2,2,2,1,1,2,1,2,1,1,1,1,1,1,2,1,3,1,1,1,1,1,1,3,3,1,1,1,3,1,1,2,4,4,3,1,2,2,1,NA,3,1,1,1,1,1,2,3,1,1,1,1,1,1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,2,1,3,1,1,1,1,4,1,2,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,2,2,1,1,1,1,1,1,1,3,1,2,1,1,1,1,2,1,5,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,2,1,2,1,2,2,1,2,1,1,1,1,2,2,1,3,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,2,1,1,1,2,1,1,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,3,1,1,1,1,1,1,3,1,1,1,3,1,1,1,1,1,4,1,1,1,1,1,1,2,1,1,3,1,1,3,1,1,1,1,1,1,1,1,1,1,1,3,1,1,2,1,2,3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,4,1,1,1,3,1,NA,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,3,1,1,1,NA,3,1,1,1,1,2,1,1,1,1,1,1,1,3,2,1,1,1,1,1,2,1,3,1,1,1,2,2,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,3,2,1,1,1,1,1,3,1,2,1,1,1,4,2,1,1,1,1,1,2,3,1,1,2,1,1,3,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,4,1,1,1,3,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,1,1,3,1,1,3,1,1,3,2,1,1,1,1,1,1,3,1,1,1,1,1,1,3,1,1,1,1,3,1,1,1,1,1,1,2,1,3,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,2,1,4,1,1,1,1,NA,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,NA,2,2,2,1,1,1,1,2,1,2,1,1,1,1,1,2,2,1,1,1,1,1,1,1,2,1,3,1,1,1,3,1,1,1,2,1,1,3,3,1,1,1,1,3,3,1,2,1,1,1,1,NA,1,2,1,1,1,3,1,3,1,2,1,3,1,1,1,2,1,1,2,1,1,1,1,1,5,3,1,1,1,1,1,1,2,1,1,1,2,1,2,3,1,1,3,3,1,1,1,1,NA,1,1,1,1,1,1,1,1,2,1,NA,1,2,1,1,1,1,1,3,3,1,1,1,2,NA,3,1,1,1,2,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,3,1,1,1,1,1,1,1,1,2,1,1,3,1,1,2,1,1,2,1,1,1,1,4,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,2,1,2,1,1,3,1,1,2,1,3,2,2,1,3,1,1,1,2,1,1,1,NA,1,1,1,1,3,1,1,1,1,1,2,3,1,3,1,1,1,1,1,2,1,2,2,1,1,3,1,1,3,1,1,1,4,1,1,2,1,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,1,1,3,1,1,2,1,1,NA,2,1,1,1,1,1,4,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,3,1,1,1,1,3,1,1,1,1,1,1,1,2,1,1,1,1,1,5,1,2,1,1,1,1,1,1,1,1,1,2,1,3,1,2,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,4,1,3,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,2,1,1,1,2,3,1,1,1,1,1,1,1,2,1,1,3,1,1,1,1,2,1,1,3,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,3,2,2,1,3,1,1,1,1,1,1,1,1,1,1,1,1,4,1,1,1,3,3,3,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,3,1,3,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,1,1,1,1,2,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,3,1,1,1,1,2,1,1,1,1,4,1,1,1,1,2,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,3,2,3,2,3,1,1,2,1,1,4,4,2,3))

scores.df3 <-data.frame(UA = c(3,2,2,3,NA,3,1,5,5,3,2,1,4,2,1,1,3,1,5,3,4,3,4,2,2,1,1,3,1,1,2,2,2,3,4,2,2,2,4,3,1,4,1,1,2,3,1,NA,1,1,1,3,2,3,3,2,2,3,2,5,4,1,1,1,2,2,1,1,1,4,3,3,2,3,2,1,5,1,1,4,2,1,1,5,1,1,3,1,1,1,2,2,1,2,4,3,3,2,1,3,2,2,1,3,1,2,3,1,2,1,1,2,3,2,1,2,4,3,2,2,3,1,1,5,3,4,3,4,1,1,2,3,3,3,2,1,2,2,3,1,2,4,3,3,2,2,1,1,1,1,4,1,1,1,2,2,3,1,1,1,1,2,1,1,3,1,1,2,2,2,1,NA,1,1,1,2,1,2,1,5,2,2,1,5,1,1,3,2,1,1,5,4,3,2,1,3,1,1,2,1,2,2,1,3,2,4,1,5,3,2,2,2,2,3,3,1,1,1,3,1,2,3,3,1,1,2,3,1,1,1,2,1,1,1,4,1,1,1,1,1,2,1,3,1,1,3,1,4,2,2,2,2,1,3,2,4,1,NA,1,1,1,1,5,1,1,3,1,2,1,2,3,1,2,1,1,1,1,5,1,1,2,3,2,1,1,2,1,3,1,3,1,2,2,2,2,1,5,1,1,2,1,1,1,1,1,2,1,3,1,2,3,1,1,1,2,1,1,2,1,2,2,3,2,1,1,3,1,1,1,3,1,1,3,3,1,3,1,2,1,1,2,1,1,3,4,1,1,1,2,1,1,3,1,1,1,1,2,1,1,3,2,1,2,1,1,1,2,1,1,5,3,1,3,1,1,1,2,2,2,3,1,1,2,2,5,2,1,3,1,1,1,1,2,2,1,1,1,3,1,1,1,1,1,2,3,2,1,3,1,5,1,2,5,1,1,1,1,1,1,1,1,5,1,1,2,1,1,1,1,3,1,2,1,1,1,1,4,1,1,4,1,2,4,1,4,1,2,2,1,1,4,1,1,1,2,1,1,1,2,1,3,1,3,1,2,1,2,1,2,1,2,4,3,2,1,2,1,2,3,1,5,1,1,2,3,2,1,2,5,1,1,2,2,1,2,1,3,1,1,3,3,1,2,1,2,2,2,1,1,2,2,2,4,4,1,3,3,1,1,2,2,2,3,1,1,2,2,1,2,1,1,4,1,1,1,4,1,1,3,4,2,5,2,2,1,2,1,2,1,3,3,1,2,1,1,1,3,1,2,3,1,1,2,4,2,1,3,2,4,5,1,1,4,2,4,1,2,1,1,3,3,2,3,3,2,2,1,1,1,1,3,3,2,1,1,2,1,3,1,1,4,3,1,1,3,4,4,2,2,1,1,1,1,4,4,1,1,1,3,5,1,3,5,4,3,2,2,4,3,NA,3,1,1,3,1,1,3,3,4,1,1,1,2,2,1,3,1,1,1,2,2,2,1,1,1,2,1,2,1,1,1,2,3,3,4,2,3,2,1,1,1,2,3,3,1,1,1,1,5,5,1,1,1,1,1,1,1,3,1,2,2,3,3,4,3,2,1,2,2,1,1,1,1,1,5,1,1,3,2,1,3,1,2,2,1,2,1,3,1,1,1,1,1,5,3,3,3,1,1,1,4,1,5,2,3,1,1,2,2,1,1,2,3,1,2,1,1,1,1,1,1,2,1,2,3,1,2,1,5,1,1,1,2,1,1,1,4,2,3,1,1,2,1,4,1,1,3,1,3,2,2,1,2,1,1,2,2,3,3,2,2,3,3,3,2,1,1,1,3,4,2,4,1,1,1,1,1,1,1,1,2,1,3,3,2,2,2,3,1,2,3,2,1,1,3,3,3,3,2,1,2,1,3,1,2,1,1,2,1,1,1,2,2,1,1,4,3,1,2,1,1,1,1,3,1,1,1,5,4,2,1,1,1,4,1,2,1,2,4,1,4,2,3,3,2,1,4,2,1,1,1,1,1,4,1,1,3,2,4,1,1,5,4,3,5,1,1,2,2,1,5,2,2,1,1,2,2,1,1,3,2,2,3,1,2,1,3,5,NA,3,1,2,2,1,2,4,1,1,3,1,1,2,5,1,2,1,3,2,1,1,1,1,2,3,2,1,1,NA,3,1,3,4,3,3,1,1,1,2,2,1,1,3,5,1,1,2,2,1,4,2,4,1,2,1,2,5,2,1,1,1,2,1,3,4,3,3,3,1,1,2,1,1,2,2,3,2,4,1,1,3,2,3,3,3,1,2,3,3,2,1,2,2,3,2,2,4,3,3,2,2,1,3,2,3,3,1,2,1,2,3,3,3,1,1,3,3,1,3,3,2,2,3,2,5,2,2,3,4,2,1,3,3,1,1,2,1,1,5,2,3,1,2,1,2,1,3,2,3,1,3,1,1,1,1,2,2,3,1,3,1,1,2,4,3,3,1,1,1,2,1,2,1,3,2,4,1,1,3,5,1,4,1,1,3,1,1,1,3,4,2,3,1,1,2,3,1,3,1,1,1,1,1,1,2,2,2,4,1,1,4,5,1,3,2,2,2,3,2,1,2,1,1,1,2,2,3,3,1,4,4,2,5,3,2,1,2,2,1,1,3,2,1,2,2,3,2,4,1,1,2,3,3,1,2,1,2,1,2,2,2,3,1,5,2,1,2,2,3,1,1,2,1,3,1,1,3,2,1,2,3,3,2,2,3,1,4,1,1,1,2,NA,3,1,4,1,1,1,1,1,3,1,1,2,2,1,3,2,1,3,1,1,1,5,4,NA,2,3,4,1,1,2,1,4,1,2,4,1,2,2,1,2,3,1,3,3,3,1,1,1,1,2,3,1,1,1,3,1,2,1,2,1,1,1,3,2,1,1,1,4,3,1,3,1,3,1,2,NA,1,2,3,1,1,3,3,3,1,3,1,3,1,1,1,3,1,1,3,1,2,2,1,2,5,3,1,2,1,1,3,2,3,2,1,3,2,2,3,5,1,1,3,1,1,1,4,1,NA,3,3,1,1,1,1,3,2,2,2,NA,1,1,2,1,2,1,2,2,4,1,3,1,2,NA,3,1,1,3,2,3,1,4,4,1,1,1,1,1,1,3,2,2,2,1,1,3,1,2,5,1,2,3,3,2,1,1,1,3,1,1,2,1,3,3,1,2,5,3,2,5,1,4,2,1,1,1,3,1,1,2,1,2,1,1,1,3,1,2,2,1,2,3,1,3,2,1,2,1,3,4,3,3,3,2,1,1,3,1,1,2,NA,1,2,3,1,2,1,4,1,1,1,4,3,1,4,1,1,1,1,2,4,3,2,3,1,2,4,1,2,5,2,1,1,4,1,1,3,2,1,2,1,1,1,2,1,1,1,3,1,2,2,2,1,1,1,3,1,1,2,2,1,NA,3,1,2,2,1,2,4,2,3,2,1,1,1,2,1,1,2,1,1,3,2,2,4,3,1,2,2,3,1,2,2,1,2,3,1,1,1,1,1,1,2,3,3,2,1,1,2,1,4,1,3,1,1,2,2,2,3,2,3,1,3,1,3,1,1,1,1,1,1,3,3,1,1,2,2,3,1,1,1,2,3,1,3,1,3,1,1,1,1,1,1,1,1,3,1,2,1,1,3,1,1,2,4,2,2,1,1,3,1,1,1,4,1,3,1,2,2,2,1,1,1,1,2,1,2,1,1,1,2,1,2,1,1,3,1,3,5,2,1,2,2,1,2,3,1,2,2,2,2,4,3,1,1,1,3,1,2,2,2,2,3,4,1,1,3,5,2,4,5,2,4,2,1,2,2,2,1,2,2,2,2,1,1,5,3,3,1,5,1,1,5,1,3,3,3,1,2,2,2,1,4,1,1,3,4,4,3,1,3,2,3,2,1,2,3,1,1,3,2,5,3,2,5,3,5,1,2,3,5,1,3,3,3,3,4,1,5,3,2,1,1,1,2,1,2,2,1,2,4,1,1,3,1,1,1,2,2,4,2,1,1,2,3,1,1,2,3,1,4,1,3,1,1,1,1,3,3,1,1,1,5,1,1,1,1,3,1,3,3,1,1,3,4,2,3,1,3,2,3,1,2,2,3,2,3,1,1,4,3,2,2,1,5,2,3,2,1,1,3,2,4,5,2,3))

scores.df4 <- data.frame(PD = c(3,2,5,2,NA,3,1,3,3,2,2,1,4,2,1,1,2,2,1,2,2,3,4,2,1,1,2,1,3,1,2,2,1,2,2,2,1,3,1,3,3,1,1,2,2,1,2,NA,1,1,1,3,1,2,4,3,2,2,2,3,3,1,1,1,2,2,1,1,1,2,5,3,2,2,3,1,3,2,2,3,2,1,2,4,2,1,3,1,1,1,2,2,1,2,1,1,2,3,1,1,3,2,1,3,1,3,2,1,2,2,1,2,3,4,1,1,3,2,3,2,1,1,2,2,3,4,1,4,2,2,3,3,1,3,3,2,1,1,2,1,1,3,1,4,2,3,1,1,2,1,2,2,1,1,1,2,3,1,1,1,1,2,1,1,2,1,1,3,1,3,1,2,2,1,1,2,2,1,1,3,1,2,1,3,2,1,3,3,4,2,3,1,2,2,2,3,1,2,3,1,3,3,1,4,3,4,1,3,2,2,1,2,1,2,2,1,1,2,2,1,2,2,2,2,1,2,2,1,1,1,1,1,1,1,3,2,1,2,1,3,1,1,3,1,2,1,1,1,2,2,3,2,1,1,2,3,3,2,1,1,1,1,3,1,1,3,1,2,1,1,3,3,2,1,2,1,1,5,1,2,2,3,1,3,3,1,1,1,1,3,2,3,1,2,1,2,2,2,1,1,1,1,1,1,1,2,2,1,1,2,2,1,1,2,2,2,2,2,1,2,1,2,2,2,2,2,4,1,1,3,1,2,1,2,3,3,1,NA,2,1,2,1,1,3,1,1,4,1,2,1,2,3,1,2,2,1,2,1,3,2,2,1,3,2,1,2,1,1,1,5,3,2,3,1,1,1,1,2,3,2,2,2,4,1,1,2,1,2,1,1,1,1,2,3,1,2,1,3,1,1,2,1,2,1,3,1,1,1,2,2,2,1,1,1,1,2,1,1,1,1,1,1,1,1,3,2,1,1,1,2,2,2,2,1,3,3,3,1,2,3,3,1,3,1,4,1,3,2,1,1,3,3,1,1,1,1,2,1,1,1,4,1,2,3,2,2,2,2,4,1,1,3,2,3,1,3,2,4,1,2,3,2,1,1,3,2,1,2,3,2,1,3,3,1,3,4,2,1,2,5,2,2,1,1,2,1,1,2,3,2,1,2,3,3,1,2,1,2,1,2,2,2,2,3,2,1,3,2,2,1,1,2,1,1,1,3,1,2,2,3,1,4,2,1,3,3,2,2,2,1,3,1,2,1,1,1,1,1,1,3,2,1,1,2,3,2,3,2,4,4,1,2,2,1,1,3,3,1,2,2,1,3,1,3,2,2,1,2,2,1,1,4,3,2,2,3,1,3,1,1,4,1,3,1,4,3,2,3,2,1,2,2,1,2,4,2,2,2,3,4,1,1,1,4,3,2,2,1,3,NA,3,1,2,3,1,1,1,3,1,1,1,2,1,2,1,1,1,1,1,2,3,1,1,2,2,3,1,3,3,2,3,3,1,1,2,2,2,3,2,1,1,2,1,3,2,1,1,2,1,1,1,3,2,1,1,2,1,1,1,1,1,3,4,1,2,3,3,1,2,1,1,1,1,1,3,3,1,1,2,1,2,4,4,1,2,3,1,1,1,3,2,3,1,3,3,1,1,2,2,2,3,2,2,1,2,1,1,2,4,1,4,2,3,2,2,1,2,1,1,2,1,3,1,4,1,2,3,1,4,1,4,1,2,3,2,1,3,1,3,2,2,4,3,4,2,2,3,4,1,2,3,1,2,1,1,2,2,1,3,2,3,4,2,1,3,3,1,2,3,1,2,3,1,2,1,1,1,2,2,1,2,1,1,4,1,3,1,1,1,1,1,2,1,2,2,3,3,1,1,2,1,1,3,1,1,2,1,2,2,3,1,3,1,2,1,3,4,1,1,2,1,1,1,3,2,3,2,4,4,2,1,2,1,2,2,2,2,3,1,2,3,1,1,3,2,1,4,4,1,2,1,1,1,1,1,1,3,2,1,2,1,4,3,3,3,1,1,2,2,1,1,1,3,1,1,2,1,2,3,3,2,1,3,2,1,1,2,2,NA,3,1,4,4,1,1,3,1,1,1,2,3,3,1,3,3,1,1,2,1,1,1,2,3,4,2,1,1,NA,4,3,3,3,3,5,2,2,2,3,1,3,2,2,1,3,1,1,1,1,4,3,3,1,2,3,2,5,1,1,2,1,2,2,3,3,3,1,1,2,1,1,1,4,2,1,1,3,3,1,1,3,2,3,3,3,2,1,1,1,3,2,1,3,3,1,2,1,4,3,2,2,1,2,2,1,3,1,1,1,3,1,2,3,1,3,3,1,1,3,2,2,1,1,1,5,4,1,3,2,2,1,3,2,1,2,1,2,1,5,2,3,1,2,1,1,1,1,2,3,1,3,1,3,1,2,2,2,1,2,4,3,3,3,3,1,3,1,1,2,2,1,1,1,2,3,2,1,1,1,1,1,4,1,1,3,2,1,2,3,1,3,2,1,1,4,3,1,1,1,1,1,1,1,2,1,2,1,4,2,1,3,3,1,1,2,1,1,3,2,2,1,1,1,1,1,1,3,4,1,3,3,2,5,1,2,1,3,3,1,2,2,3,1,1,1,2,3,4,1,1,3,1,2,1,1,1,2,1,2,2,2,4,1,2,2,1,3,2,1,1,1,2,1,3,1,1,1,4,1,1,3,3,4,3,3,1,3,1,1,1,1,NA,1,1,2,1,1,1,1,1,1,2,1,2,1,1,3,1,1,2,2,3,1,1,2,NA,1,2,3,1,2,1,1,3,1,3,4,1,1,1,1,2,1,1,3,2,2,1,1,1,1,2,4,1,1,2,2,2,2,1,2,1,1,1,3,3,1,1,2,3,4,2,2,1,1,1,1,NA,1,3,2,2,1,3,1,3,1,3,1,3,1,1,1,3,1,2,2,1,1,1,2,3,4,1,1,3,1,1,1,1,2,3,1,2,1,1,2,3,1,1,2,3,1,2,2,1,NA,1,1,2,1,1,1,2,2,3,2,NA,1,1,2,3,2,1,1,1,2,3,1,2,2,NA,3,3,1,1,1,1,1,4,2,2,1,1,1,1,1,3,2,2,3,1,2,2,1,3,2,1,1,2,3,3,1,1,1,2,1,1,2,1,2,4,1,1,4,2,2,1,2,5,3,1,1,1,4,1,2,2,1,2,1,1,1,1,1,1,3,1,3,3,1,3,1,1,NA,1,3,3,3,2,3,1,1,2,3,1,3,2,NA,1,3,1,3,2,1,3,1,1,2,3,3,1,3,1,2,1,1,1,2,2,1,3,1,2,1,1,2,4,1,1,2,3,2,2,2,3,1,1,1,4,2,2,2,1,1,3,3,4,1,1,1,1,1,5,1,1,2,2,2,NA,2,2,2,2,1,1,4,2,3,3,1,1,1,3,1,2,2,2,3,4,2,2,5,3,1,1,3,4,2,1,2,2,2,3,1,1,1,1,2,1,2,3,1,1,1,2,2,1,3,1,2,2,1,3,1,3,3,1,3,1,2,1,2,1,1,1,1,2,1,3,3,1,1,2,3,4,1,1,2,2,1,1,3,1,1,1,3,1,1,2,2,2,1,3,2,1,1,2,1,1,2,3,5,2,3,4,1,1,2,1,1,1,1,1,2,2,3,1,1,1,2,1,1,1,2,1,1,2,3,1,1,2,1,1,1,3,1,1,2,1,4,1,3,3,1,2,1,2,2,2,2,3,1,1,2,1,1,1,1,3,4,1,1,1,3,2,1,4,NA,2,3,1,1,2,1,3,1,3,2,1,1,1,1,5,3,2,1,4,1,2,2,1,3,2,1,1,1,1,1,1,3,2,1,1,4,1,3,1,1,2,4,1,1,2,1,2,1,1,3,3,3,1,3,1,1,2,2,1,1,3,1,3,2,1,3,1,2,1,3,1,1,2,2,1,2,3,1,1,2,1,1,2,1,1,1,2,4,4,1,1,1,3,1,1,1,1,2,1,3,1,3,1,3,1,1,4,2,1,3,2,4,1,2,1,1,4,1,1,1,2,1,1,1,1,1,1,1,3,3,1,3,1,3,2,3,3,1,2,1,3,2,1,1,1,4,2,1,2,2,1,1,2,2,1))

scores.df5 <- data.frame (AD = c(3,3,3,3,NA,1,1,5,3,1,2,2,1,1,1,1,2,2,1,1,2,2,2,2,2,1,1,2,3,2,4,1,1,3,1,1,1,1,2,2,1,1,1,2,2,1,1,NA,2,3,1,3,1,1,2,2,1,2,1,3,3,1,1,1,2,2,1,1,1,1,1,1,1,2,1,1,1,1,2,1,2,1,1,5,1,1,2,1,1,1,1,1,1,1,3,1,2,1,1,2,2,1,2,1,3,2,1,1,1,1,1,2,1,1,1,2,2,1,2,1,1,1,2,2,2,4,1,1,1,1,1,2,1,1,1,2,2,1,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,1,1,1,3,1,1,1,2,1,2,1,1,1,2,1,3,1,3,1,2,1,1,3,1,2,1,2,1,1,1,2,1,4,2,1,1,1,2,2,1,2,2,2,1,1,2,2,2,2,1,2,2,4,1,1,1,2,2,2,2,2,3,2,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,4,1,1,5,1,1,1,1,3,1,2,4,1,1,1,1,1,1,1,2,1,2,3,1,1,1,1,1,1,1,1,2,5,1,2,1,2,2,2,1,1,4,1,1,2,1,1,1,2,2,2,1,1,1,1,3,2,2,1,1,1,4,1,1,1,1,1,1,1,2,2,1,2,2,2,2,2,1,1,1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,1,2,2,1,1,3,1,2,3,1,1,1,3,2,3,1,1,1,1,1,1,3,1,1,1,1,1,2,1,1,2,2,1,1,1,1,1,1,2,4,2,1,4,2,1,1,1,1,4,2,1,1,3,1,2,1,1,1,1,1,3,2,3,2,1,1,1,1,1,1,3,1,1,1,2,1,2,2,1,1,1,1,1,3,1,1,2,1,1,1,3,1,3,2,1,1,1,2,1,2,2,2,1,1,2,1,3,1,2,3,3,1,1,1,4,1,1,3,2,2,1,3,2,1,1,1,1,1,1,1,1,2,1,1,1,2,1,1,1,1,2,1,1,1,1,1,2,1,1,1,2,1,1,1,2,3,1,1,2,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,4,1,2,2,1,1,2,1,1,1,1,1,1,1,1,2,1,1,1,2,1,1,1,1,1,1,3,1,2,2,1,1,2,1,1,1,3,1,1,1,3,2,1,1,1,1,1,1,1,1,2,2,2,1,2,1,1,1,2,1,4,1,1,1,3,2,1,2,1,1,1,1,2,2,1,1,1,2,1,1,1,2,3,4,3,2,2,2,2,1,1,1,1,1,2,1,2,3,2,1,1,1,1,1,1,3,1,1,1,1,2,1,1,1,1,2,1,1,1,1,NA,1,1,1,1,1,1,3,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,3,2,2,1,1,3,2,1,1,2,3,1,2,1,1,1,2,1,1,1,2,1,2,5,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,2,4,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,2,1,3,1,1,1,3,1,1,1,2,2,2,1,2,1,1,1,2,1,1,3,1,2,1,1,4,1,1,1,1,2,1,4,3,3,4,2,3,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,3,5,1,2,2,1,1,1,3,3,1,2,1,1,1,3,2,2,1,2,1,1,1,2,1,3,1,1,1,2,2,1,2,1,1,1,1,2,1,2,1,2,1,1,3,2,1,2,1,1,1,2,4,1,1,1,4,1,1,1,1,1,1,2,1,2,2,5,1,2,1,1,1,4,1,2,1,1,2,1,1,2,1,3,1,1,1,2,1,2,1,1,1,1,1,1,1,1,4,1,2,1,3,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,NA,2,1,1,3,1,1,3,2,2,1,2,2,1,1,3,1,1,5,2,3,1,1,1,2,2,1,1,1,NA,1,1,3,2,3,1,1,2,1,1,1,1,2,1,1,1,1,1,2,1,2,2,2,2,1,1,1,4,1,1,2,1,2,1,1,2,2,1,2,1,1,2,1,2,3,2,2,1,1,1,1,1,2,1,1,1,1,1,2,2,2,1,1,3,1,2,1,2,1,1,3,1,1,2,1,1,3,1,1,1,3,1,2,1,2,1,4,1,1,3,2,2,2,2,2,1,1,1,1,1,1,2,3,1,1,1,1,1,2,3,1,1,1,1,2,2,1,2,2,1,1,1,2,2,1,1,1,4,2,2,1,1,3,1,2,1,2,1,1,1,3,1,1,1,1,2,1,1,1,2,3,1,1,2,1,2,1,1,1,1,2,2,1,3,1,1,2,1,1,3,1,1,1,1,1,1,2,1,3,2,1,4,3,1,1,3,1,1,2,2,2,1,1,2,1,1,3,2,1,1,3,1,2,3,2,2,1,2,2,2,1,2,3,2,1,1,1,2,4,2,1,2,2,1,1,1,1,2,2,1,2,1,1,1,1,1,1,3,1,2,1,1,1,1,3,1,1,1,1,1,1,1,1,2,2,1,2,3,1,1,1,2,NA,1,1,1,1,2,1,1,2,2,1,1,2,1,1,2,1,1,1,2,1,2,1,1,NA,2,3,2,1,1,1,1,2,1,1,3,1,1,1,1,3,2,1,4,2,1,1,1,1,1,1,1,2,2,1,2,3,1,1,2,1,3,1,3,2,1,1,1,1,3,1,1,NA,2,1,1,NA,3,2,1,2,1,4,2,5,1,2,1,3,2,1,1,1,1,1,2,1,1,1,1,2,4,3,1,1,1,1,1,1,2,2,2,2,2,1,2,1,1,1,2,1,2,1,2,1,NA,1,1,1,1,1,1,1,1,2,2,NA,1,1,2,1,1,1,3,3,3,1,3,1,2,NA,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,4,1,5,1,1,2,1,1,2,1,1,2,2,1,2,1,1,3,1,1,2,1,2,4,1,1,2,1,2,2,1,3,2,1,1,1,3,3,2,1,2,2,1,1,1,1,1,1,3,1,3,1,1,3,2,2,1,1,1,2,1,4,2,1,1,1,1,1,1,2,NA,1,1,2,3,2,1,2,1,1,1,2,1,1,3,4,1,3,1,1,2,1,1,3,1,2,1,1,2,4,3,1,1,1,4,1,1,2,1,2,1,3,1,4,1,1,1,1,1,1,1,2,2,5,1,2,1,1,1,1,1,NA,1,1,2,1,1,1,3,2,3,2,1,1,1,2,2,1,2,2,3,1,1,1,1,3,1,2,1,3,2,1,2,2,2,1,1,1,1,2,2,2,3,1,1,2,1,1,2,1,4,1,2,1,2,1,2,1,3,1,2,1,3,2,2,1,1,1,1,1,1,2,1,1,2,2,1,1,1,1,1,1,3,1,1,2,3,1,1,1,1,2,3,1,1,3,1,1,1,1,3,1,2,3,3,2,2,1,1,2,2,1,2,2,1,1,1,1,2,1,1,1,3,5,1,2,1,1,3,1,1,1,1,1,1,1,1,2,2,1,3,3,3,1,2,3,1,5,1,1,3,3,2,1,1,1,1,2,1,3,2,2,3,1,2,1,2,2,2,2,3,2,4,1,1,2,2,2,1,2,2,1,1,1,5,3,1,2,1,3,1,1,2,1,1,1,2,2,2,2,2,1,1,2,1,1,4,1,2,1,1,2,3,2,1,1,2,2,1,1,3,3,2,1,4,2,2,1,1,4,2,1,3,1,2,1,3,2,3,1,3,1,3,1,2,3,3,2,3,1,1,1,2,2,2,2,2,1,3,3,1,1,1,1,1,1,1,2,2,1,1,1,2,2,3,1,1,1,3,1,1,1,1,1,1,1,1,4,2,2,1,1,1,4,1,1,2,1,1,1,3,1,1,1,3,1,1,1,1,2,1,3,3,1,1,1,1,1,2,1,4,1,1,2,3,2))

scores.df <- cbind(scores.df1, scores.df2, scores.df3, scores.df4,scores.df5)  # creating dataframe in appropriate format that can be saved as a csv file and used in the EQ-5D-5L Shiny Browser

write.csv(scores.df,file ="scores.df.csv") #saving dataframe for exportation to shiny interface 

eq5dlscores <- read.csv("scores_uk_4thJuly.csv") #loading in dataframe containing total scores

eq5dlscores %>% count(Index)

View(Comfort_Q8)

# Replacing health today row with index scores 

for(i in 1:nrow(Comfort_Q8)) {       # for-loop over rows
Comfort_Q8$EQ.5D.5L.Health.Today <- eq5dlscores$Index
}

names(Comfort_Q8)[43] <- "EQ5DL Index Score" # changing column name to total score 

# Converting negative EQ-5D-5L index values to 0 to restore appropriate score range

Comfort_Q8 %>% count(`EQ5DL Index Score`)

Comfort_Q8$`EQ5DL Index Score`[Comfort_Q8$`EQ5DL Index Score`<0] <- 0

Comfort_Q8 %>% count(`EQ5DL Index Score`)   

# Removing individual EQ-5D-5L component columns 

Comfort_Q8 <- Comfort_Q8[-c(38:42)]

View(Comfort_Q8)


# Observing missingness following preprocessing

# visualising the missing data 

vis_miss(Comfort_Q8,sort_miss = TRUE) + theme(
axis.title.x = element_text(size = 10),
axis.title.y = element_text(size = 8))

# visualising the missing data and what classes the columns are 


vis_dat(Comfort_Q8) + theme(
axis.title.x = element_text(size = 8),
axis.title.y = element_text(size = 8),axis.text.x =  element_text(angle = 70, size=8, color = "black"),axis.text.y = element_text(size=8, colour = "black"),legend.text = element_text(size=8), legend.title = element_text(size=8), legend.justification = c("right", "top"), panel.grid.major.x =  element_line("black", lineend = "butt"), panel.grid.major.y = element_line("black")) + coord_cartesian(ylim = c(0, 1935), expand = FALSE) + scale_y_continuous(breaks = NULL) # visualises the whole dataframe at once. Provides information about class also


# Analytically determining Which/if certain samples should possibly be removed? 

Comfort_Q_NA <- Comfort_Q8

#View(Comfort_Q_NA)

na_rows = rowSums(is.na(Comfort_Q_NA))

which(na_rows > 2) 

which(na_rows > 10)

which(na_rows > 10)  

which(na_rows > 14) 

which(na_rows > 18) 

which(na_rows > 21)  # only patients 1180 and 1182 have greater 22 missing values

which(na_rows >25) # No row has more than 50% missing values, justifying there removal

# Don't want to impute the response variable so going to remove any NA in response column

Comfort_Q8 %>% count(Deviceremovedduetoinfection1_yes)

Comfort_Q8 %>% drop_na(Deviceremovedduetoinfection1_yes)

Comfort_Q8 <- Comfort_Q8[!is.na(Comfort_Q8$Deviceremovedduetoinfection1_yes),]

summary(Comfort_Q8)

# Cleaning of Potassium column 

Comfort_Q8 %>% count(Potassium)

Comfort_Q8$Potassium[Comfort_Q8$Potassium == "4,8"|Comfort_Q8$Potassium == "4..2"] <- NA

Comfort_Q8 %>% count(Potassium)

# Cleaning of Fried Score 

Comfort_Q8 %>% count(TOTAL....Fried.Score)

Comfort_Q8$TOTAL....Fried.Score[Comfort_Q8$TOTAL....Fried.Score == "Not Done"] <- NA
Comfort_Q8 %>% count(TOTAL....Fried.Score)

# Cleaning of Platelets column 

Comfort_Q8 %>% count(Plts)

Comfort_Q8$Plts[Comfort_Q8$Plts == "-"] <- NA

Comfort_Q8 %>% count(Plts)

# Cleaning of Urea column 

Comfort_Q8 %>% count(Urea)

Comfort_Q8$Urea[Comfort_Q8$Urea == "-"] <- NA

Comfort_Q8 %>% count(Urea)

# Cleaning of Creatinine column 

Comfort_Q8 %>% count(Creatinine)

Comfort_Q8$Creatinine[Comfort_Q8$Creatinine == "-"] <- NA

Comfort_Q8 %>% count(Creatinine)

# Cleaning of Hb column

Comfort_Q8 %>% count(Hb)

Comfort_Q8$Hb[Comfort_Q8$Hb == "-"] <- NA

Comfort_Q8 %>% count(Hb)

# Cleaning of White Blood Cell Count column 

Comfort_Q8 %>% count(WCC)

Comfort_Q8$WCC[Comfort_Q8$WCC == "-"] <- NA

Comfort_Q8$WCC[Comfort_Q8$WCC == "clotted" |Comfort_Q8$WCC == "N/K"] <- NA

Comfort_Q8 %>% count(WCC)

# Cleaning of eGFR column 

Comfort_Q8 %>% count(eGFR)

Comfort_Q8$eGFR[Comfort_Q8$eGFR == "54,0"] <- NA

Comfort_Q8 %>% count(eGFR)

summary(Comfort_Q8)

# Converting incorrectly formatted quantitative variables (characters) to numeric  

Comfort_Q8$Weight..kg. <- as.numeric(unlist(Comfort_Q8$Weight..kg.))
Comfort_Q8$Potassium <- as.numeric(unlist(Comfort_Q8$Potassium))
Comfort_Q8$Urea <- as.numeric(unlist(Comfort_Q8$Urea))
Comfort_Q8$Creatinine <- as.numeric(unlist(Comfort_Q8$Creatinine))
Comfort_Q8$eGFR <- as.numeric(unlist(Comfort_Q8$eGFR))
Comfort_Q8$Hb <- as.numeric(unlist(Comfort_Q8$Hb))
Comfort_Q8$WCC <- as.numeric(unlist(Comfort_Q8$WCC))
Comfort_Q8$Plts <- as.numeric(unlist(Comfort_Q8$Plts))

# Converting incorrectly formatted quantitative variables (characters) to integers

Comfort_Q8$TOTAL....Fried.Score <- as.integer(Comfort_Q8$TOTAL....Fried.Score) 
Comfort_Q8$Number.of.Box.Changes.in.Study.Period <- as.integer(Comfort_Q8$Number.of.Box.Changes.in.Study.Period)
Comfort_Q8$Number.of.Upgrades...Downgrade.Device.Implant.Procedures <- as.integer(Comfort_Q8$Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

# Final missingness plot before imputation

# vis_dat -> missingness and dataset structure 

windows(17,8)

vis_dat(Comfort_Q8) + theme(
axis.title.x = element_text(size = 10),
axis.title.y = element_text(size = 8),axis.text.x =  element_text(angle = 70, size=8, color = "black"),axis.text.y = element_text(size=8, colour = "black"),legend.text = element_text(size=8), legend.title = element_text(size=8), legend.justification = c("right", "top"), panel.grid.major.x =  element_line("black", lineend = "butt"), panel.grid.major.y = element_line("black")) + coord_cartesian(ylim = c(0, 1935), expand = FALSE) + scale_y_continuous(breaks = NULL) # visualises the whole dataframe at once. Provides information about class also

# vis_miss -> to get missingness proportions

vis_miss(Comfort_Q8) + theme(
axis.title.x = element_text(size = 8),
axis.title.y = element_text(size = 8),axis.text.x =  element_text(angle = 70, size=8, color = "black"),axis.text.y = element_text(size=8, colour = "black"),legend.text = element_text(size=8), legend.title = element_text(size=8), legend.justification = c("right", "top"), panel.grid.major.x =  element_line("black", lineend = "butt"), panel.grid.major.y = element_line("black")) + coord_cartesian(ylim = c(0, 1935), expand = FALSE) + scale_y_continuous(breaks = NULL) # visualises the whole dataframe at once. Provides information about class also

# Imputation for mixed dataset using missMDA package 

library(missMDA)
library(FactoMineR)

res.impute3 <- imputeFAMD(Comfort_Q8, ncp = 2)

res.imputtee <- res.impute3$tab.disj #accessing the imputed matrix 

Comfort_Q10 <- res.imputtee

class(Comfort_Q10)

Comfort_Q10 <- as.data.frame(Comfort_Q10) #Converting to dataframe 


# Correctly formatting binary variables 

Comfort_Q10$Mild_LV<-factor(ifelse(Comfort_Q10$Mild_LV<0.5,0,1))
Comfort_Q10$Moderate_LV<-factor(ifelse(Comfort_Q10$Moderate_LV<0.5,0,1))
Comfort_Q10$Normal_LV<-factor(ifelse(Comfort_Q10$Normal_LV<0.5,0,1))
Comfort_Q10$Severe_LV<-factor(ifelse(Comfort_Q10$Severe_LV<0.5,0,1))
Comfort_Q10$Gender.M.0.F.1<-factor(ifelse(Comfort_Q10$Gender.M.0.F.1<0.5,0,1))
Comfort_Q10$P...Single<-factor(ifelse(Comfort_Q10$P...Single<0.5,0,1))
Comfort_Q10$P...Dual<-factor(ifelse(Comfort_Q10$P...Dual<0.5,0,1))
Comfort_Q10$P.CRT<-factor(ifelse(Comfort_Q10$P.CRT<0.5,0,1))
Comfort_Q10$P...LL<-factor(ifelse(Comfort_Q10$P...LL<0.5,0,1))
Comfort_Q10$D...Single<-factor(ifelse(Comfort_Q10$D...Dual<0.5,0,1))
Comfort_Q10$CRT.D<-factor(ifelse(Comfort_Q10$CRT.D<0.5,0,1))
Comfort_Q10$D...SQ.ICD<-factor(ifelse(Comfort_Q10$D...SQ.ICD<0.5,0,1))
Comfort_Q10$Replacement<-factor(ifelse(Comfort_Q10$Replacement<0.5,0,1))
Comfort_Q10$New<-factor(ifelse(Comfort_Q10$New<0.5,0,1))
Comfort_Q10$MRA<-factor(ifelse(Comfort_Q10$MRA<0.5,0,1))
Comfort_Q10$Statins<-factor(ifelse(Comfort_Q10$Statins<0.5,0,1))
Comfort_Q10$Aspirin<-factor(ifelse(Comfort_Q10$Aspirin<0.5,0,1))
Comfort_Q10$Clopidogrel<-factor(ifelse(Comfort_Q10$Clopidogrel<0.5,0,1))
Comfort_Q10$Other.antiplatelets<-factor(ifelse(Comfort_Q10$Other.antiplatelets<0.5,0,1))
Comfort_Q10$Beta.blockers<-factor(ifelse(Comfort_Q10$Beta.blockers<0.5,0,1))
Comfort_Q10$Ace.Inhibitors<-factor(ifelse(Comfort_Q10$Ace.Inhibitors<0.5,0,1))
Comfort_Q10$ARBs<-factor(ifelse(Comfort_Q10$ARBs<0.5,0,1))
Comfort_Q10$Sacubitril.valsartan<-factor(ifelse(Comfort_Q10$Sacubitril.valsartan<0.5,0,1))
Comfort_Q10$Loop.diuretics<-factor(ifelse(Comfort_Q10$Loop.diuretics<0.5,0,1))
Comfort_Q10$Amiodarone<-factor(ifelse(Comfort_Q10$Amiodarone<0.5,0,1))
Comfort_Q10$Anticoagulant<-factor(ifelse(Comfort_Q10$Anticoagulant<0.5,0,1))
Comfort_Q10$Antibiotics<-factor(ifelse(Comfort_Q10$Antibiotics<0.5,0,1))
Comfort_Q10$Immunosuppressants<-factor(ifelse(Comfort_Q10$Immunosuppressants<0.5,0,1))
Comfort_Q10$Number.of.Box.Changes.in.Study.Period<-factor(ifelse(Comfort_Q10$Number.of.Box.Changes.in.Study.Period<0.5,0,1))


Comfort_Q10$Upgrade<-factor(ifelse(Comfort_Q10$Upgrade<0.5,0,1))
Comfort_Q10$Group<-factor(ifelse(Comfort_Q10$Group<1.01,1,2))
Comfort_Q10$CRT_DFollowUp<-factor(ifelse(Comfort_Q10$CRT_DFollowUp<0.5,0,1))
Comfort_Q10$CRT_PFollowUp<-factor(ifelse(Comfort_Q10$CRT_PFollowUp<0.5,0,1))
Comfort_Q10$ICD_DualFollowUp<-factor(ifelse(Comfort_Q10$ICD_DualFollowUp<0.5,0,1))
Comfort_Q10$ICD_SingleFollowUp<-factor(ifelse(Comfort_Q10$ICD_SingleFollowUp<0.5,0,1))
Comfort_Q10$MicraImplantFollowUp<-factor(ifelse(Comfort_Q10$MicraImplantFollowUp<0.5,0,1))
Comfort_Q10$PPM_DualFollowUp<-factor(ifelse(Comfort_Q10$PPM_DualFollowUp<0.5,0,1))
Comfort_Q10$PPM_SingleFollowUp<-factor(ifelse(Comfort_Q10$PPM_SingleFollowUp<0.5,0,1))
Comfort_Q10$PPM_LeadlessFollowUp<-factor(ifelse(Comfort_Q10$PPM_LeadlessFollowUp<0.5,0,1))
Comfort_Q10$SQ_ICDFollowUp<-factor(ifelse(Comfort_Q10$SQ_ICDFollowUp<0.5,0,1))
Comfort_Q10$Deviceremovedduetoinfection1_yes<-factor(ifelse(Comfort_Q10$Deviceremovedduetoinfection1_yes<0.5,0,1))
Comfort_Q10$D...Dual<-factor(ifelse(Comfort_Q10$D...Dual<0.5,0,1))

# Correctly formatting categorical variables 

#Fried score

#View(Comfort_Q8)

Comfort_Q10 %>% count(TOTAL....Fried.Score)

Comfort_Q10 <- Comfort_Q10 %>% mutate(across(starts_with("TOTAL....Fried.Score"), round))

Comfort_Q10 %>% count(TOTAL....Fried.Score)

# Upgrades and downgrades

Comfort_Q10 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

Comfort_Q10 <- Comfort_Q10 %>% mutate(across(starts_with("Number.of.Upgrades...Downgrade.Device.Implant.Procedures"), round))

Comfort_Q10 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

# Converting height into metres  

Comfort_Q9 <- Comfort_Q10

Comfort_Q9$Height..cm. <- Comfort_Q9$Height..cm.  / 100

# Squaring height 

Comfort_Q9$Height..cm. <- Comfort_Q9$Height..cm.^2

# Final calculation of BMI 

Comfort_Q9$Height..cm. <- Comfort_Q9[,4]/Comfort_Q9[,3]

#View(Comfort_Q9)

names(Comfort_Q9)[3] <- "BMI" #changing column name 

Comfort_Q10 <- Comfort_Q9[-c(4)] # removing redundant weight column  

# Correctly formatting EQ-5D-5L Index Score 

Comfort_Q10 %>% count(`EQ5DL Index Score`)

Comfort_Q10$`EQ5DL Index Score`[Comfort_Q10$`EQ5DL Index Score`>1] <- 1 # to get all values in appropriate score range

Comfort_Q10 %>% count(`EQ5DL Index Score`)

# Save for version control 

vis_dat(Comfort_Q10)

write.csv(Comfort_Q10, file ="Comfort_Q10.csv")


# Initial continuous variable plot 

names(Comfort_Q10)[37] <- "EQ5D5LIndex" # so don't get parse text error

mean(Comfort_Q10$Age) # mean age of the dataset 

Plot_continous <- Comfort_Q10[c(1,3,13:20,37)] # saving all continuous variables/features to variable 

# distribution plot (density plot(s)) of continuous variables 

my_plots <- lapply(names(Plot_continous), function(var_x){
p <- 
 ggplot(Plot_continous) +
 aes_string(var_x)

if(is.numeric(Plot_continous[[var_x]])) {
 p <- p + geom_density() +theme_gray(base_size=8) + labs(y="Density")
 
} else {
 p <- p + geom_bar() + base_size()
} 

})

windows(17,8)
plot_grid(plotlist = my_plots)


mean(Comfort_Q10$BMI) # obtaining mean body mass index for dataset  


# Extra plot of only age variable

Age_1 <- as.data.frame(Comfort_Q10$Age) # converting to dataframe for ggplot

names(Age_1)[1] <- "Age" # changing column name to more appropriate name 

# Intial plot 
p <- ggplot(Age_1, aes(x=Age)) + 
geom_density() + labs(y="Density")

# Adding red line to show position of mean 

p+ geom_vline(aes(xintercept=mean(Comfort_Q10$Age)),
           color="red", linetype="dashed", size=1)


mean(Comfort_Q10$Age) # getting mean age of dataset 

# Transformation of EQ5D5LIndex variable  

Comfort_Q10$EQ5D5LIndex<- sqrt(Comfort_Q10$EQ5D5LIndex) #square root transformation for right skewness

# Revised continous distribution plot following transformation 

Plot_continous <- Comfort_Q10[c(1,3,13:20,37)]

my_plots <- lapply(names(Plot_continous), function(var_x){
p <- 
 ggplot(Plot_continous) +
 aes_string(var_x)

if(is.numeric(Plot_continous[[var_x]])) {
 p <- p + geom_density() +theme_gray(base_size=8) + labs(y="Density")
 
} else {
 p <- p + geom_bar() + base_size()
} 

})

windows(17,8)

plot_grid(plotlist = my_plots)

# Min-max normalisation of continuous variables and resulting distribution plot

Plot_continous <- Comfort_Q10[c(1,3,13:20,37)]

minmaxnormalise <- function(x){(x- min(x)) /(max(x)-min(x))} # min-max normalisation 

Plot_continous <-lapply(Plot_continous, minmaxnormalise)

class(Plot_continous)

Plot_continous <- as.data.frame(Plot_continous) #converting to a dataframe 

class(Plot_continous)

my_plots <- lapply(names(Plot_continous), function(var_x){
p <-
 ggplot(Plot_continous) +
 aes_string(var_x)

if(is.numeric(Plot_continous[[var_x]])) {
 p <- p + geom_density() +theme_gray(base_size=8) + labs(y="Density")
 
} else {
 p <- p + geom_bar() + base_size()
}

})

windows(17,8)
plot_grid(plotlist = my_plots)

# Distribution plot for discrete variables (bar plots)

Plot_discrete <- Comfort_Q10[-c(1,3,13:20,37)] # saving all discrete features to variable via negative indexing

Plot_discrete2 <- Plot_discrete #creating new variable to edit names of columns/variables 

# Editing names of columns/variables for better presentation in distribution plot 

names(Plot_discrete2)[25] <- "Charlston.Score"

names(Plot_discrete2)[27] <- "No.of.Box.Changes"

names(Plot_discrete2)[28] <- "No.of.Upgrades.Downgrades"

names(Plot_discrete2)[45] <- "Device.Removed"


# Splitting distributions into two seperate graphs to aid better visualisation  

Plot_discrete3 <- Plot_discrete2[c(1:23)] 

Plot_discrete4 <- Plot_discrete2[c(23:45)]

# Distribution plot of first plot 

## base_size = 6 -> overall plot
## base_size = 8 -> for individual plots 

my_plots <- lapply(names(Plot_discrete3), function(var_x){
p <- 
 ggplot(Plot_discrete2) +
 aes_string(var_x)
if(is.character(Plot_discrete2[[var_x]])) {
 p <- p + geom_density() +theme_gray(base_size=6)
 
} else {
 p <- p + geom_bar() + theme_gray(base_size = 6) + theme(axis.title.x =  element_text(size=6)) +labs(y="Count")
} 

})

windows(17,8)

plot_grid(plotlist = my_plots)

# Distribution plot for second plot 

my_plots <- lapply(names(Plot_discrete4), function(var_x){
p <- 
 ggplot(Plot_discrete2) +
 aes_string(var_x)

if(is.character(Plot_discrete2[[var_x]])) {
 p <- p + geom_density() +theme_gray(base_size=6)
 
} else {
 p <- p + geom_bar() + theme_gray(base_size = 6) + theme(axis.title.x =  element_text(size=6)) +labs(y="Count")
} 

})

windows(17,8)
plot_grid(plotlist = my_plots)


# PCA for continuous predictors 

PCA_comfort <- Plot_continous #so that variables are normalised for PCA

pca_res1 <- prcomp(PCA_comfort, center=TRUE, scale=TRUE) # performing PCA

View(pca_res1)

pca_res1$x

pca_ress <-data.frame(pca_res1$x, Group = Comfort_Q10$Group) # creating dataframe for plotting 

prp <- pca_res1$sdev^2 * 100 / sum(pca_res1$sdev^2) # working out proportion of variance explained by each principal component 

#plotting 2D PCA labelled by group 

ggplot(pca_ress, aes(x= PC1, y=PC2, col = Group)) + geom_point(size=4) + labs(x = paste0("PC1 = ", prp[1], '%'), y = paste0("PC2 = ", prp[2], '%'))

# Saving for version control 

write.csv(PCA_comfort, file = "PCA_comfort.csv")

PCA_comfort2 <- read.csv("PCA_comfort.csv")

# 2D PCA with numerical labelling to visualise which samples are potential outliers 

ggplot(pca_ress, aes(x= PC1, y=PC2, col = Group)) + geom_point(size=4) + labs(x = paste0(prp[1], '%'), y = paste0(prp[2], '%')) +geom_label(aes(label = PCA_comfort2$X))

# Confirming outliers analytically via mahalanobis distance

windows(17,8)
mt::pca.outlier(PCA_comfort, center = TRUE, scale= TRUE,conf.level = 0.975, cex =0.5) #0.975 as it is the default

Outliers <- mt::pca.outlier(PCA_comfort, center = TRUE, scale= TRUE,conf.level = 0.975, cex =0)

Outliers$outlier #obtaining vector of outliers so can remove  

Table_outlier <- table(Outliers$outlier) #there are 64 outliers 

# 3D PCA plot - labeling by device removed due to infection 

library(plotly)

PCA_3D <- Comfort_Q10[c(1,3,13:20,37,56)]

X <- subset(PCA_3D, select = -c(Deviceremovedduetoinfection1_yes))

prp <- pca_res1$sdev^2 * 100 / sum(pca_res1$sdev^2) # to get proportion of variance explained by each principal component

xlab= paste0(prp[1], '%') # 20.8% (3.sf)
ylab= paste0(prp[2], '%') # 13.2% (3.sf)
zlab= paste0(prp[3], '%') # 12.0% (3.sf)

# Formatting axis titles 

axx <- list(
title = "PC1 = 20.8%"
)

axy <- list(
title = "PC2 = 13.2%"
)

axz <- list(
title = "PC3 = 12.0%"
)

tit = '3D PCA plot' # formatting title for PCA plot 

#Creating plot 

fig <- plot_ly(pca_ress, x = ~PC1, y = ~PC2, z = ~PC3, color = ~PCA_3D$Deviceremovedduetoinfection1_yes, colors = c('#636EFA','#EF553B','#00CC96') ) %>%add_markers(size = 10)


fig <- fig %>%
layout(
 title = tit,scene = list(xaxis=axx,yaxis=axy,zaxis=axz),
 scene = list(bgcolor = "#e5ecf6"))

fig

#  Combining (now) normalised (and transformed) continous variables and discrete variables

Comfort_Q10 <- cbind(PCA_comfort, Plot_discrete)

# Removing outliers 

Comfort_Q10 <- Comfort_Q10[-c(1,3,52,74,104,142,188,235,278,295,318,330,443,448,465,511,539,541,542,543,589,592,788,794,858,884,889,895,896,908,918,937,985,1046,1065,1070,1104,1161,1255,1263,1294,1297,1309,1333,1335,1366,1526,1543,1544,1644,1672,1673,1697,1703,1710,1715,1721,1729,1746,1748,1760,1769,1791,1831),]
# Correlation matrix for continuous predictors

Correlation_comfort <- PCA_comfort # creating new variable

windows(17,8)

rquery.cormat(Correlation_comfort, type="full") # plot of correlation matrix 

# Investigated correlations between variables further 

cor(Correlation_comfort$eGFR, Correlation_comfort$Creatinine) # -0.5380223 
cor(Correlation_comfort$Urea, Correlation_comfort$Creatinine) #0.4325085
cor(Correlation_comfort$WCC,Correlation_comfort$Plts) #0.3187701

# Correlation plot for discrete variables  

Correlation_discrete <- Plot_discrete[-c(45)] # to get rid of outcome variable 

windows(17,8)

library(ggcorrplot)
model.matrix(~0+., data = Correlation_discrete) %>% 
cor(use="pairwise.complete.obs") %>% 
ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=1,  tl.cex =5)

ggsave("cor_discrete.png")

# Correlation plot for everything (excluding outcome variable)

Correlation_everything <- Comfort_Q10[-c(56)] #to get rid of outcome variable 
windows(17,8)

model.matrix(~0+., data = Correlation_everything) %>% 
cor(use="pairwise.complete.obs") %>% 
ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=1,  tl.cex = 5)

#Correlation plot for frailty scores 

Comfort_q_frail_scores <- Comfort_Q10[c(11,36,37)]

windows(17,8)
model.matrix(~0+., data = Comfort_q_frail_scores) %>% 
cor(use="pairwise.complete.obs") %>% 
ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=4,  tl.cex = 10)

#Correlation plot for procedure related variables

windows(17,8)

View(Comfort_Q10)

Comfort_q_extra_procedures <- Comfort_Q10[c(38,39,40,41,42)]

Comfort_Q10 %>% count(Number.of.Box.Changes.in.Study.Period)

model.matrix(~0+., data = Comfort_q_extra_procedures) %>% 
cor(use="pairwise.complete.obs") %>% 
ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=4,  tl.cex = 7)

# Correlation plot for risk score variables (see in RF refined risk score model) 

View(Comfort_Q10)
Comfort_q_risk_score_cor <- Comfort_Q10[c(3,13,10,9,25,8,4,20,5,2)]
View(Comfort_q_risk_score_cor)

model.matrix(~0+., data = Comfort_q_risk_score_cor) %>% 
cor(use="pairwise.complete.obs") %>% 
ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=4,  tl.cex = 7)


Comfort_Q10 <- Comfort_Q10[-c(41)] #getting rid of "replacement" column due to high correlation with "new" column 

# Means of health-related scores across dataset 

mean(Comfort_Q10$EQ5D5LIndex)
mean(Comfort_Q10$Charlson.score.TOTAL)
mean(Comfort_Q10$TOTAL....Fried.Score)

Comfort_Q11 <- Comfort_Q10

# Exploring patients who had device removed due to infection

Comfort_Q_Device <- filter(Comfort_Q11, Deviceremovedduetoinfection1_yes == 1) # obtaining only patients who had device removed due to infection

Comfort_Q_Device %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)
Comfort_Q_Device %>% count(New)
Comfort_Q_Device %>% count(Upgrade)

mean(Comfort_Q_Device$Charlson.score.TOTAL)
mean(Comfort_Q_Device$TOTAL....Fried.Score)
mean(Comfort_Q_Device$EQ5D5LIndex)
mean(Comfort_Q_Device$Age)

write.csv(Comfort_Q11, file ="Comfort_Q11.csv") # for version control

# SMOTE Implementation

Comfort_Q11<-read.csv("Comfort_Q11.csv") #loading in dataset following preprocessing


Comfort_Q11 %>% count(Number.of.Box.Changes.in.Study.Period)

Comfort_Q11 %>% count(Number.of.Upgrades...Downgrade.Device.Implant.Procedures)

# Scope of imbalanced dataset problem => just under 1% yes

print(table(Comfort_Q11$Deviceremovedduetoinfection1_yes))

print(prop.table(table(Comfort_Q11$Deviceremovedduetoinfection1_yes)))

set.seed(58722)

library(smotefamily) # loading smote family package 

DATA <-SMOTE(Comfort_Q11, Comfort_Q11$Deviceremovedduetoinfection1_yes, K=5, dup_size = 30) #implementing SMOTE


Comfort_Q12 <- DATA$data #extracting SMOTE dataset


print(prop.table(table(Comfort_Q12$Deviceremovedduetoinfection1_yes))) #obtaining proportion in the minority class following SMOTE


print(table(Comfort_Q12$Deviceremovedduetoinfection1_yes)) #obtaining numbers in the minority class following SMOTE

# Reformatting (rounding) binary variables where appropriate

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
Comfort_Q12$Deviceremovedduetoinfection1_yes<-factor(ifelse(Comfort_Q12$Deviceremovedduetoinfection1_yes<0.5,0,1))


Comfort_Q12 <- Comfort_Q12[-c(1)]  #Removing redundant column, X column generated by loading in csv file

# Reformatting (rounding) categorical variables where appopriate 

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

write.csv(Comfort_Q12,file="SMOTE_draft.csv") #saving SMOTE dataset for version control

# Random Forest Implementation 

#Random Forest Implementation

# Initial Random Forest Model 

Comfort_Q18 <- read.csv("SMOTE_draft.csv") #loading in file from SMOTE implementation

library(dplyr)

Comfort_Q18 %>% count(Deviceremovedduetoinfection1_yes)

Comfort_Q20<- Comfort_Q18[-c(1,57)] # removing x column that is added in when you load a csv file as well as class column generated from SMOTE implementation


Comfort_Q20$Deviceremovedduetoinfection1_yes <- as.factor(Comfort_Q20$Deviceremovedduetoinfection1_yes) #converting to a factor so that implement as a classification problem

# Splitting data into train and test data

library(caret)
set.seed(10)
trainIndex <- createDataPartition(Comfort_Q20$Deviceremovedduetoinfection1_yes, p = .7,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

Train <- Comfort_Q20[ trainIndex,] # obtaining training split
Test  <- Comfort_Q20[-trainIndex,] # obtaining test split 

library(dplyr)


Train %>% count(Deviceremovedduetoinfection1_yes) #checking proportions are correct (createDataPartition has worked)
Test %>% count(Deviceremovedduetoinfection1_yes)

folds <- 10 #number of folds 
cvIndex <- createFolds(factor(Train$Deviceremovedduetoinfection1_yes), folds, returnTrain = T) # splitting the data into 10 folds. Proportions of outcome variable preserved in folds

# setting the properties of the train function -> repeated stratified k-fold cross-validation 

repeat_cv <- trainControl(index = cvIndex,method='repeatedcv', number=folds, repeats=10)

set.seed(6666)

# Training
forest <- train(
  
  
  Deviceremovedduetoinfection1_yes~., 
  
  
  data=Train, 
  
  
  method='rf', 
  
  
  trControl=repeat_cv,
  
  
  metric='Accuracy')

#Info about the model
forest$finalModel

var_imp <- varImp(forest, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)

View(var_imp)
# variable importance plot
var_imp %>%
  
  # Arranging by importance
  arrange(importance) %>%
  
  
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  
  geom_bar(stat='identity') + 
  
  
  coord_flip() + 
  
  
  xlab('Variables') +
  
  ylab('Importance') +
  
  
  labs(title='Random forest variable importance') + 
  
  
  theme_minimal() + 
  theme(axis.text = element_text(size = 7), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 20), 
  )


y_hats <- predict(object=forest, newdata=Test[, -55])

# Obtaining accuracy 
accuracy <- mean(y_hats == Test$Deviceremovedduetoinfection1_yes)*100
cat('Accuracy on testing data: ', round(accuracy, 3), '%',  sep='')

# Confusion Matrix for model 

confusionMatrix(table(y_hats, Test$Deviceremovedduetoinfection1_yes))

# Precision recall curve

#install.packages("PRROC")
library("PRROC")

fg <- y_hats[Test[,55] == 1] #classification scores for infection
bg <- y_hats[Test[,55] == 0] #classification scores for not infection 

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T) # calculating the area under the PR curve


windows(17,8)

plot(pr, cex.main= 2)

# Final refined risk score model 

Comfort_Q_Risk_score_2 <- Comfort_Q20[c(3,13,10,9,25,8,4,20,5,2,55)] #creating variable with top 10 pre-implant features + outcome variable 
View(Comfort_Q_Risk_score_2)

# Train-test split 

library(caret)
set.seed(611)
trainIndex <- createDataPartition(Comfort_Q_Risk_score_2$Deviceremovedduetoinfection1_yes, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

Train <- Comfort_Q_Risk_score_2[ trainIndex,]
Test  <- Comfort_Q_Risk_score_2[-trainIndex,]

library(dplyr)

Train %>% count(Deviceremovedduetoinfection1_yes)
Test %>% count(Deviceremovedduetoinfection1_yes)

folds <- 10 #number of folds 
cvIndex <- createFolds(factor(Train$Deviceremovedduetoinfection1_yes), folds, returnTrain = T) # splitting the data into 10 folds. Proportions of outcome variable preserved in folds

# setting the properties of the train function -> repeated stratified k-fold cross-validation 

repeat_cv <- trainControl(index = cvIndex,method='repeatedcv', number=folds, repeats=10)

set.seed(1121515)

# Training
forest <- train(
  
  
  Deviceremovedduetoinfection1_yes~., 
  
  
  data=Train, 
  
  
  method='rf', 
  
  
  trControl=repeat_cv,
  
  
  metric='Accuracy')

#Info about the model
forest$finalModel

var_imp <- varImp(forest, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)

# variable importance plot
var_imp %>%
  
  # Arranging by importance
  arrange(importance) %>%
  
  
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  
  geom_bar(stat='identity') + 
  
  
  coord_flip() + 
  
  
  xlab('Variables') +
  
  ylab('Importance') +
  
  
  labs(title='Random forest variable importance') + 
  
  
  theme_minimal() + 
  theme(axis.text = element_text(size = 7), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 20), 
  )


y_hats <- predict(object=forest, newdata=Test[, -11])

# Obtaining accuracy 
accuracy <- mean(y_hats == Test$Deviceremovedduetoinfection1_yes)*100
cat('Accuracy on testing data: ', round(accuracy, 3), '%',  sep='')


# Confusion Matrix for model 
confusionMatrix(table(y_hats, Test$Deviceremovedduetoinfection1_yes))

# Precision recall curve

#install.packages("PRROC")
library("PRROC")

fg <- y_hats[Test[,11] == 1] #classification scores for infection
bg <- y_hats[Test[,11] == 0] #classification scores for not infection 

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T) # calculating the area under the PR curve

#windows(17,8)

plot(pr, cex.main=2)

# Sample code for risk score usage

#Test <- # insert sample code for patient with appropriate pieces of information. Values for each of the 10 variables

Test <- Comfort_Q_Risk_score_2[c(6),] #testing code on patient 2 in Comfort_Q20/Comfort_Q_Risk_score_2, just to check that code works + as an example

library(randomForest)

y_pred <- randomForest:::predict.randomForest(forest$finalModel, Test, predict.all = TRUE)#predict.all = TRUE -> allows you to obtain a matrix where each column contains prediction by a tree in the forest.

predictions_500 <- y_pred$individual # obtaining individual predictions from each tree. Total of 500

predictions_500 <-t(predictions_500) # transposing to make it easier to work with 

predictions_500 <- as.data.frame(predictions_500) # converting to a dataframe 

names(predictions_500)[1] <- "Predictions" # changing name of column 

count <- predictions_500 %>% count(Predictions) # saving total (0/1) counts across all trees

proportion_of_trees_infection <- count[2,2]/500 # proportion of trees that predict infection out of 500

print(proportion_of_trees_infection) # printing results 


# Implementing a support vector machine

rm(list=ls())

# Loading in additional libraries 

library(caret)

library(ggrepel)

Comfort_QSVM <- read.csv("SMOTE_draft.csv") #reading in dataset following SMOTE implementation 

Comfort_QSVM<- Comfort_QSVM[-c(1,57)] #removing X column that appears when CSV file loaded in and class column at end which is a byproduct of the SMOTE implementation

Comfort_QSVM$Deviceremovedduetoinfection1_yes <- as.factor(Comfort_QSVM$Deviceremovedduetoinfection1_yes) ##converting to a factor so that implement as a classification problem

set.seed(132014736)

##Splitting into test and train proportions

trainIndex <- createDataPartition(Comfort_QSVM$Deviceremovedduetoinfection1_yes, p = .7, 
                               list = FALSE, 
                               times = 1)
head(trainIndex)

Train <- Comfort_QSVM[ trainIndex,]
Test  <- Comfort_QSVM[-trainIndex,]

table(Test$Deviceremovedduetoinfection1_yes)
table(Train$Deviceremovedduetoinfection1_yes)

folds <- 10 #number of folds 
cvIndex <- createFolds(factor(Train$Deviceremovedduetoinfection1_yes), folds, returnTrain = T) # splitting the data into 10 folds. Proportions of outcome variable preserved in folds

# setting the properties of the train function -> repeated stratified k-fold cross-validation 

tc <- trainControl(index = cvIndex,
                method = 'repeatedcv', 
                number = folds,
                repeats = 10)


# Model construction 

set.seed(528037621)

# Linear SVM 


svm_Linear <- train(Deviceremovedduetoinfection1_yes ~., data = Train, method = "svmLinear",
                 trControl=tc,
                 preProcess = c("center", "scale"), #help with centering and scaling the data
                 tuneLength = 10) #this is for tuning our algorithm

svm_Linear
# RBF SVM 

svm_Radial <- train(Deviceremovedduetoinfection1_yes ~., data = Train, method = "svmRadial",
                 trControl=tc,
                 preProcess = c("center", "scale"), #help with centering and scaling the data
                 tuneLength = 10) # tuning our algorithm, number of levels for each tuning parameter
svm_Radial

#assessing model performance on test data

# Linear SVM - confusion matrix 
test_pred <- predict(svm_Linear, newdata = Test)
test_pred

confusionMatrix(table(test_pred, Test$Deviceremovedduetoinfection1_yes))

test_pred_linear <- test_pred # for construction of PR curves

# RBF SVM - confusion matrix 

test_pred <- predict(svm_Radial, newdata = Test)
test_pred

confusionMatrix(table(test_pred, Test$Deviceremovedduetoinfection1_yes))

test_pred_radial <- test_pred

# Setting C parameter values (grid) 

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25,0.5, 1, 2)) # change till it actually improves the performance - haha 

# Tuning SVM Linear 

svm_Linear_Grid <- train(Deviceremovedduetoinfection1_yes ~., data = Train, method = "svmLinear",
                      trControl=tc,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid,
                      tuneLength = 10)

svm_Linear_Grid
windows(17,8)
plot(svm_Linear_Grid,"main" ="Accuracy at different C values (Cost)")

# assessing SVM Linear performance on test data following C parameter tuning

test_pred_grid <- predict(svm_Linear_Grid, newdata = Test) 
test_pred_grid

confusionMatrix(table(test_pred_grid, Test$Deviceremovedduetoinfection1_yes)) # obtaining confusion matrix

test_pred_svm_linear_c <- test_pred_grid 

# Extracting variable importance from RBF SVM - via sensitivity analysis 

library(rminer)

set.seed(676722)

# fit used to rebuild model with exact same parameter values

M <- fit(Deviceremovedduetoinfection1_yes ~., data=Train, model="svm", kpar=list(sigma=0.01252536), C=8)
svm.imp <- Importance(M, data=Train) #extracting variable importance 

plot(svm.imp$value) # initial plot of importance values (no labelling, only index positions)

svm_values<- svm.imp$value #saving importance values to variable 

class(svm_values)

svm_values

Comfort_QSVM_t <- t(Comfort_QSVM) # transposing original dataframe 

write.csv(Comfort_QSVM_t, file="Comfort_QSVM_t.csv") # saving for version control 

Comfort_QSVM_t2 <- read.csv("Comfort_QSVM_t.csv") #reading csv file in

Comfort_QSVM_t2$X #extracting variable names and associated index positions

combinedxxx <- cbind(Comfort_QSVM_t2$X,svm_values) #so that variable names match up with importance values

write.csv(combinedxxx, file="svm_importance.csv") # saving for version control and just in case

svm_importance <- read.csv("svm_importance.csv") # reading csv file in

View(svm_importance)

svm_importance <- svm_importance[c(-1)] # removing now unneeded index position information 

# Table ordered on importance values 

order_importance_svm <- svm_importance[order(-svm_importance$svm_values),]

# Formatting column names 

names(order_importance_svm)[1] <- "Variables" 


names(order_importance_svm)[2] <- "Importance Values"

View(order_importance_svm)

write.csv(order_importance_svm, file = "order_importance_svm_revised.csv") # saving for table construction in results


# PR curves 

# RBF SVM

library("PRROC")

fg <- test_pred_radial[Test[,55] == 1]
bg <- test_pred_radial[Test[,55] == 0]

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)

windows(17,8)

plot(pr, cex.main = 2)

# SVM Linear without C-value tuning 

library("PRROC")

fg <- test_pred_linear[Test[,55] == 1]
bg <- test_pred_linear[Test[,55] == 0]

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)

windows(17,8)

plot(pr, cex.main = 2)

# SVM Linear with C parameter tuning

library("PRROC")

fg <- test_pred_svm_linear_c[Test[,55] == 1]
bg <- test_pred_svm_linear_c[Test[,55] == 0]

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)

windows(17,8)

plot(pr, cex.main = 2)

#Logistic regression Implementation

#rm(list=ls())

# Loading in additional libraries 

library(caret)
library(dplyr)
library(tidyverse)

Comfort_Q18 <- read.csv("SMOTE_draft.csv") #loading in file 

Comfort_Q20<- Comfort_Q18[-c(1,57)]

Comfort_Q20$Deviceremovedduetoinfection1_yes <- as.factor(Comfort_Q20$Deviceremovedduetoinfection1_yes) #converting to a factor so that implement as a classification problem

set.seed(14696)

# Splitting into train and test splits 

trainIndex <- createDataPartition(Comfort_Q20$Deviceremovedduetoinfection1_yes, p = .7, 
                               list = FALSE, 
                               times = 1)
head(trainIndex)

Train <- Comfort_Q20[ trainIndex,]
Test  <- Comfort_Q20[-trainIndex,]

table(Test$Deviceremovedduetoinfection1_yes)
table(Train$Deviceremovedduetoinfection1_yes)

# Fitting logistic regression model

set.seed(17)


model <- glm( Deviceremovedduetoinfection1_yes ~ Age + Gender.M.0.F.1 + BMI + Group + P...Single + P...Dual + P.CRT + P...LL + D...Single + D...Dual + CRT.D + D...SQ.ICD + Sodium + Potassium + Urea + Creatinine + eGFR + Hb + WCC + Plts + Aspirin + Other.antiplatelets + Clopidogrel + Beta.blockers + Ace.Inhibitors + ARBs + Sacubitril.valsartan + MRA + Loop.diuretics + Statins + Anticoagulant + Amiodarone + Immunosuppressants + Charlson.score.TOTAL + TOTAL....Fried.Score + EQ5D5LIndex + Antibiotics + New + Upgrade + Number.of.Box.Changes.in.Study.Period + Number.of.Upgrades...Downgrade.Device.Implant.Procedures + Moderate_LV + Severe_LV + Mild_LV + PPM_DualFollowUp + PPM_LeadlessFollowUp + PPM_SingleFollowUp + CRT_PFollowUp + MicraImplantFollowUp + Normal_LV + CRT_DFollowUp + ICD_DualFollowUp + ICD_SingleFollowUp,data = Train, family = binomial(link='logit'),maxit=100)


# logistic regression - cross-validation

library(caret)

# Number of iterations
k <- 10

# Accuracy
acc <- NULL

library(dplyr)

set.seed(1236311)


# External variant of repeated stratified cross validation                  

for(i in 1:k)
{
# splitting of train data into 90% train, 10% validation 
trainIndex <- createDataPartition(Train$Deviceremovedduetoinfection1_yes, p = 0.9,
                                 list = FALSE,
                                 times = 1)
train <- Train[trainIndex, ] 
validation <- Train[-trainIndex, ]

# Fitting the model 
model_x <-glm( Deviceremovedduetoinfection1_yes ~ Age + Gender.M.0.F.1 + BMI + Group + P...Single + P...Dual + P.CRT + P...LL + D...Single + D...Dual + CRT.D + D...SQ.ICD + Sodium + Potassium + Urea + Creatinine + eGFR + Hb + WCC + Plts + Aspirin + Other.antiplatelets + Clopidogrel + Beta.blockers + Ace.Inhibitors + ARBs + Sacubitril.valsartan + MRA + Loop.diuretics + Statins + Anticoagulant + Amiodarone + Immunosuppressants + Charlson.score.TOTAL + TOTAL....Fried.Score + EQ5D5LIndex + Antibiotics + New + Upgrade + Number.of.Box.Changes.in.Study.Period + Number.of.Upgrades...Downgrade.Device.Implant.Procedures + Moderate_LV + Severe_LV + Mild_LV + PPM_DualFollowUp + PPM_LeadlessFollowUp + PPM_SingleFollowUp + CRT_PFollowUp + MicraImplantFollowUp + Normal_LV + CRT_DFollowUp + ICD_DualFollowUp + ICD_SingleFollowUp,data = train, family = binomial(link='logit'),maxit=100)

# Predicting results in validation data 
probabilities <- model %>% predict(validation, type = "response")

#predict the class of individuals/categorizes individuals into two groups based on their predicted probabilities
results <- ifelse(probabilities > 0.5,1,0)
class(results)

# Actual class 
answers <- validation$Deviceremovedduetoinfection1_yes

# Calculation of accuracy
misClasificError <- mean(answers != results)

#Obtaining accuracy 
acc[i] <- 1-misClasificError
}

# Average accuracy during cross-validation
mean(acc)

#Model information 

summary(model)$coef
summary(model)

## Confidence intervals 

confint(model) # to get the confidence intervals


# Creating coefficient tables 

library(broom)
table_model <- tidy(model, exp=T, conf.int=T) # table with odds ratio 
View(table_model)

table_model_2 <- tidy(model, exp= F, conf.int=T) # table with coefficients
View(table_model_2)

write.csv(table_model_2, file= "complete_table_logistic_regression.csv") # saving complete coefficient table

table_model3 <- table_model%>% 
arrange(desc(abs(estimate))) #arranging odds ratio by absolute value


table_model3 <- table_model3 %>% filter(table_model3$p.value <=0.05) # filtering/removing non statistically significant results 

View(table_model3)

table_model4 <- table_model_2%>% 
arrange(desc(abs(estimate))) #arranging by absolute coefficient value

table_model4 <- table_model4 %>% filter(table_model4$p.value <=0.05) #filtering by statistical significance 

View(table_model4)

write.csv(table_model4, file= "table_model_coef.csv") #saving final coefficients table


# Making test predictions/predict the probabilities of extraction due to infection

probabilities <- model %>% predict(Test, type = "response")
probabilities


# predict the class of individuals/The following R code categorizes individuals into two groups based on their predicted probabilities

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
head(predicted.classes)
table(predicted.classes)

# Assessing model accuracy 

mean(predicted.classes == Test$Deviceremovedduetoinfection1_yes)

#predicted.classes

# Confusion matrix for evaluation of performance 

confusionMatrix(as.factor(predicted.classes), as.factor(Test$Deviceremovedduetoinfection1_yes))

# Precision recall curve for logistic regression

library("PRROC")

fg <- predicted.classes[Test[,55] == 1]
bg <- predicted.classes[Test[,55] == 0]

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)


windows(17,8)
plot(pr, cex.main = 2)  

# Lasso penalised logistic regression 

Comfort_Q20$Deviceremovedduetoinfection1_yes <- as.factor(Comfort_Q20$Deviceremovedduetoinfection1_yes) ##converting to a factor so that implement as a classification problem

# Train test split

set.seed(903)

trainIndex <- createDataPartition(Comfort_Q20$Deviceremovedduetoinfection1_yes, p = .7, 
                               list = FALSE, 
                               times = 1)
head(trainIndex)

Train <- Comfort_Q20[ trainIndex,]
Test  <- Comfort_Q20[-trainIndex,]

table(Test$Deviceremovedduetoinfection1_yes)
table(Train$Deviceremovedduetoinfection1_yes)


x <- model.matrix(Deviceremovedduetoinfection1_yes~., Train)[,-1]  # create model matrix

#View(x)

y <- Train$Deviceremovedduetoinfection1_yes # setting outcome variable 


# Computing lambda value

library(glmnet)

set.seed(42844) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")  # k-fold cross validation on train split to get optimal lambda value 

plot(cv.lasso)

cv.lasso$lambda.min # accessing value of lambda that mimimises the cross-validation error 

coef(cv.lasso, cv.lasso$lambda.min)
# Fitting the model 

set.seed(13678)

model2 <- glmnet(x, y, alpha = 1, family = "binomial",
              lambda = cv.lasso$lambda.min)

# Cross-validation for lasso penalised logistic regression

# External variant of repeated stratified cross-validation

#install.packages("plyr")

library(caret)

# Number of iterations 
k <- 10

# Accuracy
acc <- NULL

library(dplyr)

set.seed(1238)

for(i in 1:k)
{
## splitting of train data into 90% train, 10% validation 
trainIndex <- createDataPartition(Train$Deviceremovedduetoinfection1_yes, p = 0.9,
                                 list = FALSE,
                                 times = 1)
train <- Train[trainIndex, ]
validation <- Train[-trainIndex, ]

x <- model.matrix(Deviceremovedduetoinfection1_yes~., train)[,-1]


y <- train$Deviceremovedduetoinfection1_yes

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Fitting the model  
model2_x <- glmnet(x, y, alpha = 1, family = "binomial",
                  lambda = cv.lasso$lambda.min)

# Predicting results in validation data 
probabilities <- model %>% predict(validation, type = "response")

#predict the class of individuals/categorizes individuals into two groups based on their predicted probabilities
results <- ifelse(probabilities > 0.5,1,0)
class(results)

# Actual class 
answers <- validation$Deviceremovedduetoinfection1_yes

# Calculation of accuracy 
misClasificError <- mean(answers != results)

# Obtaining accuracy
acc[i] <- 1-misClasificError
}

# Average accuracy of the model
mean(acc)

# Creating table 

table_coef <- tidy(model2, conf.int = T) # obtaining coefficient values 

#View(table_coef)

table_coef2 <- table_coef[c(1,3)]

#View(table_coef2)

# Changing the names of the columns for better presentation 

names(table_coef2)[1] <- "Feature"
names(table_coef2)[2] <- "Coefficient Value"

table_coef3 <- table_coef2 %>% 
arrange(desc(abs(`Coefficient Value`))) #arrange by absolute coefficient value 

View(table_coef3)

write.csv(table_coef3, "table_coef_lassoz.csv")  # saving so can implement in results section 


# Prediction on test data 

x.test <- model.matrix(Deviceremovedduetoinfection1_yes ~., Test)[,-1]
probabilities <- model2 %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)


# Confusion matrix for evaluation of model performance  

confusionMatrix(as.factor(predicted.classes), as.factor(Test$Deviceremovedduetoinfection1_yes))

# PR curve for lasso penalised logistic regression 

library("PRROC")

fg <- predicted.classes[Test[,55] == 1]
bg <- predicted.classes[Test[,55] == 0]

pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr, cex.main = 2)


# Barplot of model accuracies (finish when collate final plots, results etc)

rm(list=ls())

library(ggplot2)

Accuracy_values <- c( 99.3, 99.1, 94.7, 94.5, 93.5,95.7) # vector of accuracy values 

ML_methods <- c("RF","SVM RBF","SVM Linear","SVM Linear with C parameter tuning", "Logistic regression", "Lasso penalised logistic regression") # vector of ML methods 

ML_methods<- as.data.frame(ML_methods) # vector to dataframe 

Accuracy_values <- as.data.frame(Accuracy_values) # vector dataframe 

ML_ACC <- cbind(ML_methods, Accuracy_values) # combining dataframes 

#View(ML_ACC)

windows(17,8)

#Barplot 

ggplot(ML_ACC, mapping = aes(x =ML_methods, y= Accuracy_values, fill=ML_methods)) +geom_bar(stat = "identity")  + scale_fill_manual(values = c("red", "blue", "green","orange", "pink", "black")) + geom_text(aes(label = Accuracy_values), vjust = -0.2, size = 4,
                                                                                                                                                                                                           position = position_dodge(0.9)) + labs(x = "Machine Learning Method", y="Accuracy Value (3.s.f)") + theme(axis.text.x =element_blank(), axis.title.x = element_blank()) + guides(fill=guide_legend(title="Machine Learning Method"))
# Barplots of the RF model and associated risk score/refined models

library(ggplot2)

Accuracy_values <- c( 99.3, 96.6)

ML_methods <- c("RF","RF - 10 Features")


ML_methods<- as.data.frame(ML_methods)

Accuracy_values <- as.data.frame(Accuracy_values)

ML_ACC <- cbind(ML_methods, Accuracy_values)

#View(ML_ACC)

windows(17,8)

library(ggplot2)
ggplot(ML_ACC, mapping = aes(x =ML_methods, y= Accuracy_values, fill=ML_methods)) +
geom_bar(stat = "identity")  + 
scale_fill_manual(values = c("red", "blue", "green")) +
geom_text(aes(label = Accuracy_values), vjust = -0.2, size = 6,
         position = position_dodge(0.9)) + 
labs(x = "Machine Learning Method", y="Accuracy Value (3.s.f)") + 
theme(axis.text.x =element_blank(), axis.title.x = element_blank(),
     legend.key.height= unit(2, 'cm'),legend.key.width= unit(2, 'cm'),
     legend.title = element_text(size=15),
     legend.text = element_text(size=15),
     axis.title.y = element_text(size=15))+ guides(fill=guide_legend(title="Machine Learning Method")) 







                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               
                                                                                                                                                               