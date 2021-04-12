#################################################################################
#Project: Homework Assignment for Interviewees
#Author:Andre
#Date Created: 4/09/21
#Date Last Modified: 4/11/21
#Description: The code implements approach to use mecication and procedure
#data to predict hypertension dx for a given patient at a given date. The two
#outputs are 
# 1) A function which takes the Patient ID and Date and returns a predicted dx
# of hypertension, and
#2) A list of the top most predictive variables in the model
##################################################################################


###Load required libraries
library(readxl)
library(fastDummies)
library(lubridate)
library(dplyr)
library(sqldf)
library(randomForest)
library(caret)
library(gbm)
library(e1071)
library(pROC)
library(RWeka)
library(caTools)
library(ranger)

###Read in data sets
###Please note: for this code to work you would have to change the file locations
###to the exact location on your computer

conditions <- read_excel("C:/Users/AND23582/Desktop/conditions.xlsx")

medications <- read_excel("C:/Users/AND23582/Desktop/medications.xlsx")

procedures <- read_excel("C:/Users/AND23582/Desktop/procedures.xlsx")


###Sort datasets by Patient id and date
conditions.2 <- with(conditions, conditions[order(PATIENT, START),])

medication.2 <- with(medications, medications[order(PATIENT,START),])

procedures.2 <- with(procedures, procedures[order(PATIENT, DATE),])

###Create list of patient ids present in both the medication and procedures table
id.req <-   unique(intersect(unique(medication.2$PATIENT), unique(procedures.2$PATIENT)))

conditions.3 <- with(conditions.2, conditions.2[PATIENT %in% id.req,])

procedures.3 <- with(procedures.2, procedures.2[PATIENT %in% id.req,])

medication.3 <- with(medication.2, medication.2[PATIENT %in% id.req,])

###Remove datasets that will no longer be used
rm(conditions);rm(conditions.2); rm(medications); rm(medication.2); rm(procedures); rm(procedures.2)


###Create hypertension variable
conditions.3$HYPERTENSION <-  ifelse(conditions.3$CODE == 38341003,1,0)

###Because hypertension is a chronic disease, once a patient is dx with it they will always have it.
###As a result for every patient who has hypertension, for every date after their dx date the HYPERTENSION
###variable must be set to 1
for(i in unique(conditions.3$PATIENT)){
  print(i)
  temp.table <- conditions.3[conditions.3$PATIENT== i,]
  if (dim(temp.table)[1]>1){
    indicate <- 0
    for(j in 1:dim(temp.table)[1]){
      if(temp.table$HYPERTENSION[j]==1|indicate == 1){
        indicate <- 1
        temp.table$HYPERTENSION[j] <- 1
      }      
    }
    conditions.3$HYPERTENSION[conditions.3$PATIENT== i] <- temp.table$HYPERTENSION
  }
  
}


###Create final hypertension dx table
conditions.3$DATE <- ymd(conditions.3$START) 

condition.hypertension <- conditions.3[,c(8,3,7)]

###Remove data sets no longer being used
rm(conditions.3)

###Save the important datasets for future use.
###Again, change the location to where you want the files stored on you computer
save.image("C:/Users/AND23582/Desktop/Condition_done.RData")

###Further process the medication variable
###Remove columns STOP, Encounter, reasoncode and reason description
medication.4 <- medication.3[,c(1,3,5,6)]


###Convert date time into date
medication.4$START <- ymd(medication.4$START)

###Data cleanup
rm(medication.3)

###Create variable that finds each unique CODE and DESCRIPTION paring-----this was not so this line should be deleted
rxnorm.code.check <- sort(unique(with(medication.4, paste( as.character(CODE),DESCRIPTION,sep = ""))))

###For the 98 unique rxnorm codes create a dummy matrix of 98 columns---each column now represents a medication variable
meds.mat <- dummy_cols(medication.4$CODE)

### Join meds table(with dummy variables) to medication table
medication.final <- cbind(medication.4[,c(1,2)],meds.mat[,-1])

###Data clean up
rm(medication.4); rm(meds.mat)


###Further process the procedures table
###Remove variables that will no longer be used
procedures.4 <- procedures.3[,c(1,2,4,5)]

###Data cleanup
rm(procedures.3)

###Convert Date Time variable to Date format  
procedures.4$DATE <- ymd(procedures.4$DATE)

###For the 98 unique SNOMED CT codes create a dummy matrix---each column now represents a procedure variable
proc.mat <- dummy_cols(procedures.4$CODE)

### Join proc.mat(with dummy variables) to the procedures table
procedure.final <- cbind(procedures.4[,c(1,2)],proc.mat[,-1])

###Data cleanup
rm(procedures.4);rm(proc.mat)


###Now using the condition table for each person who has a hypertension dx create a table which stores the patient id and the date of 
###First Dx

condition.hypertension$first.occur <-  condition.hypertension$HYPERTENSION

for(i in unique(condition.hypertension$PATIENT)){
  ###print(i)
  temp.table <- condition.hypertension[condition.hypertension$PATIENT== i,]
  if (dim(temp.table)[1]>1){
    first.occ <- numeric(length=dim(temp.table)[1])

    for(j in 1:dim(temp.table)[1]){
      if(temp.table$HYPERTENSION[j]==1){
        first.occ[j] <- 1
        break
          
      }      
    }
    condition.hypertension$first.occur[condition.hypertension$PATIENT== i] <- first.occ
  }
}

### subset the condition.hypertension table where the first.occ =1

first.dx <- condition.hypertension[condition.hypertension$first.occur==1, c(1,2)]

###Now use the previously create table to add the Hypertension Dx to the medication and procedure tables

medication.final$HYPERTENSION <- 0
procedure.final$HYPERTENSION <- 0
medication.final$HYPERTENSION[medication.final$PATIENT %in% unique(first.dx$PATIENT) ] <- 1
procedure.final$HYPERTENSION[procedure.final$PATIENT %in% unique(first.dx$PATIENT) ] <- 1

for(i in unique(first.dx$PATIENT)){
  medication.final$HYPERTENSION[medication.final$PATIENT==i] <- ifelse(medication.final$START[medication.final$PATIENT==i] 
                                                                        > first.dx$DATE[first.dx$PATIENT==i],1,0)
  
  procedure.final$HYPERTENSION[procedure.final$PATIENT==i] <- ifelse(procedure.final$DATE[procedure.final$PATIENT==i] 
                                                                       > first.dx$DATE[first.dx$PATIENT==i],1,0)
}

###Save all created datasets
###Again change the location to where you want this file stored on your data
save.image("C:/Users/AND23582/Desktop/DataAssignment2.R.RData")


### Now merge modified procedure and medication tables to being creating analytic dataset
medication_final <- data.frame(medication.final)
medication_final$DATEDIFF <- as.numeric(as.Date("2021-04-10") - as.Date(medication_final$START))
procedure_final <- data.frame(procedure.final)
procedure_final$DATEDIFF <- as.numeric(as.Date("2021-04-10") -  as.Date(procedure_final$DATE))

final.data <-  sqldf("SELECT *, ABS(a.datediff - b.datediff) as Datediff  FROM  medication_final a
      INNER JOIN procedure_final b
      on a.PATIENT = b.PATIENT
      AND ABS(a.DATEDIFF - b.DATEDIFF) < 30")

###Remove all unnecessary columns
final.data.analysis <- final.data[,-c(1,2,101,102,103,104,184,185)]

###Remove all columns that have only zeros
any.ones <- numeric()
for(i in 1:(dim(final.data.analysis)[2]-1)){
  any.ones[i] <- sum(final.data.analysis[,i]==1) >= 1
}

final.data.analysis.2 <- final.data.analysis[,as.logical(any.ones)]


###Fit univariate logistic regressions and retain only variables where the p value is < 0.1
prelim.p <- numeric()
for(i in 1:(dim(final.data.analysis.2)[2]-1)){
prelim.p[i] <- coef(summary(glm(final.data.analysis.2$HYPERTENSION ~ final.data.analysis.2[,i],family = "binomial")))[2,4]
}

final.data.analysis.3 <- final.data.analysis.2[,c(prelim.p < 0.10,TRUE)]


###For some of the RxNorm variables there was some redundancy so for variables Where the Rxnrom
###codes referred to the same medication I simmply combined the two columns into 1.  I can provide
###more specifics if requested
###I also removed some SNOMED CT variables that reffered to uncommon unecessary procedures. I can provide the 
###procedures remove if requsted.
final.data.analysis.4 <- final.data.analysis.3[,-c(76,78,73,77,66,63,47,79,58)]

final.data.analysis.4[,32]  <- as.numeric(final.data.analysis.4[,33] | final.data.analysis.4[,32])

final.data.analysis.4[,17]  <- as.numeric(final.data.analysis.4[,17] | final.data.analysis.4[,25])

final.data.analysis.4[,5]  <- as.numeric(final.data.analysis.4[,5] | final.data.analysis.4[,23])


final.data.analysis.4[,27]  <- as.numeric(final.data.analysis.4[,27] | final.data.analysis.4[,28]|final.data.analysis.4[,29])
                                          
final.data.analysis.4[,21]  <- as.numeric(final.data.analysis.4[,21] | final.data.analysis.4[,22]|final.data.analysis.4[,24])

final.data.analysis.5 <- final.data.analysis.4[,-c(33,25,23,28,29,22,24)]                                           

###I refit all the univariate logistic regression models just to verify pvalues
prelim.p2 <- numeric()
for(i in 1:(dim(final.data.analysis.5)[2]-1)){
  prelim.p2[i] <- coef(summary(glm(final.data.analysis.5$HYPERTENSION ~ final.data.analysis.5[,i],family = "binomial")))[2,4]
}

###Clean up analytic data set
###Convert hypertension variable to a factor and reformat predictor variable names

final.data.analysis.5[,"HYPERTENSION"] <- as.factor(final.data.analysis.5[,"HYPERTENSION"])
colnames(final.data.analysis.5)[1:64] <- paste("var_",substring(colnames(final.data.analysis.5),7)[1:64],sep="")

###Split data into train and test using 90/10 ratio
set.seed(1)
split.ndx <- createDataPartition(final.data.analysis.5[,"HYPERTENSION"],p=.90, list = FALSE, times = 1)
train.df <- final.data.analysis.5[split.ndx,]
test.df <- final.data.analysis.5[-split.ndx,]

train.df[,"HYPERTENSION"] <- as.factor(train.df[,"HYPERTENSION"])


###Three candidate models were fit to the data. A logistic regression, bagged classifcation, and Boosted Logistic regression

###First model----Logistic regression
fit.glm <- train(HYPERTENSION~., data = train.df, method = "glm",metric=metric)

predictions.glm <- predict(object = fit.glm,test.df[,1:64],type="raw")

print(postResample(pred=predictions.glm,obs = as.factor(test.df[,"HYPERTENSION"])))


###Second model------Bagged classification
fit.rpart <- train(HYPERTENSION~., data = train.df, method = "treebag",metric=metric)

predictions.rpart <- predict(object = fit.rpart,test.df[,1:64],type="raw")

print(postResample(pred=predictions.rpart,obs = as.factor(test.df[,"HYPERTENSION"])))

###Third model-----Boosted Logistic Regresion Model
fit.logitboost <- train(HYPERTENSION~., data = train.df, method = "LogitBoost",metric=metric)

predictions.logitboost <- predict(object = fit.logitboost,test.df[,1:64],type="raw")

print(postResample(pred=predictions.logitboost,obs = as.factor(test.df[,"HYPERTENSION"])))


###The model with the highest accuracy on the test data was the Bagged classification method

varImp(fit.rpart)

###The topmost predictive medications and procedures are:
###paclitaxel
###cisplatin
###fluoroscopy
###injection of adrenaline
###clopidogrel
###epinephrine
###Asthma Screening
###subcutaneous immunotherapy
###allergy screening test
###spirometry
###norethindrone
###nitroglycerin
###amlodipine
###simvastatin
###percutaneous coronary intervention
###liraglutide
###Coronary bypass graft
###Insulin
###Bone density scan
###alteplase



###Create the function that takes Pateint ID and Date and returns predictied dx of hypertension

###Create final function for the pipeline

predict.patient.status <- function(Pat.id, Dte){
  
  place <- (1:dim(final.data)[1])[final.data$PATIENT == Pat.id & final.data$START == Dte]
  
  if (length(place) > 1)
    place <- place[1]
  
 
  a <- predict(object = fit.rpart,final.data.analysis.5[place,1:64],type="raw")
 
  return(a)
}


###Some test runs of the function---to verify correctness
predict.patient.status("025fff05-23d7-43c4-9017-2f89cb19caec","2016-07-18")

predict.patient.status("5eac5c75-419d-48ae-b6c7-8edb1f496808","2011-01-10")


