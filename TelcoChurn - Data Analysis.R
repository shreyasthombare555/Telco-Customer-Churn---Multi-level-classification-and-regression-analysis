rm(list=ls())

#---------------- Importing the Libraries ---------------------------------------

library(readxl)
library(dplyr)
library(stargazer)

#---------------- Import Data ---------------------------------------------------

master.dataset = read_excel("TelcoChurn.xlsx")
colnames(master.dataset) = tolower(make.names(colnames(master.dataset)))
attach(master.dataset)
summary(master.dataset)
nrow(master.dataset)
ncol(master.dataset)

#------------------ Data Preprocessing ------------------------------------------------
#------------------- Dropping rows with blank values ----------------------------------

colSums(is.na(master.dataset))
master.dataset = master.dataset[!(is.na(master.dataset$totalcharges) | master.dataset$totalcharges==""), ]
colSums(is.na(master.dataset))

#-------------------- Excluding cusotmer id which s irrelevant in our analysis -------------------

filtered_df <- subset( master.dataset, select = -c(customerid))

#-------------------- Generating new feature streamingtype which combine streammovies and streamtv --------------

filtered_df$streamingtype = ifelse(master.dataset$streamingtv == "Yes" & master.dataset$streamingmovies == "No", 'TV',
                                   ifelse(master.dataset$streamingtv == "No" & master.dataset$streamingmovies == "Yes", 'Movies',
                                          ifelse(master.dataset$streamingtv == "Yes" & master.dataset$streamingtv == "Yes", 'Both', "None")))

#--------------------- Converting Variables Into Factors -----------------------------------------

filtered_df$seniorcitizen <- factor(filtered_df$seniorcitizen , labels=c("Yes", "No"))
filtered_df$gender <- factor(filtered_df$gender)
filtered_df$partner <- factor(filtered_df$partner)
filtered_df$dependents <- factor(filtered_df$dependents)
filtered_df$phoneservice <- factor(filtered_df$phoneservice)
filtered_df$multiplelines <- factor(filtered_df$multiplelines)
filtered_df$internetservice <- factor(filtered_df$internetservice)
filtered_df$onlinesecurity <- factor(filtered_df$onlinesecurity)
filtered_df$onlinebackup <- factor(filtered_df$onlinebackup)
filtered_df$deviceprotection <- factor(filtered_df$deviceprotection)
filtered_df$deviceprotection <- relevel(filtered_df$deviceprotection, "Yes")
filtered_df$techsupport <- factor(filtered_df$techsupport)
filtered_df$streamingtype <- factor(filtered_df$streamingtype)
filtered_df$contract <- factor(filtered_df$contract)
filtered_df$paperlessbilling <- factor(filtered_df$paperlessbilling)
filtered_df$paymentmethod <- factor(filtered_df$paymentmethod)
filtered_df$churn <- factor(filtered_df$churn , labels=c("0", "1"))
levels(filtered_df$churn)

#----------------------------- Exploratory Data Analysis--------------------------------------

table(filtered_df$churn)                                      # Unbalanced sample
table(filtered_df$churn, filtered_df$phoneservice)
table(filtered_df$churn, filtered_df$internetservice)
plot(churn ~ gender, data=filtered_df)
plot(churn ~ contract, data=filtered_df)
plot(churn ~ multiplelines, data=filtered_df)

#-------------------Partitioning Data into Separate Dataframes ---------------------------------
#-------------------Based on customers subscribed for Phone Service, Internet Service and Both --------

df1_phone_service_subs = filter(filtered_df, phoneservice == "Yes", internetservice == "No") #customers opted for phone service.
df2_internet_service_subs = filter(filtered_df, phoneservice == "No", internetservice != "No") #customers opted for internet service.
df3_both = filter(filtered_df, phoneservice == "Yes" & internetservice != "No") #customers opted for both.

#-------------------spliting data into test and train -------------------------------------
set.seed(1024)
trainIndex = sample(1:nrow(df1_phone_service_subs), size=round(0.75*nrow(df1_phone_service_subs)), replace=FALSE)
train_ps <- df1_phone_service_subs[trainIndex,]
test_ps  <- df1_phone_service_subs[-trainIndex,]
dim(train_ps); dim(test_ps)

trainIndex = sample(1:nrow(df2_internet_service_subs), size=round(0.75*nrow(df2_internet_service_subs)), replace=FALSE)
train_is <- df2_internet_service_subs[trainIndex,]
test_is  <- df2_internet_service_subs[-trainIndex,]
dim(train_is); dim(test_is)

trainIndex = sample(1:nrow(df3_both), size=round(0.75*nrow(df3_both)), replace=FALSE)
train_b <- df3_both[trainIndex,]
test_b  <- df3_both[-trainIndex,]
dim(train_b); dim(test_b)

#------------------ Model Building -------------------------------------------------
#-----------------Churn Analysis for Phone Service Subscribers------------------------------------------------------

logit_ps  <- glm(churn ~ tenure + seniorcitizen + partner + multiplelines + contract + paperlessbilling 
                 + paymentmethod + totalcharges, family=binomial (link="logit"), data=train_ps)
summary(logit_ps)

test_x <- filtered_df <- subset( test_ps, select = -c(churn))
predlogit <-predict(logit_ps, newdata=test_x, type="response")
predlogit
predlogit <- ifelse(predlogit>0.3, 1, 0)

ClassificationError <- mean(predlogit != test_ps$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate
pr <- prediction(predlogit, test_ps$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)  

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

confusionMatrix(factor(predlogit), test_ps$churn, mode = "prec_recall", positive="1")

#---------------  Churn Analysis for Internet Service Subscribers ------------------------------------------

logit_is  <- glm(churn ~ tenure  + partner + onlinebackup + onlinesecurity + seniorcitizen +
                 + deviceprotection + techsupport + streamingtype+ contract + paperlessbilling + paymentmethod
                 + totalcharges, family=binomial (link="logit"), data=train_is)
summary(logit_is)
test_x <- filtered_df <- subset( test_is, select = -c(churn))
predlogit <-predict(logit_is, newdata=test_x, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)
ClassificationError <- mean(predlogit != test_is$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate
table(test_is$churn, predlogit)

pr <- prediction(predlogit, test_is$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)  

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

confusionMatrix(factor(predlogit), test_is$churn, mode = "prec_recall", positive="1")

#----------------Churn Analysis for both Phone Service and Internet Service and Subscribers-----------------------------

logit_b  <- glm(churn ~ tenure + partner + onlinebackup + onlinesecurity
                + deviceprotection + techsupport + streamingtype+ multiplelines + seniorcitizen
                + contract + paperlessbilling + paymentmethod+ totalcharges, family=binomial (link="logit"), data=train_b)
summary(logit_b)
test_x <- filtered_df <- subset( test_b, select = -c(churn))
predlogit <-predict(logit_b, newdata=test_x, type="response")
predlogit <- ifelse(predlogit>0.5, 1, 0)
ClassificationError <- mean(predlogit != test_b$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))    # Accuraty rate
table(test_b$churn, predlogit)
pr <- prediction(predlogit, test_b$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)  

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

confusionMatrix(factor(predlogit), test_b$churn, mode = "prec_recall", positive="1")

#------------------------------------Stargazer Output--------------------------
stargazer(logit_ps, logit_is, logit_b, title="Telco Customer Churn", type="text")
