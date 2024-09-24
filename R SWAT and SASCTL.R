##############################################################
#                                                            #
#                     load dependencies                      #
#                                                            #
##############################################################
library(swat)
library(sasctl)
library(ggplot2)
library(caret)
library(pROC)
library(dplyr)
library(gridExtra)

setwd('<my-dir>')

##############################################################
#                                                            #
#                    download dataset                        #
#                                                            #
##############################################################
hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv")
hmeq$row_id <- 1:nrow(hmeq)

##############################################################
#                                                            #
#                     exploratory analysis                   #
#                                                            #
##############################################################

summary(hmeq)

ggplot(hmeq, aes(x = REASON, fill = factor(BAD))) +
  geom_histogram(binwidth = 5, position = "dodge",stat="count") + 
  labs(title = "Distribution of Application Reason by Default Status", x = "Reason", fill = "Bad")

ggplot(hmeq, aes(x = factor(BAD), y = DELINQ, fill = factor(BAD))) +
  geom_boxplot() +
  labs(title = "Boxplot of Delinquency by Default Status", x = "Bad", y = "Lines of Delinquent Credit")

ggplot(hmeq, aes(x = JOB, fill = factor(BAD))) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Applicant Job by Default Status", x = "Job", fill = "Bad")

##############################################################
#                                                            #
#                     Build local R model                    #
#                                                            #
##############################################################

## removing missing data
hmeq[hmeq == ""] <- NA 
hmeq <- na.omit(hmeq)
hmeq$BAD <- as.factor(hmeq$BAD)
hmeq$REASON <- as.factor(hmeq$REASON)
hmeq$JOB <- as.factor(hmeq$JOB)

# train/test split
trainIndex <- createDataPartition(hmeq$BAD, p = 0.7, list = FALSE)
train <- hmeq[trainIndex, ]
test <- hmeq[-trainIndex, ]

## creating logistic regression
model1 <- glm(BAD ~ . -row_id, hmeq, family = binomial("logit"))

summary(model1)

##############################################################
#                                                            #
#                    Assess Model Performance                #
#                                                            #
##############################################################

# validate performance on test dataset
test$predicted_prob <- predict(model1, newdata = test, type = "response")

# assign predicted class
test$p_bad <- ifelse(test$predicted_prob > 0.5, 1, 0)

# view ROC plot
roc_curve <- roc(test$BAD, test$predicted_prob)
plot(roc_curve, main = "ROC Curve", col = "blue",xlim=c(1,0),ylim=c(0,1))

# plot confusion matrix
conf_matrix <- confusionMatrix(as.factor(test$p_bad), as.factor(test$BAD))

conf_matrix_df <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_df) <- c("Actual", "Predicted", "Count")

ggplot(data = conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

##############################################################
#                                                            #
#                     Save to PMML                           #
#                                                            #
##############################################################

XML::saveXML(pmml::pmml(model1, model.name = "General_Regression_Model",
                        app.name = "Rattle/PMML",
                        description = "Logistic Regression Model"),
             "my_model.pmml")

convert_to_pmml42("my_model.pmml", "my_model_conv.pmml")


##############################################################
#                                                            #
#             Connect to SAS Viya using SWAT                 #
#                                                            #
##############################################################

# connect to SAS Viya
user <- rstudioapi::askForPassword('pwd')
pwd <- rstudioapi::askForPassword('pwd')
host <- "https://<my-host>/cas-shared-default-http"
conn <- swat::CAS(host,443,user,pwd)

# push data into CAS
train_hmeq <- as.casTable(conn,train,casOut=list(name="train_hmeq",replace=TRUE))
test_hmeq <- as.casTable(conn,test,casOut=list(name="train_hmeq",replace=TRUE))

# build model via CAS Actions

cas.builtins.loadActionSet(conn, actionSet="decisionTree")                       

# train gradient boosting model
cas.decisionTree.gbtreeTrain(conn,                                               # 5
                             table=train_hmeq,
                             target="BAD",
                             inputs=list("CLAGE", 
                                         "CLNO",
                                         "DEBTINC",
                                         "DELINQ",
                                         "DEROG",
                                         "LOAN",
                                         "MORTDUE",
                                         "NINQ",
                                         "VALUE",
                                         "YOJ",
                                         "JOB",
                                         "REASON"),
                             nominals=list("BAD",
                                           "JOB",
                                           "REASON"),copyVars=list("row_id","predicted_prob"),
                             saveState=list(name="gb_astore"),
                             casOut=list(name="gb_model",
                                         replace=TRUE))

# score test dataset
cas.decisionTree.dtreeScore(conn,
                            table=test_hmeq,                                       
                            assess=TRUE,
                            modelTable=list(name="gb_model"),               
                            path=FALSE, copyVars=list("row_id","predicted_prob","BAD"),                                  
                            casOut=list(name="gb_score", 
                                        replace=TRUE),                    
                            encodeName=FALSE)

# sync locally with score table
gb_score <- defCasTable(conn,caslib='casuser',table='gb_score')
gb_res <- to.casDataFrame(gb_score)
gb_out <- to.data.frame(gb_res)

head(gb_out)

##############################################################
#                                                            #
#                    Assess Model Performance                #
#                                                            #
##############################################################

# remove unnecessary columns
gb_out <- gb_out %>% select(row_id,predicted_prob,BAD,`_GBT_PredName_`,`_GBT_PredP_`)

# tidy up column names
names(gb_out) <- c("row_id","R_pred_prob","BAD","SAS_pred_class","SAS_pred_prob")
gb_out$R_pred_class <- ifelse(gb_out$R_pred_prob > 0.5, 1, 0)

head(gb_out)

# deduplicate rows
gb_out <- gb_out[!duplicated(gb_out$row_id), ]

# view ROC plot
roc_curve <- roc(gb_out$BAD, gb_out$SAS_pred_prob)
plot1 <- ggroc(roc_curve)  + ggtitle("SAS Model")

roc_curve <- roc(gb_out$BAD, gb_out$R_pred_prob)
plot2 <- ggroc(roc_curve)  + ggtitle("R Model")

grob1 <- ggplotGrob(plot1) 
grob2 <- ggplotGrob(plot2) 

grid.arrange(plot1, plot2, ncol = 2)

# plot confusion matrix
gb_out$SAS_pred_class <- as.numeric(gb_out$SAS_pred_class)
conf_matrix <- confusionMatrix(as.factor(gb_out$SAS_pred_class), as.factor(gb_out$BAD))

conf_matrix_df <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_df) <- c("Actual", "Predicted", "Count")

cm1 <- ggplot(data = conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "SAS Model", x = "Actual", y = "Predicted") +
  theme_minimal()

conf_matrix <- confusionMatrix(as.factor(gb_out$R_pred_class), as.factor(gb_out$BAD))

conf_matrix_df <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_df) <- c("Actual", "Predicted", "Count")

cm2 <- ggplot(data = conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "R Model", x = "Actual", y = "Predicted") +
  theme_minimal()

grid.arrange(cm1, cm2, ncol = 2)


##############################################################
#                                                            #
#             Upload PMML Model to SAS Viya                  #
#                                                            #
##############################################################

# create SAS Model Manager Session
mm_session <- session(hostname = host,username=user,password=pwd)

# Register model with PMML file
mod <- register_model(session=mm_session,
                      file="my_model_conv.pmml",
                      name="R PMML Model",
                      type="pmml",force_pmml_translation = FALSE,
                      project = "R Integration Demo",
                      force=TRUE
        )

# we can delete the project using R too
#delete_project(mm_session, "R Integration Demo")

##############################################################
#                                                            #
#             Upload SWAT Model to SAS Viya                  #
#                                                            #
##############################################################

# load Astore actionset
cas.builtins.loadActionSet(conn, actionSet="astore")                       

# download Astore to local file
astore_blob <- cas.astore.download(conn,rstore =  list(name = "gb_astore"))
con <- file('gb_model.astore', "wb")        
writeBin(object = jsonlite::base64_dec(astore_blob$blob$data), 
          con = con, useBytes = T)
          
close(con)

# Register SWAT model into SAS Model Manager project
mod <- register_model(session = mm_session,
                      file = "gb_model.astore",
                      name = "R SWAT Model",
                      type = "astore",
                      project = "R Integration Demo",
                      force=TRUE)

# terminate SWAT session to clear any temp tables
cas.close(conn)
