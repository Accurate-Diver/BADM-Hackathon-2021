setwd("~/Desktop/Hackathon 2021")


source('BCA_functions_source_file.R')

fsa  <- read.csv("FSA_DATASET.CSV")
survey <- read.csv("SURVEY_DATASET.csv")
usg    <- read.csv("USAGE_DATASET.csv")

usg$power_us <- as.factor(usg$power_us)


names(usg)[names(usg) == "clinic_fsa"] <- "fsa"


view(fsa)
view(survey)
view(usg)


table1 <- merge(usg,survey,by="pt_id",all.y=TRUE) 
newdata <- merge(table1,fsa,by='fsa',all.x=TRUE)




view(newdata)

newdata$bmi_class <- fct_explicit_na(newdata$bmi_class, # Factor of interest
                                     na_level = "NR") # replacement value

newdata$perc_weight <- fct_explicit_na(newdata$perc_weight, # Factor of interest
                                       na_level = "NR") # replacement value


variable.summary(newdata)

#Interactions
newdata$incomexage <- newdata$median_age_fsa*newdata$median_income_fsa

r.train <- filter(newdata,Sample == "Estimation")
r.test <- filter(newdata,Sample == "Validation") 
r.holdout <- filter(newdata,Sample == "Holdout" )

paste(names(newdata), collapse = " + ")




variable.summary(newdata)


model1.logreg <- glm(power_us ~ fsa + income + age + edu + perc_health + perc_weight + bmi_class + arthritis + 
                       highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator  + 
                       perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending  + 
                       pop_fsa + median_age_fsa + hhold_fsa + median_income_fsa + hhold_work_health + 
                       avg_spend_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco,
                     data=r.train,family=binomial(logit)) 

summary(model1.logreg)


#Interactions

model1.logreg.interact <- glm(power_us ~ fsa + income + age + edu + perc_health + perc_weight + bmi_class + arthritis + 
                                highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator  + 
                                perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending  + 
                                pop_fsa + median_age_fsa + hhold_fsa + median_income_fsa + hhold_work_health + 
                                avg_spend_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco+incomexage,
                              data = r.train, family = binomial(logit))

summary(model1.logreg.interact)



model2.step <- step(model1.logreg,direction="both")

summary(model2.step)



model1.logreg2 <- glm(power_us ~ age +  edu + perc_health + perc_weight + physactivityindicator + 
                        othercare,
                     data=r.train,family=binomial(logit)) 

summary(model1.logreg2)





model3.rpart <- rpart(power_us ~ fsa + income + age + edu + perc_health + perc_weight + bmi_class + arthritis + 
                        highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator  + 
                        perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending  + 
                        pop_fsa + median_age_fsa + hhold_fsa + median_income_fsa + hhold_work_health + 
                        avg_spend_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco,
                      data=r.train, cp = 0.01, model = TRUE)

plotcp(model3.rpart)
printcp(model3.rpart) 

rpart.plot(model3.rpart, type = 0, fallen.leaves = TRUE, uniform = TRUE, yes.text = "TRUE", no.text = "FALSE", cex = .8)





model4.RF<-randomForest(power_us ~ income + age + edu + perc_health + perc_weight + bmi_class + arthritis + 
                          highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator  + 
                          perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending  + 
                          pop_fsa + median_age_fsa + hhold_fsa + median_income_fsa + hhold_work_health + 
                          avg_spend_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco, 
                        data=r.train, mtry=sqrt(23), importance = TRUE) 

model4.RF importance(model4.RF,type = 2) 
varImpPlot(model4.RF,type = 2, main = "Importance Plot")



model5.Neunet <- Nnet(formula = power_us ~ fsa + income + age + edu + perc_health + perc_weight + bmi_class + arthritis + 
                        highBP + diabetes + stroke + repstrain + injstatus + physactivityindicator  + 
                        perc_mentalHealth + perc_lifstress + perc_workstress + care_language + othercare + spending  + 
                        pop_fsa + median_age_fsa + hhold_fsa + median_income_fsa + hhold_work_health + 
                        avg_spend_health + avg_dcost + avg_insur_prem + tot_spend_toba_alco,
                         data = r.train, decay = 0.10, size = 2)

model5.Neunet$value 
summary(model5.Neunet)



model5.Neunet2 <- Nnet(formula = power_us ~ age + edu+ perc_health + perc_weight + physactivityindicator + 
                        othercare,
                      data = r.train, decay = 0.10, size = 2)

model5.Neunet2$value 
summary(model5.Neunet2)







#Estimation - Cumulative 
lift.chart(modelList = c("model1.logreg","model2.step",'model3.rpart','model5.Neunet','model5.Neunet2','model4.RF'),
           data = filter(newdata, Sample == "Estimation"),
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Estimation")

#Validation - Cumulative

lift.chart(modelList = c("model1.logreg2","model2.step",'model5.Neunet','model5.Neunet2'),
           data = r.test,
           targLevel = "1", 
           trueResp = 0.22,
           type = "cumulative", sub = "Validation")



#Incremental 
lift.chart(modelList = c("model2.step"), 
           data = r.test, targLevel = "1", 
           trueResp = 0.22, type = "incremental", # "incremental" instead of "cumulative" sub = "Validation")


           
           plot(allEffects(model5.Neunet2),type = "response")
           
           ##Scoring Prediction
           newdata$power.model1.logreg2 <- rawProbScore(model = "model1.logreg2",
                                                        data = newdata,
                                                        targLevel = "1") 
           
           newdata$power.model1.logreg2
           
           Submission.model2.step <- newdata[newdata$Sample == "Holdout",c("pt_id","power.model1.logreg2")]
           
           
           names(Submission.model2.step) <- c("pt_id", "score")
           
           write.csv(Submission.model2.step,"Submission.model2.step_default.csv")
