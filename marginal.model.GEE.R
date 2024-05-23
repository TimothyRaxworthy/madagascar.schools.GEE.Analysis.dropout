#Packages


library(tidyverse)
library(kableExtra)
library(MASS)
library(emplik)
library(geepack)
library(readr)
library(RColorBrewer)
library(parameters)
library(pROC)
library(arm)
library(readxl)
library(gee)
library(lme4)

#run the setup.R code before running the model
dta <-read.csv("Data_for_model.csv")


#restrict data for model, drop ages that have <50 cases
data_model <- dta %>%
  dplyr::filter(age > 7 & age < 16) %>%
  dplyr::mutate(age = age - 8) #make the youngest (8 years old) equal 0


#good range
range(data_model$age)


#create character variable for gender
data_model$gender_chr<- as.character(data_model$gender)
data_model$gender_chr[data_model$gender_chr == "0"] <- "M"
data_model$gender_chr[data_model$gender_chr == "1"] <- "F"
table(data_model$gender_chr)


#model with exchangeable structure
log.marg.mod.exc <- geeglm(dropout ~  gender_chr + age,id=school, 
                           binomial(link = "logit"), waves=NULL, 
                           data=data_model, corstr="exchangeable")

#model with independent structure
log.marg.mod.ind <- geeglm(dropout ~ gender_chr + age, id=school, 
                           binomial(link = "logit"), waves=NULL, 
                           data=data_model, corstr="independence")


#QIC & CIC lower for each model
QIC(log.marg.mod.exc)
QIC(log.marg.mod.ind)

QIC_v <- c(QIC(log.marg.mod.exc), QIC(log.marg.mod.ind))


table.1 <- data.frame("Correlation structure" = c("exchangeable", "independent"),
                      "QIC" = c(QIC_v[1], QIC_v[7]),
                      "CIC" = c(QIC_v[4], QIC_v[10]))
                      

kable(table.1,
      caption = "QIC and CIC for different correlation structure of candidate models", booktabs = T, align = rep('c', 1), digits=3) %>%
  kable_styling(latex_options = "hold_position")







##Table of model coefficients

#exponentiated coefficients (Odds Ratios)
model.obj <- model_parameters(log.marg.mod.exc, exponentiate = T)

#un exponentiated coefficients (Log-Odds)
library(dplyr)
kable(model_parameters(log.marg.mod.exc) %>%
        dplyr::select("Parameter", "Coefficient","SE","CI_low","CI_high","p") %>%
        dplyr::rename("P-value" ="p", "Estimate" = "Coefficient"),
      caption = "parameter estimates GEE-Logit", booktabs = T,linesep = "",
      digits=2) %>%
  kable_styling(latex_options = "HOLD_position")







#Full model w/exchangeable correlation structure
mod3 <- geeglm(dropout ~ age + gender_chr + repeatyear + 
                 prog_char + prog_char*repeatyear, id=school, 
               binomial(link = "logit"), waves=NULL, data=data_model, 
               corstr="exchangeable")

summary(mod3)
mod3


library(gee)
mod4 <- gee(dropout ~ age + gender_chr + repeatyear + prog_char + 
              prog_char*repeatyear, id=school, family = binomial(link = "logit"), 
            corstr="exchangeable", data=data_model)

summary(mod4)


#Make LaTex code for model coefficients
library(dplyr)
library(stargazer)
table <- kable(model_parameters(mod3) %>%
                 dplyr::select("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "p") %>%
                 dplyr::rename("P-value" ="p", "Estimate" = "Coefficient"),
               caption = "Parameter estimates GEE-Logit", booktabs = T,linesep = "",
               digits=2) %>%
  kable_styling(latex_options = "HOLD_position")



stargazer(mod4, type = "latex", title = "Marginal Generalized Linear Model using
          GEE",
          covariate.labels = c("Age", "Male", "School Years Repeated", 
                               "MRMW EE Program", "Rainforest Class EE Program", 
                               "MRMW*School Years Repeated", "Rainforest Class*School Years
                           Repeated", "Intercept"),
          dep.var.labels = "Log odds of a student dropping out",
          model.names = FALSE)






# Final model diagnostics
#full model
data_model$phat<-predict(mod3,type="response")
data_model$resid<-resid(mod3,type="response")

#Binned residual plot
arm::binnedplot(data_model$phat,data_model$resid)


#ROC curve
pROC::roc(data_model$dropout,as.numeric(data_model$phat), plot=TRUE, auc.print=TRUE)





#Preliminary Mixed effect model

#dropout fit by program and age with random intercept classfied by school
test_model <- glmer(data = data_model, dropout ~ prog_char + age + (1|school), 
                    family = binomial(link = "logit"), nAGQ = 10)

#dropout fit by program, teacher and random intercepts by school
size_model <- glmer(data = data_model, dropout ~ prog_char + teacher + 
                      (1|school), family = binomial(link = "logit"), nAGQ = 10)

#dropout by program with random intercepts
model1 <- glmer(data = data_model, dropout ~ prog_char + (1|school), 
                family = binomial(link = "logit"), nAGQ = 10)

summary(model1)
summary(test_model)
summary(size_model)

