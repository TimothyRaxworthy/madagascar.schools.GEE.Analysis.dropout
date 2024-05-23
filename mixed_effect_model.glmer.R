#Packages

library(lme4)       # linear mixed-effects models
library(lmerTest) 
library(sjPlot)
library(merDeriv)
library(parameters)
library(knitr)
library(kableExtra)
library(dplyr)
library(pROC)
library(broom)
library(broom.mixed)
library(lmtest)
library(glmmTMB)


data <- read.csv("/Users/timra/Downloads/Data_for_model.csv")
summary(data)



#create binary variable for if student repeated year or not
data$repeat_binary<-data$repeatyear
data$repeat_binary[data$repeat_binary==2]<-1
data$repeat_binary[data$repeat_binary==3]<-1
data$repeat_binary[data$repeat_binary==4]<-1
data$repeat_binary[data$repeat_binary==5]<-1
data$repeat_binary[data$repeat_binary==6]<-1

#drop ages that have <50 cases
data_model<-data%>%
  dplyr::filter(age > 7 & age < 16) %>%
  dplyr::mutate(age = age - 8)

range(data_model$age)



## Fix the variable to a character


#create character variable for gender
data_model$gender_chr<- as.character(data_model$gender)
data_model$gender_chr[data_model$gender_chr == "0"] <- "M"
data_model$gender_chr[data_model$gender_chr == "1"] <- "F"


## Running Same model as marginal 


mixed_effect <- glmer(dropout ~ age + gender + repeatyear +repeatyear*prog_char 
                      + prog_char+ (1| school), family=binomial(link="logit"), 
                      nAGQ = 10, data = data_model)

summary(mixed_effect)


## First Model: only include random intercepts for school and age and gender as 
##fixed effects

#model to compare to
model_1 <- glmer(dropout ~ age + gender + (1|school), 
                 data = data_model, nAGQ= 10, family = binomial(link = "logit"))

summary(model_1)



lrtest(model_1,mixed_effect)

#Raw between school variance
school_variance <- glmer(dropout ~ 1 + (1 | school), 
                         data=data_model, nAGQ = 10, 
                         family=binomial(link = "logit"))

summary(school_variance)


#Comparison between nested model and full model AIC & BIC
AIC_2 <- c(AIC(model_1), AIC(mixed_effect))
BIC_2 <- c(BIC(model_1), BIC(mixed_effect))

tablea <- data.frame("Model" = c("Nested", "Full"), "AIC" = AIC_2, 
                     "BIC" = BIC_2)

kable(tablea,
      caption = "AIC and BIC Values for nested and full model",
      col.names = c("Model", "AIC", "BIC"),
      booktabs = T, digits=3) %>%
  kable_styling(latex_options = "hold_position")




## Table for this model:


model_parameters <- tidy(mixed_effect, conf.int = TRUE)

# First, check the structure of 'model_parameters'
str(model_parameters)

# Next, try creating a basic kable without styling to ensure this works
basic_table <- kable(model_parameters, format = "html", booktabs = TRUE)

# If the basic table works, then try adding styling
styled_table <- basic_table %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
print(styled_table)



plot_model(mixed_effect, type = "re")
plot_model(mixed_effect, type = "diag")


library(arm)

# Calculate the fitted values and residuals
fitted_vals <- predict(mixed_effect, type="response")
residuals <- resid(mixed_effect,type="response")

# Create the binned residual plot
binnedplot(fitted_vals, residuals)

data_model$phat <- predict(mixed_effect, type = "response")
data_model <- na.omit(data_model)

#roc curve
roc(data_model$dropout, as.numeric(data_model$phat), 
    plot = TRUE, auc.print = TRUE)

