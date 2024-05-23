#Packages

library(lme4)
library(ggplot2)
library(nlme)
library(kableExtra)
library(MASS)
library(emplik)
library(geepack)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(parameters)
library(pROC)
library(arm)
library(readxl)


# Load in data

mschool<- readxl::read_xlsx("/Users/timra/Desktop/Madagascar School Project/mschool_2023.11.03.xlsx", sheet = "Sheet1")

# Create data object for marginal linear model
#create variables for marginal linear model
dropout <- mschool$e3 #outcome for analysis

#smallest school is 53 students (no school has zero students)
size <- (mschool$D23)
grade <- (mschool$d21)

# number of of children in house not used
repeatyear <- mschool$e7
program <- mschool$d19
school <- sort(mschool$D24)
#youngest student is 4 years old -> no student is 0 years old
age <- (mschool$e2)
gender <- mschool$d17
classroom <- mschool$d18
teacher <- mschool$D22
#ID variable for each student (will have to change due to dropping cases)
ID <- seq(1,1469,1)




#create data frame of variables
dta <- as.data.frame(cbind(ID,dropout,size,repeatyear,program,school,age,
                           gender, classroom, teacher, grade))
#drop missing cases
dta <- dta[complete.cases(dta), ] 


#Correct program type
dta$prog_char <- as.character(dta$program)
dta$prog_char[dta$program == "2"] <- "No Program"
dta$prog_char[dta$program == "1"] <- "MRMW"
dta$prog_char[dta$program == "0"] <- "Rainforest Class"
dta$prog_char <- factor(dta$prog_char, ordered = FALSE )
dta$prog_char <- relevel(dta$prog_char, ref = "No Program")

#lost 50 cases
dta$ID <- seq(1,1419,1)








#create binary variable for if student repeated year or not
dta$repeat_binary <- dta$repeatyear
dta$repeat_binary[dta$repeat_binary == 2] <- 1
dta$repeat_binary[dta$repeat_binary == 3] <- 1
dta$repeat_binary[dta$repeat_binary == 4] <- 1
dta$repeat_binary[dta$repeat_binary == 5] <- 1
dta$repeat_binary[dta$repeat_binary == 6] <- 1

dta$repeat_chr <- as.character(dta$repeat_binary)
dta$repeat_chr[dta$repeat_chr == "0"] <- "Never Repeated"
dta$repeat_chr[dta$repeat_chr == "1"] <- "Has Repeated"
dta$repeat_chr <- factor(dta$repeat_chr, ordered = FALSE )
dta$repeat_chr <- relevel(dta$repeat_chr, ref = "Never Repeated")



write.csv(dta, file = "Data_for_model.csv")


