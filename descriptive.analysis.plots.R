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


#compute counts by age
age_count <- dta %>%
  group_by(age) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()
print(age_count)

#compute counts by age and program
age.by.program <- dta %>%
  group_by(age, prog_char) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()
print(age.by.program, n = 42)

#compute counts by age and if repeated a year
age.by.repeatyear <- dta %>%
  group_by(age, repeat_chr) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()
print(age.by.repeatyear, n = 29)

#compute counts by age and program
dta %>%
  group_by(age, prog_char) %>%
  dplyr::summarise(count = n()) %>%
  ungroup()

#compute proportion by age and if repeated 
summary_data <- dta %>%
  group_by(age, repeat_chr) %>%
  dplyr::summarise(proportion = mean(dropout)) %>%
  ungroup()

print(summary_data,n=29)

summary_data <- summary_data[-26:-29,]
summary_data <- summary_data[-1:-7,]


#Customize table with results - Table 1
kable(summary_data,
      caption = "Proportion of a child dropping out by ever having to repeat a year of school and age",
      booktabs = T, digits=2,linesep="") %>%
  kable_styling(latex_options = "HOLD_position")


ggplot(summary_data, aes(x = age, y = proportion, group = repeat_chr, color = repeat_chr)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Proportion of children dropping out from school",
    x = "Age",
    y = "Proportion",
    color = "Repeated a year or not"
  ) +
  scale_color_brewer(palette = "Dark2")


#create character variable for gender
dta$gender_chr<- as.character(dta$gender)
dta$gender_chr[dta$gender_chr == "0"] <- "M"
dta$gender_chr[dta$gender_chr == "1"] <- "F"

#compute proportion by age and gender
summary_data2 <- dta %>%
  group_by(age, gender_chr) %>%
  dplyr::summarise(proportion = mean(dropout)) %>%
  ungroup()

#drop oldest cases with not enough data
summary_data2 <- summary_data2[-26:-30,]
summary_data2 <- summary_data2[-1:-7,]


#Customize table with results - Table 2
kable(summary_data2,
      caption = "Proportion of a child dropping out by gender and age",
      booktabs = T, digits=2,linesep="") %>%
  kable_styling(latex_options = "HOLD_position")



ggplot(summary_data2, aes(x = age, y = proportion, group = gender_chr, color = gender_chr)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Dropout generally increases with age",
    subtitle = "Males tend to have a higher dropout proportion than females",
    x = "Age",
    y = "Proportion",
    color = "Gender",
  )+
  scale_color_brewer(palette = "Set1") +
  theme_bw()

ggsave(filename = "genderplot.pdf", device = "pdf", path = "/Users/timra/Desktop/School_madagascar", width = 8, height = 5.71, units = "in", dpi = "print")


#Proportion by age and program
summary_data3 <- dta %>%
  group_by(age, prog_char) %>%
  dplyr::summarise(proportion = mean(dropout)) %>%
  ungroup()



#drop oldest cases with not enough data
summary_data3 <- summary_data3[-34:-42,]
summary_data3 <- summary_data3[-1:-9,]
#Customize table with results - Table 2


kable(summary_data3,
      caption = "Proportion of a child dropping out by program and age",
      booktabs = T, digits=2,linesep="") %>%
  kable_styling(latex_options = "HOLD_position")


ggplot(summary_data3, aes(x = age, y = proportion, group = prog_char, color = prog_char)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Proportion of drop out by age and EE program",
    x = "Age",
    y = "Proportion",
    color = "EE Program",
    subtitle = "Proportion tends to be different across EE programs",
    caption = "Only included ages 8-15 due to no children at age 16 being in the Rainforest Class EE program"
  ) +
  scale_color_brewer(palette ="Set2") +
  theme_bw()

ggsave(filename = "programplot.pdf", device = "pdf", path = "/Users/timra/Desktop/School_madagascar", width = 8, height = 5.71, units = "in", dpi = "print")


## Size of School
summary_data4 <- dta %>%
  group_by(size) %>%
  dplyr::summarise(proportion = mean(dropout)) %>%
  ungroup()

kable(summary_data4,
      caption = "Proportion of a child dropping out by size of school",
      booktabs = T, digits=2,linesep="") %>%
  kable_styling(latex_options = "HOLD_position")


ggplot(summary_data4, aes(x = size, y = proportion)) +
  geom_point(size = 3) +
  labs(
    title = "Proportion of children dropping out from school",
    x = "size",
    y = "Proportion",
  ) 

## teachers
summary_data5 <- dta %>%
  group_by(teacher) %>%
  dplyr::summarise(proportion = mean(dropout)) %>%
  ungroup()

kable(summary_data5,
      caption = "Proportion of a child dropping out by number of teachers in school",
      booktabs = T, digits=2,linesep="") %>%
  kable_styling(latex_options = "HOLD_position")

ggplot(summary_data5, aes(x = teacher, y = proportion)) +
  geom_point(size = 3) +
  labs(
    title = "Proportion of children dropping out from school",
    x = "# of teachers",
    y = "Proportion",
  ) 
