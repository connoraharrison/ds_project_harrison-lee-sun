---
  title: "Group Project Cleaning File"
author: "Connor Harrison, Dong Hoon Lee, Haorui Sun"
date: "April 8, 2019"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Purpose
For our project, we are using the "" data set from the .....Before we can perform any anlysis, we need to import and clean the data into a usable format. This will include retaining only the variables we place to use, transforming variable coding, and controlling for missing values. 


## Import Data Set
```{r}
library(haven)
library(tidyverse)
epcg17 <- read_dta("epcg17.dta")
View(epcg17)
```

## Relevant Variables
This data set is from a large, comprehensive survey. As such, it contains data on a wide variety of topicsm many of which are not relevant for our analysis. This section of code retains only the variables that we will explore 
```{r}
## filtering by age, selecting variables of interest, creating key indicators
epcg17_new <-  epcg17 %>% 
  filter(., age<36) %>% 
  # selecting age, race, gender, undergraduate loan amoutn, graduate loan amount, physical disability indicator
  select(age, racethm, ugloanr, gender, grloanr, hcapin,
         #degree information, second to fifth highest degrees, highest degree, most recent degree
         d2dg, d3dg, d4dg, d5dg, dgrdg, mrdg,
         #primary/secondary work activity, public/private status of undergrad institution, year ba received
         actcap:acttch, bapbpr, bayr,
         # reason for changing employer
         chchg:chloc, chot:chsch,
         # visa type for non-US citizen, year highest degree awarded, father and mother's education level
         ctzfor, dgryr, eddad, edmom,
         #region code for employer, detailed employer sector, 
         emrg, emsecdt, facadv:facsoc,
         # hours per week worked, job benefits, job satisfaction, labor force status, looking for work, marital status
         hrswk, jobins:jobproft, jobvac, jobsatis, lfstat, lookwk, marsta,
         # job code for last job, job code for principal job, field of study of BA degree, employer type
         n2ocmlst, n2ocprmg, nbamed, nedtp,
         # reasons for working outside field of highest degree, most important reason for working outside field
         nrchg:nrpay, nrrea,
         # reasons for not working, extent job is related to highest degree, reason worked part-time
         nwfam:nwstu, ocedrlp, pjfam:pjstu,
         # respondent location, salary, satisfaction in prinicipal job for..., spouse working indicator, year principal job started
         resploc, salary, satadv:satsoc, spowk, strtyr,
         # amount still owed from undergrad loan, amount still owed from grad loan, veteran status 
         ugower, grower, vetstat) %>% 
  # creating gender and race indicators
  mutate(., female = ifelse(gender=="F", 1,0)) %>% 
  mutate(., race_white = ifelse(racethm=="5",1,0 )) %>% 
  mutate(., race_black = ifelse(racethm=="3",1,0 )) %>% 
  mutate(., race_hispanic = ifelse(racethm=="4",1,0 )) %>%
  mutate(., race_asian = ifelse(racethm=="1",1,0 )) %>%
  mutate(., race_other = ifelse(racethm=="2" | racethm=="6" | racethm=="7", 1,0)) %>% 
  # replacing 9999998 to NA for salary variable
  mutate(., salary= ifelse(salary==9999998, NA, salary)) %>% 
  # bachelor degree indicator
  mutate(., bachelor_degree= ifelse(mrdg=="1", 1,
                                    ifelse(dgrdg=="1", 1,
                                           ifelse(d2dg=="1", 1,
                                                  ifelse(d3dg=="1", 1,
                                                         ifelse(d4dg=="1",1,
                                                                ifelse(d5dg=="1", 1, 0))))))) %>%
  # master's degree indicator
  mutate(., master_degree= ifelse(mrdg=="2", 1,
                                  ifelse(dgrdg=="2", 1,
                                         ifelse(d2dg=="2", 1,
                                                ifelse(d3dg=="2", 1,
                                                       ifelse(d4dg=="2",1,
                                                              ifelse(d5dg=="2", 1, 0))))))) %>%
  mutate(., doctorate_degree= ifelse(mrdg=="3", 1,
                                     ifelse(dgrdg=="3", 1,
                                            ifelse(d2dg=="3", 1,
                                                   ifelse(d3dg=="3", 1,
                                                          ifelse(d4dg=="3",1,
                                                                 ifelse(d5dg=="3", 1, 0))))))) %>%
  # professional degree
  mutate(., professional_degree= ifelse(mrdg=="4", 1,
                                        ifelse(dgrdg=="4", 1,
                                               ifelse(d2dg=="4", 1,
                                                      ifelse(d3dg=="4", 1,
                                                             ifelse(d4dg=="4",1,
                                                                    ifelse(d5dg=="4", 1, 0)))))))
head(epcg17_new)
table(epcg17_new$ugloanr)
### There are 687 (ugloanr==1) people who did not earn a BA but are included in the data because they have earned other graduate degrees
### We can decide whether we want to keep these in our sample or not.
```
# edmom-ugloanr
# try bar chart

```{r}
require("car")  
epcg17_new$ugloanr1 <- recode(epcg17_new$ugloanr, "c('01','02','03')='A';c('04','05','06')='B';c('07','08','09')='C';c('10','11','12')='D'")

grouped_data <- aggregate(epcg17_new, by=list(epcg17_new$ugloanr1, epcg17_new$racethm), FUN=length)
grouped_data <- subset(grouped_data, select = c(1:3))
grouped_data <- select(grouped_data, ugloanr = Group.1, racethm = Group.2, count=age)
grouped_data%>% 
  group_by(racethm) %>% 
  mutate(sum = sum(count))

ggplot(data = epcg17_new,
       aes(x = edmom, y = racethm, fill = ugloanr)) +
  geom_bar(stat = "identity") +
  theme(plot.margin = unit(c(1,1,1,1,1,1,1,1,1,1,1,1), "cm")) + 
  geom_text(aes(label= edmom),
            position = position_stack(vjust = 0.5),
            size = 3)+
  coord_flip()
```

# try other plots

```{r}
ggplot(data = epcg17_new, 
       aes(x = bapbpr, 
           y = bayr)) +
  geom_tile(aes(fill = ugloanr)) +
  scale_y_discrete(limits = c(1959, 2016))
```

#histigram 
```{r}
ggplot(data = epcg17_new, 
       aes(x = ugloanr, 
           y = bapbpr)) +
  geom_histogram() +
  scale_y_discrete(limits = c(1959, 2016))
```