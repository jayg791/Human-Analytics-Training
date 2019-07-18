# Human Resources Analytics in R: Exploring Employee Churn
# there is 10 good years of data 2006 - 2015
# column STATUS is either 'active' or 'terminated', 
# this will be our dependant variable

# load the data
hrData <- read.csv("C:/Users/EliteBook/Documents/Human Resource Data/MFG10YearTerminationData.csv")

# look at the structure of the data
str(hrData)

# install libraries to conduct analysis
install.packages('plyr')
install.packages('dplyr')
install.packages("magrittr")
install.packages("Rcpp")
library(plyr)
library(magrittr)
library(Rcpp)
library(dplyr)


# look at a summary of the data
summary(hrData)

# a cursory look at the data above does not have anthing jump out
# as having data quality issues

# What proportion of our staff are leaving?
StatusCount<- as.data.frame.matrix(hrData %>%
                                     group_by(STATUS_YEAR) %>%
                                     select(STATUS) %>%
                                     table())
StatusCount$TOTAL<-StatusCount$ACTIVE + StatusCount$TERMINATED
StatusCount$PercentTerminated <-StatusCount$TERMINATED/(StatusCount$TOTAL)*100
StatusCount

# Where are our terminations happening?

# lets look by business us
library(ggplot2)

ggplot() + geom_bar(aes(y = ..count..,x = as.factor(BUSINESS_UNIT),fill = as.factor(STATUS)),data=hrData, position = position_stack())

# it looks like the terminations of the last 10 years have been occuring in the Stores
# lets turn our focus to just terminates
# just terminates by Termination Type and Status Year

TerminatesData<- as.data.frame(hrData %>%
                                 filter(STATUS=="TERMINATED"))

ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termtype_desc)),data=TerminatesData,position = position_stack())

# it appears that year-on-year most terminations are voluntary apart 
# from 2014 and 2015 where there wasan increase in involuntary

# termination by Status Year and Termination Reason
ggplot() + geom_bar(aes(y = ..count..,x= as.factor(STATUS_YEAR), fill = as.factor(termreason_desc)), data = TerminatesData, position = position_stack())

# it appears there was layoffs on 2014 and 2015 which accounsts for involunatry terminates

# termination by Termination Reason and Department
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(department_name),fill = as.factor(termreason_desc)),data=TerminatesData,position = position_stack())+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# from the graph we can see that resignation is high in customer service and retirement is high overall

# How does Age and Length of Service affect termination

install.packages('caret')
install.packages('devtools')
library(devtools)






