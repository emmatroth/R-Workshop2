#Header

#Load packages
library(ggplot2)

#Load Data
setwd("C:/Users/workshop/Desktop/wnv")
wnv<-read.csv('wnv.csv')

#Analysis
wnv$Year<- as.factor(wnv$Year)

#SCRIPTS PORTION OF WORKSHEET
#graph of cases per state over years
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Year, y=Total, fill=State), stat='Identity') +
  labs(x='Year', y='Number of Cases', title='Number of cases per year per state') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#graph of cases per year across states
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=State, y=Total, fill=Year), stat='Identity') +
  labs(x='State', y='Number of Cases', title='Number of cases per year per state') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#Log number of cases- 1
ggplot(data=wnv) +
  geom_histogram(aes(x=Year, y=log10(Total), fill=State), stat='Identity') +
  labs(x='Year', y='log(Number of Cases)', title='Number of cases per year per state') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Log number of cases- 2
wnv$cases<- log10(wnv$Total)

ggplot(data=wnv) +
  geom_histogram(aes(x=Year, y=cases, fill=State), stat='Identity') +
  labs(x='Year', y='log(Number of Cases)', title='Number of cases per year per state') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#FUNCTIONS PORTION OF WORKSHEET
wnv$NDR<-wnv$EncephMen/wnv$Total
wnv$meanNDR<-mean(wnv$NDR)

mean<- function(x){
  s <-sum(x)
  n <- length(x)
  m <- s/n
  return(m)
}

STE <- function(a){
  sd <- sd(a)
  sq <-sqrt(length(a))
  se <- sd/sq
  return(se)
}

#Standard Deviation
sd(c())

#Mean
mean(c())





