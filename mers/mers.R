library(lubridate)
library(ggplot2) 

setwd("C:/Users/workshop/Desktop/mers")
mers<-read.csv('cases.csv')
head(mers)
class(mers$onset)
mers$hospitalized[809]<-c('2015-02-20')
mers<-mers[-471,]

mers$onset2<-ymd(mers$onset)
mers$hospitalized2<-ymd(mers$hospitalized)
class(mers$onset2)
day0<-min(na.omit(mers$onset2))
mers$epi.day<-as.numeric(mers$onset2-day0)


ggplot(data=mers) +
geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidmeic day', y='Case count', title='Global count of MERS cases by date of MERS cases by date of symptom onset', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

mers$infectious.period <- mers$hospitalized2-mers$onset2
class(mers$infectious.period)
mers$infectious.period<-as.numeric(mers$infectious.period, units="days")
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period', caption="Data from: https://github.com/rambuat/MERS-Cases/blobl/gh-pages/data/cases.csv")

mers$infectious.period2<-ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infecitus period', y='Frequency', title='Distribution of calculated MERS infectious period (positive values only)', captions='Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/case.csv')

ggplot(data=mers) +
  geom_bar(aes(x=epi.day)) +
  labs(x='Epidemic Day', y='Infectious Period', title='Infectious Period over course of MERS epidemic', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/case.csv')

ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping=aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x="Epidemic Day", y="Infectious period", title="MERS infectious period (positive values only)over time", caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/case.csv")
