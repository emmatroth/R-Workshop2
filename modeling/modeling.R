#Load libraries
install.packages('tidyverse')
install.packages('magrittr')
install.packages('GGally')
library('tidyverse')
library('magrittr')
library('GGally')
library('purrr')

#Task 1
setwd("C:/Users/workshop/Desktop/wrangling")
ld.prism.pop <- read_csv("wranglingresults2.csv")

#Task 2
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))

#Task 3
ld.prism.pop$log10size <- log10(ld.prism.pop$size)
ld.prism.pop$log10cases1 <- log10(ld.prism.pop$cases +1)
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","log10cases1"))
#add +1 to cases because log10 of 0 is undefined

#Task 4 and 5
set.seed(222)
myData <- ld.prism.pop %>% sample_n(100)

Myplot <- ggplot(myData)+geom_point(aes(prcp,avtemp))+
  geom_smooth(aes(prcp,avtemp),method='lm')
Myplot

#Task 6 and 7
myModel <- lm(avtemp ~ prcp, data=myData)
summary(myModel)
summary(myModel)$coefficients[2,1]
summary(myModel)$coefficients[2,4]
#The slope is  which is 0.006720231. p value = 3.19 e-06, which is less than 0.05, so it is significant.

#Task 8
ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+geom_point(aes(x=year,y=total))

#Task 9
by_state<- ld.prism.pop %>% group_by(state)

#Task 10
by_state %<>% nest
by_state

#Task 11
by_state$data[[10]]

#Task 12
linGrowth_model <- function(df){
  lm(size ~ year, data=df)
}

models <- purrr::map(by_state$data, linGrowth_model)

#Task 13
by_state %<>% mutate(model = purrr::map(data, linGrowth_model))

#Task 14
library(modelr)
by_state %<>% mutate(resids = purrr::map2(data, model, add_residuals))
by_state
#resids is a tibble

#Task 15
sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = purrr::map(resids,sum_resids))

#Task 16
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))
slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

#Task 17
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 18
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
 
