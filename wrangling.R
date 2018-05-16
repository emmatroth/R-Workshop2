#Load Libaries
install.packages('tidyverse')
install.packages('magrittr')
install.packages('dplyr')
install.packages('stringr')
install.packages('GGally')
install.packages('maptools')
install.packages('ggmap')
install.packags('maps')
library('tidyverse')
library('magrittr')
library('dplyr')
library('stringr')
library('GGally')
library('maptools')
library('ggmap')
library('maps')

#Task 1
setwd("C:/Users/workshop/Desktop/wrangling")
ld<-read_csv('lyme.csv')
pop<-read_csv('pop.csv')
prism<-read_csv('climate.csv')

#Task 2
#pop has 5 different FIPS columns while prism only has 1 FIPS column. 
#pop has all the county, state, and country names which are not necessary when you have the full FIPS. prism does not have these columns.
#pop has columns for region and division, prism does not. Unsure what these are but must not be essential.
#pop has a lot of empty cells in the table that are wasting space, making the table way bigger than necessary

#Task 3
#select gets rid of all fips except the complete 5 digit fips
pop %<>% select(fips,starts_with("pop2"))

#gather organized years to go from earliest to latest 
pop %<>% gather(starts_with('pop2'),key="str_year",value="size")

#adds column called str_year
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))

#makes year read as an integer
pop %<>% mutate(year=as.integer(year))

#removes 0 from beginning of fips
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))

#makes fips read as integer
pop %<>% mutate(fips=as.integer(fips))

#Task 4

#puts cases under str_year column and makes a column of cases
ld %<>% gather(starts_with("Cases"),key="str_year", value="cases")

#Makes column that has year (no case in front)
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))

#Makes year an integer
ld %<>% mutate(year=as.integer(year))

#Renames STNAME and CTYNAME to state and county
ld %<>% rename(state=STNAME)
ld %<>% rename(county=CTYNAME)
ld
fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st), as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))
ld %<>% select(-c(STCODE,CTYCODE,str_year))

#Task 5
ld.prism<-inner_join(ld,prism)

#Task 6
ld.prism.pop<-inner_join(ld.prism,pop)

#Task 7
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))

cases_by_state <- ld %>% ungroup %>% group_by(state) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))

averages_by_state <- ld %>% ungroup %>% group_by(year,state) %>%
  summarize(average=mean(cases)) %>% arrange(desc(average))

averages_by_county <- ld %>% ungroup %>% group_by(county) %>%
  summarize(average=mean(cases)) %>% arrange(desc(average))

averages_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize (average=mean(cases)) %>% arrange(desc(average))


#The worst year was 2009. Conneticut, Massachussetts, and New Jersey were most impacted.

#Task 8
save(ld.prism.pop,file ="ld.prism.pop.Rda")
write_csv(ld.prism.pop,"wranglingresults.csv")


