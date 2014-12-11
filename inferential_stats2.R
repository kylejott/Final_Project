##################################
# Assignment 3: Data Science Course
# Kyle Ott & Cornelius Schneider
# 14 November 2014
##################################

# Load packages
library(httr)
library(dplyr)
library(XML)
library(ggplot2)
library(stringr)
library(car)
library(devtools)
library(rsdmx)
library(stargazer)
library(knitr)
library(CausalImpact)
library(sandwich)
library(lmtest)
library(plm)

#################
# Time Series 
#################

setwd("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/")
load("clean.RData")

###creating net-of-tax

clean["net_of_tax"] <- 1-(clean$ratio/100)

clean2 <- group_by(clean, year)
clean <- mutate(clean2, avg_inc=mean(total_inc))
summary(clean$avg_inc)

# Create Year Dummies
clean <- within(clean, yr2009<-ifelse(year==2009, 1, 0))
clean <- within(clean, yr2010<-ifelse(year==2010, 1, 0))
clean <- within(clean, yr2011<-ifelse(year==2011, 1, 0))
clean <- within(clean, yr2012<-ifelse(year==2012, 1, 0))
clean <- within(clean, yr2013<-ifelse(year==2013, 1, 0))

cleaned <- clean

save(cleaned, file = "/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/cleaned.RData")


# without time trend
M1 <- lm(log(avg_inc) ~ log(net_of_tax), data = cleaned, na.action = NULL)
summary(M1)
confint(M1)
# Durbin-Watson test for autocorrelation
dwt(M1)
# we have strong autocorr, so we use newey-west SE's
M1a <- coeftest(M1,vcov=NeweyWest)

# including time trend
M2 <- lm(log(avg_inc) ~ log(net_of_tax) + year, data = cleaned)
summary(M2)
confint(M2)
# Durbin-Watson test for autocorrelation
dwt(M2)
# we have strong autocorr, so we use newey-west SE's
coeftest(M2,vcov=NeweyWest)
NeweyWest(M2)

M3 <- lm(log(avg_inc) ~ log(net_of_tax)+yr2009+yr2010+yr2011+yr2012+yr2013, data = cleaned)
summary(M3)
confint(M3)
# Durbin-Watson test for autocorrelation
dwt(M3)
# we have strong autocorr, so we use newey-west SE's
coeftest(M3,vcov=NeweyWest)

## make a table of our results



#################
# Panel Data
#################

load("clean5obs.RData")

panel <- clean5obs

panel <- mutate(panel, t11 = ifelse(year == 2011, total_inc, 0))
panel <- mutate(panel, t11 = max(t11))
panel <- mutate(panel, t12 = ifelse(year == 2012, total_inc, 0))
panel <- mutate(panel, t12 = max(t12))
panel <- mutate(panel, t13 = ifelse(year == 2013, total_inc, 0))
panel <- mutate(panel, t13 = max(t13))

panel <- mutate(panel, dep1 = t11/total_inc)
panel <- mutate(panel, dep2 = t12/total_inc)
panel <- mutate(panel, dep3 = t13/total_inc)

panel <- mutate(panel, dep1a = ifelse(year == 2009, dep1, 1))
panel <- mutate(panel, dep2a = ifelse(year == 2010, dep2, 1))
panel <- mutate(panel, dep3a = ifelse(year == 2011, dep3, 1))

panel <- mutate(panel, dep = dep1a*dep2a*dep3a)


panel <- mutate(panel, tax11 = ifelse(year == 2011, net_of_tax, 1))
panel <- mutate(panel, tax11 = max(tax11))
panel <- mutate(panel, tax12 = ifelse(year == 2012, net_of_tax, 1))
panel <- mutate(panel, tax12 = max(tax12))
panel <- mutate(panel, tax13 = ifelse(year == 2013, net_of_tax, 1))
panel <- mutate(panel, tax13 = max(tax13))

panel <- mutate(panel, indep1 = tax11/net_of_tax)
panel <- mutate(panel, indep2 = tax12/net_of_tax)
panel <- mutate(panel, indep3 = tax13/net_of_tax)

panel <- mutate(panel, indep1a = ifelse(year == 2009, indep1, 1))
panel <- mutate(panel, indep2a = ifelse(year == 2010, indep2, 1))
panel <- mutate(panel, indep3a = ifelse(year == 2011, indep3, 1))

panel <- mutate(panel, indep = indep1a*indep2a*indep3a)

panelmodel2 <- panel[!(panel$year==2012 | panel$year==2013),]

#duplicates?
dup <- panelmodel2[c("justname", "year")]
duplicated(dup)
# we have 3 duplicates 
nodup <- dup[!duplicated(dup[c("justname", "year")]),]
# these are the three duplicates
dup2 <- dup[duplicated(dup[c("justname", "year")]),]

bad <- filter(panelmodel2, dep == 0 | indep == 0)
# 3 additional id's are bad because of strange data, we should not get 0, it's impossible
bad <- filter(panelmodel2, id2==141 | id2==934 | id2==2263 | id2==1342 | id2==1827 | id2==2535)

# dropping the observations with errors due to data tables from website

panelmodel3 <- panelmodel2[!(panelmodel2$id2==141 | panelmodel2$id2==934 | panelmodel2$id2==2263 | panelmodel2$id2==1342 | panelmodel2$id2==1827 | panelmodel2$id2==2535),]

# coplot(dep ~ year|id2, type="l", data=panelmodel2) 
                  
#OLS
ols1 <- lm(log(dep) ~ log(indep), data=panelmodel3)
summary(ols1)
confint(ols1)
# Durbin-Watson test for autocorrelation
dwt(ols1)
# we do not have strong autocorr

panelmodel3$id2 <- as.character(panelmodel3$id2)
# still can't get the panel to run using id2, using justname from now on

# Fixed

fixed <- plm(dep ~ indep, data=panelmodel3, index=c("justname", "year"), model="within")
summary(fixed)
confint(fixed)
# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed, ols1) 

#BP LM test of cross-sectional dependence
pcdtest(fixed, test = c("lm"))
# yes cross-sectional dependence?

# testing for serial correlation
pbgtest(fixed)
# yes serial correlation?

# testing for heterosk, BP test
bptest(dep ~ indep + factor(justname), data = panelmodel3, studentize=F)

