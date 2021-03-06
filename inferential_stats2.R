##################################
# Data Science Course
# Kyle Ott & Cornelius Schneider
# 12 December 2014
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

#set your local directory
setwd("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/")
# from loop_best2.R
load("cleaned.RData")

# Creating Summary Stats of Time Series
cleaned2 <- group_by(cleaned, year)

# number of observations by year
obs_all <- tally(cleaned2)
# total income by year
t1_totinc <- summarise(cleaned2, mean1=mean(total_inc),median1=median(total_inc), sd1=sd(total_inc))
# total taxes paid by year
t1_tottax <- summarise(cleaned2, mean2=mean(taxes_paid),median2=median(taxes_paid), sd2=sd(taxes_paid))
# average tax rate by year
t1_rat <- summarise(cleaned2, mean3=mean(ratio),median3=median(ratio), sd3=sd(ratio))

t1a <- merge(obs_all, t1_totinc,
                  by = c('year'))
t1b <- merge(t1a, t1_tottax,
             by = c('year'))
t1c <- merge(t1b, t1_rat,
             by = c('year'))

# now we want to use ungrouped data to get the totals for all
t1_totinc_sum <- summarise(cleaned, mean1=mean(total_inc),median1=median(total_inc), sd1=sd(total_inc))
t1_tottax_sum <- summarise(cleaned, mean2=mean(taxes_paid),median2=median(taxes_paid), sd2=sd(taxes_paid))
t1_rat_sum <- summarise(cleaned, mean3=mean(ratio),median3=median(ratio), sd3=sd(ratio))

obs_all_sum$year <- c('sum')
t1_totinc_sum$year <- c('sum')
t1_tottax_sum$year <- c('sum')
t1_rat_sum$year <- c('sum')

t1a_sum <- merge(obs_all_sum, t1_totinc_sum,
             by = c('year'))
t1b_sum <- merge(t1a_sum, t1_tottax_sum,
             by = c('year'))
t1c_sum <- merge(t1b_sum, t1_rat_sum,
             by = c('year'))

summarytableTS <- rbind(t1c, t1c_sum)

knitr::kable(summarytableTS, align ='c', digits = 0, format='latex', 
             col.names=c("Year", "N", "Mean Income (Euros)", "Median Income (Euros)", "SD of Income (Euros)",
                         "Mean Tax Paid (Euros)", "Median Tax Paid (Euros)", "SD of Tax Paid (Euros)",
                         "Mean Ave Tax Rate (%)", "Median Ave Tax Rate (%)", "SD of Ave Tax Rate (%)"))

# without time trend
M1 <- lm(log(avg_inc) ~ log(net_of_tax), data = cleaned)
summary(M1)
confint(M1)
# Durbin-Watson test for autocorrelation
dwt(M1)
# we have autocorr, so is it adviseable to use newey-west SE's??
# but we cannot figure out how to include this in stargazer
coeftest(M1,vcov=NeweyWest)


# including time trend
M2 <- lm(log(avg_inc) ~ log(net_of_tax) + year, data = cleaned)
summary(M2)
confint(M2)
# Durbin-Watson test for autocorrelation
dwt(M2)
# we have autocorr, so is it adviseable to use newey-west SE's??
# R will not calculate the NeweyWest errors due to matrix problems is it our data or R?
coeftest(M2,vcov=NeweyWest)
NeweyWest(M2)


#################
# Panel Data
#################

# from loop_best2.R
load("clean5obs.RData")

panel <- clean5obs

# here we are creating time dummies
panel <- mutate(panel, t11 = ifelse(year == 2011, total_inc, 0))
panel <- mutate(panel, t11 = max(t11))
panel <- mutate(panel, t12 = ifelse(year == 2012, total_inc, 0))
panel <- mutate(panel, t12 = max(t12))
panel <- mutate(panel, t13 = ifelse(year == 2013, total_inc, 0))
panel <- mutate(panel, t13 = max(t13))

# we are creating our dependent variable here
panel <- mutate(panel, dep1 = t11/total_inc)
panel <- mutate(panel, dep2 = t12/total_inc)
panel <- mutate(panel, dep3 = t13/total_inc)

panel <- mutate(panel, dep1a = ifelse(year == 2009, dep1, 1))
panel <- mutate(panel, dep2a = ifelse(year == 2010, dep2, 1))
panel <- mutate(panel, dep3a = ifelse(year == 2011, dep3, 1))

# our dependent variable is total income in t+2 divided by income in t
# multiplying these three variables will do just that
panel <- mutate(panel, dep = dep1a*dep2a*dep3a)

# here we are creating our independent variable
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

# our dependent variable is average tax rate in t+2 divided by ave tax rate in t
# multiplying these three variables will do just that
panel <- mutate(panel, indep = indep1a*indep2a*indep3a)
# with our time conversion, we end up only having 3 time periods, so 2012 and 2013 are unnecessary
panelmodel2 <- panel[!(panel$year==2012 | panel$year==2013),]

# duplicates?
# we identified a few strange things in our panel data when looking at persons names
# namely, we found duplicate names in the same year (with different income information)
# it's possible someone could have the same name, but just to be sure, we dropped those who appear
# in multiple years
dup <- panelmodel2[c("justname", "year")]
duplicated(dup)
# we have 3 duplicates 
nodup <- dup[!duplicated(dup[c("justname", "year")]),]
# these are the three duplicates
dup2 <- dup[duplicated(dup[c("justname", "year")]),]

bad <- filter(panelmodel2, dep == 0 | indep == 0)
# 3 additional id's are bad because of strange data, we should not get 0, it's impossible by definition
bad <- filter(panelmodel2, id2==141 | id2==934 | id2==2263 | id2==1342 | id2==1827 | id2==2535)

# dropping these observations, we cannot tell if they are errors from the website table or not

panelmodel3 <- panelmodel2[!(panelmodel2$id2==141 | panelmodel2$id2==934 | panelmodel2$id2==2263 | panelmodel2$id2==1342 | panelmodel2$id2==1827 | panelmodel2$id2==2535),]
save(panelmodel3, file = "/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/panelmodel3.RData")

                  
#pooled OLS
ols1 <- lm(log(dep) ~ log(indep), data=panelmodel3)
summary(ols1)
confint(ols1)
# Durbin-Watson test for autocorrelation
dwt(ols1)
# unclear if we have strong autocorr

panelmodel3$id2 <- as.character(panelmodel3$id2)
# still can't get the panel to run using id2, using justname from now on

# Fixed Effects

fixed <- plm(log(dep) ~ log(indep), data=panelmodel3, index=c("justname", "year"), model="within")
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
bptest(log(dep) ~ log(indep) + factor(justname), data = panelmodel3, studentize=F)

