
# add 1 to taxes paid if it is zero
clean$taxes_paid <- replace(clean$taxes_paid,clean$taxes_paid<=1, 1)

# log transforming the income variables
clean$log_taxes_paid <-log(clean$taxes_paid)
clean$log_total_inc <- log(clean$total_inc)

######## Scraping Macro Data

# OECD: Tax Revenues 2009 - 2012
# URL
URL <- 'http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/REV/NES.1000.TAXNAT.FIN?startTime=2009&endTime=2012'
sdmx <- readSDMX('http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/REV/NES.1000.TAXNAT.FIN?startTime=2009&endTime=2012')

# Data Frame
Tax09.12 <- as.data.frame(sdmx)

# Making It Tidy
drops <- c("GOV","TAX","TIME_FORMAT","Unit","PowerCode","VAR","COU")
Tax09.12 <- Tax09.12[,!(names(Tax09.12) %in% drops)]

as.numeric('obsTime', 'obsValue' )

names(Tax09.12)[1] <- "year"
names(Tax09.12)[2] <- "Total_Tax_Revenue"
Tax09.12$year <- as.numeric(Tax09.12$year)
Tax09.12$Total_Tax_Revenue <- as.numeric(Tax09.12$Total_Tax_Revenue)


# Tax Revenues 2013
Tax09.12$Total_Tax_Revenue <- Tax09.12$Total_Tax_Revenue*1000000000
Tax13 <- data.frame(year=2013, Total_Tax_Revenue =30780000000)
Tax09.13 <- rbind( Tax09.12, Tax13 )


# GDP in constant prices, national base year
Tax09.13$Total_GDP <- c(181664000000, 187100000000, 191910000000,189111000000,186831000000)

# Tax Rates & Delta Tax Rates & GDP
Tax09.13$Tax_Rate <- c(30.50, 30.00, 30.00, 29.75,31.75)

Tax09.13$DELTA_Tax_Rate <- c(NA, 0.5, 0,-0.25,1.0)


# Merge Our Scraped, Cleaned Data Sets
FINAL <- merge(clean, Tax09.13,
               by = c('year'))

#### Gathering more data

# OECD: Population Data 2009 - 2013
# URL
URL <- 'http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/POP_FIVE_HIST/FIN.YP99TLL1_ST.TT.A?startTime=2009&endTime=2013'
sdmx <- readSDMX('http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/POP_FIVE_HIST/FIN.YP99TLL1_ST.TT.A?startTime=2009&endTime=2013')

# Data Frame
Pop09.13 <- as.data.frame(sdmx)

# Making It Tidy
drops <- c("LOCATION","SUBJECT","SEX","FREQUENCY","TIME_FORMAT","Unit","OBS_STATUS")
Pop09.13 <- Pop09.13[,!(names(Pop09.13) %in% drops)]

as.numeric('obsTime', 'obsValue' )

names(Pop09.13)[1] <- "year"
names(Pop09.13)[2] <- "Population"
Pop09.13$year <- as.numeric(Pop09.13$year)
Pop09.13$Population <- as.numeric(Pop09.13$Population)

PercWorkPop <- c(0.6645439, 0.661943, 0.6570156, 0.6510898  , 0.6449715)
year <- c(2009, 2010, 2011, 2012, 2013)
WorkPop <- data.frame(year, PercWorkPop)

Pop09.13 <- merge(WorkPop, Pop09.13,
                  by = c('year'))

Pop09.13$WorkPop <- Pop09.13$PercWorkPop*Pop09.13$Population 

drops <- c("PercWorkPop","Population")
Pop09.13 <- Pop09.13[,!(names(Pop09.13) %in% drops)]


# Merge Data Sets
FINAL <- merge(FINAL, Pop09.13,
               by = c('year'))

# Create Year Dummies
FINAL <- within(FINAL, yr2009<-ifelse(year==2009, 1, 0))
FINAL <- within(FINAL, yr2010<-ifelse(year==2010, 1, 0))
FINAL <- within(FINAL, yr2011<-ifelse(year==2011, 1, 0))
FINAL <- within(FINAL, yr2012<-ifelse(year==2012, 1, 0))
FINAL <- within(FINAL, yr2013<-ifelse(year==2013, 1, 0))

# Create Dep Var
#FINAL$total2009 <- with(FINAL, sum(FINAL[yr2009==1, "taxes_paid"]))
#FINAL$total2010 <- with(FINAL, sum(FINAL[yr2010==1, "taxes_paid"])) 
#FINAL$total2011 <- with(FINAL, sum(FINAL[yr2011==1, "taxes_paid"]))  
#FINAL$total2012 <- with(FINAL, sum(FINAL[yr2012==1, "taxes_paid"])) 
#FINAL$total2013 <- with(FINAL, sum(FINAL[yr2013==1, "taxes_paid"])) 

#FINAL$share2009 <- FINAL$total2009/FINAL$Total_Tax_Revenue
#FINAL$share2010 <- FINAL$total2010/FINAL$Total_Tax_Revenue 
#FINAL$share2011 <- FINAL$total2011/FINAL$Total_Tax_Revenue 
#FINAL$share2012 <- FINAL$total2012/FINAL$Total_Tax_Revenue 
#FINAL$share2013 <- FINAL$total2013/FINAL$Total_Tax_Revenue 

share <- c(0.04898281, 0.06022747, 0.06752648, 0.06347982, 0.0770184)
year <- c(2009, 2010, 2011, 2012, 2013)
shares <- data.frame(year, share)


FINAL <- merge(FINAL, shares,
               by = c('year'))

save(FINAL, file = "/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/A3_Ott_Schneider/FINAL.RData")

################
#Descriptive Statistics
################

# createing subsets just for further analysis and ease of calculations
FINAL2009 <- subset(FINAL, year == 2009)
FINAL2010 <- subset(FINAL, year == 2010)
FINAL2011 <- subset(FINAL, year == 2011)
FINAL2012 <- subset(FINAL, year == 2012)
FINAL2013 <- subset(FINAL, year == 2013)

# number of observations by year
obs <- tally(group_by(FINAL, year))

# creating first summary statistics table
sum2_table <- merge(obs, Pop09.13,
                    by = c('year'))
percent <- (obs$n / Pop09.13$WorkPop)*100
percent_of_working <- data.frame(year, percent)

sum2_table <- merge(sum2_table, percent_of_working,
                    by = c('year'))
sum2_table <- merge(sum2_table, Tax09.13,
                    by = c('year'))
# Caption not working?
# This is our first table
kable(sum2_table, align ='c', digits = c(4,5,7,2,12,13,4,3))

# Creating our Second Summary Table
obs_all <- tally(FINAL)
mean_inc <- mean(FINAL$total_inc)
mean_tax <- mean(FINAL$taxes_paid)
mean_ratio <- mean(FINAL$ratio)
med_inc <- median(FINAL$total_inc)
med_tax <- median(FINAL$taxes_paid)
med_ratio <- median(FINAL$ratio)
sd_inc <- sd(FINAL$total_inc)
sd_tax <- sd(FINAL$taxes_paid)
sd_ratio <- sd(FINAL$ratio)

tot_inc <- c(med_inc, mean_inc, sd_inc)
tot_paid_taxes <- c(med_tax, mean_tax, sd_tax)
ave_tax_rate <- c(med_ratio, mean_ratio, sd_ratio)

# creating table labels
table3 <- c('Median', 'Mean', 'SD')
sum3_table <- data.frame(table3, tot_inc, tot_paid_taxes, ave_tax_rate)

kable(sum3_table, align ='c', digits = c(0,4,4,1))
#why won't this work: , caption = "Quick Summary Statistics on Tax Records Dataset N = 70402"

#overall summary statistics
summary(FINAL$taxes_paid)
summary(FINAL$total_inc)
summary(FINAL$ratio)
summary(FINAL$log_taxes_paid)
summary(FINAL$log_total_inc)

#########
# Figures
#########

# shows the average tax rate paid over the years, see that the range doesn't change all too much
qplot(year, ratio, data=FINAL, main='Average Tax Rate Paid Across Years', ylab ='Average Tax Rate')

# shows the income outliers in 2013; Q: should we drop them?
qplot(year, total_inc, data=FINAL, main="Total Income by Year Highlighting 2013 Outliers", ylab = 'Total Annual Income')

# plotting the density of the average tax rate
d <- density(FINAL$ratio) # returns the density data 
plot(d, main = 'Density Plot of Average Tax Rates') # plots the results

# hist(FINAL$total_inc, main = '')
# income looks slightly better after logging it
hist(FINAL$log_total_inc, main = 'Histogram of Logged Total Income', xlab = 'Log Total Annual Income')

# hist(FINAL$taxes_paid, main = '')
# taxes paid looks slightly better after logging it
hist(FINAL$log_taxes_paid, main = 'Histogram of Logged Total Taxes', xlab = 'Log Paid Taxes')

# plotting income against tax rate
qplot(ratio, log_total_inc, data=FINAL, ylab  = 'Log Total Annual Income', xlab ='Average Tax Rate', main = "Plot of Average Tax Rate Against Annual Total Income")
qplot(ratio, log_total_inc, data=FINAL, xlim=c(0,31), ylab  = 'Log Total Annual Income', xlab ='Average Tax Rate', main = "Plot of Average Tax Rate < 31% Against Log Annual Total Incomes")

# car::scatterplotMatrix(FINAL)

# plotting

sharefigure <- qplot(shares$year, shares$share, caption='Top 0.4% Share of Total Paid Finnish Taxes', ylim=c(0.04, 0.08), geom='line', ylab='Total Finnish Taxes Share Paid by Top 0.4%', xlab='Year')
sharefigure + theme_bw(base_size = 13)

##############
# Inferential Statistics
##############

# our probit models....

FINAL <- within(FINAL, less30<-ifelse(ratio<30, 1, 0))
logit1 <- glm(less30 ~ log_total_inc, data = FINAL, family = 'binomial')
summary(logit1)
confint(logit1)
fitted <- with(FINAL,data.frame(log_total_inc))
fitted$predicted <- predict(logit1, newdata = fitted, type = 'response')
qplot(fitted$predicted, log_total_inc, data = FINAL, xlab = 'Predicted Probability', ylab = 'Log Total Income', main='Predicted Probability Individual Would Have an Average Tax Rate Below 30' )

FINAL <- within(FINAL, less25<-ifelse(ratio<25, 1, 0))
logit2 <- glm(less25 ~ log_total_inc, data = FINAL, family = 'binomial')
confint(logit2)

fitted2 <- with(FINAL,data.frame(log_total_inc))
fitted2$predicted2 <- predict(logit2, newdata = fitted2, type = 'response')
qplot(fitted2$predicted2, log_total_inc, data = FINAL )

logit3 <- glm(less30 ~ log_total_inc + Tax_Rate, data = FINAL, family = 'binomial')
confint(logit3)

fitted3 <- with(FINAL,data.frame(log_total_inc))
fitted3$predicted3 <- predict(logit3, newdata = fitted3, type = 'response')
qplot(fitted3$predicted3, log_total_inc, data = FINAL )

# Causal Impact
# will look into this at a later time 
# pre.period <- c(2009, 2010, 2011, 2012)
# post.period <- c(2013)
# impact <- CausalImpact(FINAL, pre.period, post.period)

## Our OLS model needs signficant improvement, is it even appropriate?

M1 <- lm(share ~ Tax_Rate+ratio+DELTA_Tax_Rate+log(Total_GDP),data = FINAL)
summary(M1)

# Create cleaner covariate labels
labels <- c('(Intercept)', 'Top tax rate', 'Change in top tax rate',
            'Log total GDP')

stargazer::stargazer(M1, covariate.labels = labels,
                     title = 'Inappropriate Model, First Try',
                     type = 'latex', header = FALSE)


M2 <- lm(share ~ Tax_Rate+ratio+DELTA_Tax_Rate+Total_GDP+yr2009+yr2010+yr2011+yr2012+yr2013+DELTA_Tax_Rate*yr2009+DELTA_Tax_Rate*yr2010+DELTA_Tax_Rate*yr2011+DELTA_Tax_Rate*yr2012+DELTA_Tax_Rate*yr2013 ,data = FINAL)
M3 <- lm(share ~ Tax_Rate+ratio+DELTA_Tax_Rate+log(Total_GDP),data = FINAL)
M4 <- lm(share ~ Tax_Rate+ratio+DELTA_Tax_Rate+log(Total_GDP), year==2013, data = FINAL)
summary(M4)
M5 <- lm(share ~ Tax_Rate+ratio+DELTA_Tax_Rate+log(Total_GDP), year>=2012, data = FINAL)
summary(M5)

