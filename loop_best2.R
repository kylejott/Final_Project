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
library(tidyr)
library(reshape2)
library(sandwich)
library(lmtest)
library(plm)
library(Zelig)


setwd("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/")
load("all.RData")


class(all$year)

#changing the titles to english
all <- plyr::rename(x = all,
                           replace = c("Nimi" = "name",
                                       "Tulot yht" = "total_inc",
                                       "Verot" = "taxes_paid",
                                       "Suhde" = "ratio"
                                       ))
str(all)

#cleaning ratio
all$ratio2 <- str_sub(all$ratio, 1, 2)
summary(all$ratio2)
all$ratio3 <- as.numeric(all$ratio2, length=2)
summary(all$ratio3)

#cleaning taxes_paid
sub(' â¬ $', '',all$taxes_paid)
all$taxes_paid2 <- sub(' â¬$', '',all$taxes_paid)
all$taxes_paid3 <- str_trim(all$taxes_paid2)
all$taxes_paid4 <-sub(' ', '',all$taxes_paid3)
all$taxes_paid5 <-sub(' ', '',all$taxes_paid4)
all$taxes_paid6 <- as.numeric(all$taxes_paid5, length=9)
summary(all$taxes_paid6)

#cleaning total_inc
all$total_inc2 <- sub(' â¬$', '',all$total_inc)
all$total_inc3 <- str_trim(all$total_inc2)
all$total_inc4 <-sub(' ', '',all$total_inc3)
all$total_inc5 <-sub(' ', '',all$total_inc4)
all$total_inc6 <- as.numeric(all$total_inc5, length=13)
summary(all$total_inc6)

#dropping name and keeping rank in given year
all$name2 <- str_sub(all$name, 1, 5)
all$name3 <- sub('\\. ..$', '',all$name2)
all$name4 <- sub('\\. .$', '',all$name3)
all$name5 <- sub('\\. $', '',all$name4)
all$name6 <- sub('\\.$', '',all$name5)
all$name7 <- as.numeric(all$name6, length=5)
summary(all$name7)

#dropping name and keeping rank in given year
all$justname <- str_sub(all$name)
all$justname <- sub('^.*\\. ', '',all$name)


clean <- all[, (colnames(all) %in% c("justname", "name7", "total_inc6", "taxes_paid6", "ratio3", "year"))]
str(clean)
clean <- plyr::rename(x = clean,
                             replace = c("total_inc6" = "total_inc",
                                         "taxes_paid6" = "taxes_paid",
                                         "name7" = "rank",
                                         "ratio3" = "ratio"
                             ))
# add 1 to taxes paid if it is zero to avoid problems
clean$taxes_paid <- replace(clean$taxes_paid,clean$taxes_paid<=1, 1)
save(clean, file = "clean.RData")

## now we have a clean and tidy dataset!

## preparing time series dataset

###creating net-of-tax

clean["net_of_tax"] <- 1-(clean$ratio/100)

clean2 <- group_by(clean, year)
cleaned <- mutate(clean2, avg_inc=mean(total_inc))
summary(cleaned$avg_inc)

# Create Year Dummies
cleaned <- within(cleaned, yr2009<-ifelse(year==2009, 1, 0))
cleaned <- within(cleaned, yr2010<-ifelse(year==2010, 1, 0))
cleaned <- within(cleaned, yr2011<-ifelse(year==2011, 1, 0))
cleaned <- within(cleaned, yr2012<-ifelse(year==2012, 1, 0))
cleaned <- within(cleaned, yr2013<-ifelse(year==2013, 1, 0))

#cleaned is the dataset we will use for time series analysis

save(cleaned, file = "cleaned.RData")


## making our panel dataset

cleangroup <- group_by(clean, justname)
unique(cleangroup$justname)

clean <- mutate(cleangroup, numberobs = n())
clean5obs <- filter(clean, numberobs == 5)
head(clean5obs)
clean5obs <- arrange(clean5obs, desc(justname))
n_distinct(clean5obs$justname)

clean5obs$id2 <- id(clean5obs[c("justname", "numberobs")], drop = FALSE)
summary(clean5obs$id2)

#saving panel dataset
save(clean5obs, file = "/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/clean5obs.RData")



## Creating graph, bin plo like finnish top income paper
bins <- group_by(clean, year)

bins1 <- subset(bins, total_inc <= quantile(total_inc, 0.1))
binplot1 <- mutate(bins1, avetax1 = mean(ratio))
binplot1a <- summarise(binplot1, median(avetax1))

bins2 <- subset(bins, total_inc <= quantile(total_inc, 0.2))
binplot2 <- mutate(bins2, avetax2 = mean(ratio))
binplot2a <- summarise(binplot2, median(avetax2))

binplots <- merge(binplot1a, binplot2a,
               by = c('year'))

bins3 <- subset(bins, total_inc <= quantile(total_inc, 0.3))
binplot3 <- mutate(bins3, avetax3 = mean(ratio))
binplot3a <- summarise(binplot3, median(avetax3))

binplots <- merge(binplots, binplot3a,
                  by = c('year'))
                  
bins4 <- subset(bins, total_inc <= quantile(total_inc, 0.4))
binplot4 <- mutate(bins4, avetax4 = mean(ratio))
binplot4a <- summarise(binplot4, median(avetax4))

binplots <- merge(binplots, binplot4a,
                  by = c('year'))
bins5 <- subset(bins, total_inc <= quantile(total_inc, 0.5))
binplot5 <- mutate(bins5, avetax5 = mean(ratio))
binplot5a <- summarise(binplot5, median(avetax5))

binplots <- merge(binplots, binplot5a,
                  by = c('year'))
bins6 <- subset(bins, total_inc <= quantile(total_inc, 0.6))
binplot6 <- mutate(bins6, avetax6 = mean(ratio))
binplot6a <- summarise(binplot6, median(avetax6))

binplots <- merge(binplots, binplot6a,
                  by = c('year'))
bins7 <- subset(bins, total_inc <= quantile(total_inc, 0.7))
binplot7 <- mutate(bins7, avetax7 = mean(ratio))
binplot7a <- summarise(binplot7, median(avetax7))

binplots <- merge(binplots, binplot7a,
                  by = c('year'))
bins8 <- subset(bins, total_inc <= quantile(total_inc, 0.8))
binplot8 <- mutate(bins8, avetax8 = mean(ratio))
binplot8a <- summarise(binplot8, median(avetax8))

binplots <- merge(binplots, binplot8a,
                  by = c('year'))
bins9 <- subset(bins, total_inc <= quantile(total_inc, 0.9))
binplot9 <- mutate(bins9, avetax9 = mean(ratio))
binplot9a <- summarise(binplot9, median(avetax9))

binplots <- merge(binplots, binplot9a,
                  by = c('year'))

## melting and plotting the bins

binplots <- plyr::rename(x = binplots,
                      replace = c("median(avetax1)" = "1",
                                  "median(avetax2)" = "2",
                                  "median(avetax3)" = "3",
                                  "median(avetax4)" = "4",
                                  "median(avetax5)" = "5",
                                  "median(avetax6)" = "6",
                                  "median(avetax7)" = "7",
                                  "median(avetax8)" = "8",
                                  "median(avetax9)" = "9"
                      ))

binplots2 <- melt(binplots, id=c("year"))
binplots2 <- group_by(binplots2, year)

binplots2$year <- as.character(binplots2$year)

ggplot(data = binplots2, aes(x = variable,y = value, group=year)) +
          geom_line(aes(color = year)) + theme_bw(base_size = 13) +
          xlab("\nDecile Bin") +
          ylab("Average Tax Rate (%)\n") +
          ggtitle("Average Tax Rate by Deciles\n") +
          scale_colour_brewer(palette="Set1")

ggsave("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/Figures/binplot.png")

# figure on shares
# clean$total2009 <- with(clean, sum(clean[yr2009==1, "taxes_paid"]))
# clean$total2010 <- with(clean, sum(clean[yr2010==1, "taxes_paid"])) 
# clean$total2011 <- with(clean, sum(clean[yr2011==1, "taxes_paid"]))  
# clean$total2012 <- with(clean, sum(clean[yr2012==1, "taxes_paid"])) 
# clean$total2013 <- with(clean, sum(clean[yr2013==1, "taxes_paid"])) 

# clean$share2009 <- clean$total2009/clean$Total_Tax_Revenue
# clean$share2010 <- clean$total2010/clean$Total_Tax_Revenue 
# clean$share2011 <- clean$total2011/clean$Total_Tax_Revenue 
# clean$share2012 <- clean$total2012/clean$Total_Tax_Revenue 
# clean$share2013 <- clean$total2013/clean$Total_Tax_Revenue 
# since we only have 5 datapoints, easier to graph manually...

share <- c(0.04898281, 0.06022747, 0.06752648, 0.06347982, 0.0770184)
year <- c(2009, 2010, 2011, 2012, 2013)
shares <- data.frame(year, share)

dev.off()
sharefigure <- qplot(shares$year, shares$share, caption='Top 0.4% Share of Total Paid Finnish Taxes', ylim=c(0.04, 0.08), geom='line', ylab='Total Finnish Taxes Share Paid by Top 0.4%', xlab='Year')
sharefigure + theme_bw(base_size = 13)

ggsave("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/Figures/shareplot.png")




# shows the average tax rate paid over the years, see that the range doesn't change all too much

clean$year2 <- as.character(clean$year)
m <- ggplot(clean, aes(x = ratio, y= ..count.., group=year2))
m + geom_density(aes(colour=year2)) +
  scale_colour_brewer(palette="Set1") +
  theme_bw(base_size = 13) +
  xlab("\nAverage Tax Rate (%)") +
  ylab("Count\n") +
  ggtitle("Observation Counts of Average Tax Rate by Year\n")

ggsave("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/Figures/avetaxcount.png")


# shows the total income

m2 <- ggplot(clean, aes(x = log(taxes_paid), y= ..count.., group=year2))
m2 + geom_density(aes(colour=year2)) +
  scale_colour_brewer(palette="Set1") +
  theme_bw(base_size = 13) +
  xlab("\nLog Total Income (Euros)") +
  ylab("Count\n") +
  xlim(8,18)+
  ggtitle("Observation Count of Log Total Income by Year\n")

ggsave("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/Figures/incomecount.png")



