##################################
# Assignment 3: Data Science Course
# Kyle Ott & Cornelius Schneider
# 12 December 2014
##################################

## This do file scrapes the data from the Finnish Newspaper Website
## It takes a while to run this code, so we only do it once and save the result
## We would prefer to be able to run this as a code chunk but it simply takes
## too long to knit (45+ minutes), even when caching...


# Load packages
library(httr)
library(dplyr)
library(XML)
library(stringr)
library(devtools)
library(rsdmx)
library(knitr)
library(tidyr)
library(reshape2)


# set your local directory
setwd("/Users/Kyle/Dropbox/!Fall_2014/Collab_Data/Final_Project/")


# 2013 data
tables2013 = data.frame()

for (i in 1:30){
  
  # URL with the ta table
  URL_temp2013 <- paste0('http://www.taloussanomat.fi/verotiedot/2013/suurituloisimmat/?n=', i)
  if (i==1) { tables2013 <- URL_temp2013 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2013 <- tables2013[[1]] }
  else if (i!=1){
    
    # Gather content and parse all tables #
    table_temp2013 <- URL_temp2013 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # Identify correct table
    # names(table) # the table does not have an ID
    
    # select first table with taxData
    tables_df_temp2013 <- table_temp2013[[1]]
    
    tables2013 <- rbind(tables2013, tables_df_temp2013)
    
  }
  #end loop
}
tables2013$year <- 2013

# 2012 data
tables2012 = data.frame()

for (i in 1:30){
  
  # URL with the medals table
  URL_temp2012 <- paste0('http://www.taloussanomat.fi/verotiedot/2012/suurituloisimmat/?n=', i)
  if (i==1) { tables2012 <- URL_temp2012 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2012 <- tables2012[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2012 <- URL_temp2012 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # Identify correct table
    # names(table) # the table does not have an ID
    
    # select first table with taxData
    tables_df_temp2012 <- table_temp2012[[1]]
    
    tables2012 <- rbind(tables2012, tables_df_temp2012)
  }
  ##end loop
}

tables2012$year <- 2012


## 2011 data
tables2011 = data.frame()

#note that for some years they don't have complete obs (15,000), which is why the loop is only up to 28 for the following year
for (i in 1:28){
  
  # URL with the medals table
  URL_temp2011 <- paste0('http://www.taloussanomat.fi/verotiedot/2011/suurituloisimmat/?n=', i)
  if (i==1) { tables2011 <- URL_temp2011 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2011 <- tables2011[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2011 <- URL_temp2011 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # Identify correct table
    # names(table) # the table does not have an ID
    
    # select first table with taxData
    tables_df_temp2011 <- table_temp2011[[1]]
    
    tables2011 <- rbind(tables2011, tables_df_temp2011)
  }
  #end loop
}

tables2011$year <- 2011

# 2010 data
tables2010 = data.frame()

for (i in 1:29){
  
  # URL with the medals table
  URL_temp2010 <- paste0('http://www.taloussanomat.fi/verotiedot/2010/suurituloisimmat/?n=', i)
  if (i==1) { tables2010 <- URL_temp2010 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2010 <- tables2010[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2010 <- URL_temp2010 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # select first table with taxData
    tables_df_temp2010 <- table_temp2010[[1]]
    
    tables2010 <- rbind(tables2010, tables_df_temp2010)
  }
  ##end loop
}

tables2010$year <- 2010

## 2009 data
tables2009 = data.frame()

for (i in 1:25){
  
  # URL with the medals table
  URL_temp2009 <- paste0('http://www.taloussanomat.fi/verotiedot/2009/suurituloisimmat/?n=', i)
  if (i==1) { tables2009 <- URL_temp2009 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2009 <- tables2009[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2009 <- URL_temp2009 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # select first table with taxData
    tables_df_temp2009 <- table_temp2009[[1]]
    
    tables2009 <- rbind(tables2009, tables_df_temp2009)
  }
  ##end loop
}

tables2009$year <- 2009

#appending scraped tables
all <- rbind(tables2009, tables2010, tables2011, tables2012, tables2013)

# this dataset is used to start loop_best2.R
save(all, file = all.RData")


