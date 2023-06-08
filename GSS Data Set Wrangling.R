### GSS Data Analysis

# Loading Libraries
library(tidyverse)
library(haven)
library(foreign)
library(janitor)
library(labelled)

### Download GSS data from

### From total file
gss_all <- as.data.frame(read_sav("data/GSS7218_R3.sav"))

# Vars of interest

cols <- c("year", "race", 'hispanic', 'region', 'partyid', 'coninc', 'polviews')

# ### Creating a small data set to prepare for cleaning

gss_small <- gss_all %>% 
  clean_names() %>% 
  dplyr::select(cols) %>% 
  remove_empty()

write.csv(gss_small, "data/GSSsmall.csv", row.names = FALSE)

### Visualizing unique values for all variables in gss_small

for(i in 1:20){
  vals <- unique(gss_small[[i]])
  print(vals)
}

### Cleaning data

gss_small2 <- gss_small

val_labels(gss_small2) <- NULL



### Tidying the data and recoding

unique(gss_small$hispanic)
colSums(is.na(gss_small))
table(gss_small$hispanic)
unique(gss_small2$hispanic)


gss_tidy <- gss_small2 %>% 
  mutate(na_if(hispanic, '99')) %>% 
  mutate(na_if(hispanic, '0')) %>% 
  mutate(na_if(hispanic, '98')) %>% 
  mutate(hispanic = recode(hispanic,
                '1' = 0,
                '50' = 0))

table(gss_tidy$hispanic)


gss_tidy <- gss_small2 %>% 
  mutate(hispanic = na_if(hispanic, '99')) %>% 
  mutate(hispanic = na_if(hispanic, '0')) %>% 
  mutate(hispanic = na_if(hispanic, '98')) %>% 
  mutate(hispanic = recode(hispanic,
                           '1' = 0,
                           '50' = 0)) %>% 
  mutate(hispanic = recode(hispanic,
                           '2' = 1,
                           '3' = 1,
                           '4' = 1,
                           '5' = 1,
                           '6' = 1,
                           '7' = 1,
                           '8' = 1,
                           '9' = 1,
                           '10' = 1,
                           '11' = 1,
                           '15' = 1,
                           '16' = 1,
                           '20' = 1,
                           '21' = 1,
                           '22' = 1,
                           '23' = 1,
                           '24' = 1,
                           '25' = 1,
                           '30' = 1,
                           '31' = 1,
                           '35' = 1,
                           '40' = 1,
                           '41' = 1,
                           '42' = 1,
                           '45' = 1,
                           '46' = 1,
                           '47' = 1))


unique(gss_tidy$hispanic)
table(gss_tidy$hispanic)
colSums(is.na(gss_tidy))
  
 
### Race
# 1 = white, 2 = Black, 3 = other

table(gss_tidy$race)

### Recoding party

unique(gss_small$partyid)

dem <- c(0:3)
rep <- c(4:6)

gss_tidy <- gss_tidy %>% 
  mutate(partyid = na_if(partyid, 3)) %>% 
  mutate(partyid = na_if(partyid, 7)) %>% 
  mutate(partyid = na_if(partyid, 8)) %>% 
  mutate(partyid = na_if(partyid, 9)) %>% 
  mutate(partyid = recode(partyid,
                          '0' = 0,
                          '1' = 0,
                          '2' = 0,
                          '3' = 0,
                          '4' = 1,
                          '5' = 1,
                          '6' = 1))

table(gss_tidy$partyid)  


### Focus on Hispanic, Race, and Partyid

### Time to tidy the data up even further

### Region: 1 = Northeast, 2 = Midwest, 3 = South, 4 = West

gss_tidy <- gss_tidy %>% 
  select(year, race, region, partyid, hispanic) %>% 
  filter(!is.na(hispanic)) %>% 
  filter(!is.na(partyid)) %>% 
  mutate(region = recode(region,
                         '1' = 1,
                         '2' = 1,
                         '3' = 2,
                         '4' = 2,
                         '5' = 3,
                         '6' = 3,
                         '7' = 3,
                         '8' = 4,
                         '9' = 4))


colSums(!is.na(gss_tidy))
table(gss_tidy$region)  
unique(gss_small$region)  

### This is the data set!


