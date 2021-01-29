# data management
rm(list=ls())
library(tidyverse)
library(data.table)
library(arcos)

# # get pop by age by fips (source are census pop [POPEST] by age/sex and county)
popage<-fread("Other_data/pop_county_age_sex.csv") %>%
  filter(year%in%2006:2014) %>%
  mutate(age_cat=case_when(
    age_5yr_group<15 ~ "0-14",
    age_5yr_group%in%(15:64) ~ "15-64",
    age_5yr_group%in%(65:100) ~ "65+"
  )) %>%
  group_by(fips, age_cat) %>%
  summarise(pop=sum(pop_county)) %>%
  ungroup()
# note there are two counties (46113 and 51515) in the WaPo dataset
# that have different ids here (46102) or have merged into another one (51019)
# Oglala Lakota is easy, just change ids
popage<-popage %>%
  mutate(fips=ifelse(fips==46102, 46113, fips))
# for Bedford what we'll do is merge both city and county in the wapo dataset (same Cz)
save(popage, file="other_data/pop_by_age.rdata")

# Had to get Montgomery County, Arkansas population from our own data (WaPo data is missing it)
mtar<-fread("Other_data/pop_county.csv") %>% filter(fips==5097) %>%
  filter(year%in%2006:2014)
mtar<-mtar %>%
  mutate(BUYER_COUNTY="MONTGOMERY",
         BUYER_STATE="AR",
         countyfips="05097",
         STATE=5,
         COUNTY=97,
         county_name="Montgomery",
         NAME="Montgomery County, Arkansas",
         variable="B01003_001") %>%
  rename(population=pop_county) %>%
  select(-fips)
save(mtar, file="Other_data/Montgomery_AR_Data.rdata")

# now lets get pills
pills<-combined_buyer_annual(key="WaPo")
pills_agg<-pills %>% group_by(BUYER_STATE, BUYER_COUNTY, year) %>% 
  summarise(pills=sum(DOSAGE_UNIT))
pop<-county_population(key="WaPo")
load("Other_data/Montgomery_AR_Data.rdata")
pop<-bind_rows(pop, mtar)
contig_states_plus_dc<-pop %>% pull(BUYER_STATE) %>% unique
contig_states_plus_dc<-contig_states_plus_dc[!contig_states_plus_dc%in%c("AK", "HI")]
both<-full_join(pills_agg, pop) %>% 
  filter(BUYER_STATE%in%contig_states_plus_dc)
both %>% filter(is.na(population)) %>% print(n=100) %>% select(-NAME, -COUNTY)
both<-both %>% filter(!is.na(BUYER_COUNTY))
both<-both %>% 
  ungroup() %>% 
  mutate(fips=as.numeric(countyfips)) %>% 
  select(fips, BUYER_STATE, BUYER_COUNTY, year, pills, population) %>% 
  mutate(pills=replace_na(pills, 0))
# change bedford city for county and reaggregate
# first get names
names<-both %>% filter(fips!=51515) %>% select(fips, BUYER_STATE, BUYER_COUNTY) %>% 
  filter(!duplicated(fips))
both<-both %>%
  mutate(fips=ifelse(fips==51515, 51019, fips)) %>% 
  group_by(fips, year) %>% 
  summarise(pills=sum(pills),
            population=sum(population))
both<-both %>% left_join(names)
save(both, file="Other_data/ARCOS_county_API.rdata")
