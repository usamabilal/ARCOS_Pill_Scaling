rm(list=ls())
# general packages
library(tidyverse)
library(data.table)
library(arcos)
library(broom)
library(scales)
library(gridExtra)
library(readxl)
library(lspline)
# mapping packages
library(ggmap)
library(sf)
library(tidycensus) # for the states shp
library(rgdal)
# package to find best fitting spline knot
library(segmented)

load("Other_data/ARCOS_county_API.rdata")
# 2010 CZ crosswalk obtained from https://sites.psu.edu/psucz/data/
cw<-fread("Other_data/counties10-zqvz0r.csv") %>% 
  mutate(fips=as.numeric(FIPS)) %>% rename(cz=`OUT10`)

both<-full_join(both, cw %>% select(fips, cz))
# all missing counties are AK/HI
both<-both %>% filter(!is.na(population))
cz<-both %>% group_by(cz, year) %>% 
  summarise(pills=sum(pills),
            population=sum(population))
# get states in each cz
cz<-cz %>% 
  full_join(both %>% 
              group_by(cz) %>% 
              summarise(states=paste(unique(BUYER_STATE), collapse="-")))

cz<-cz %>% group_by(cz, states) %>% summarise(pills=sum(pills), pop=mean(population))
cz %>% filter(pills==0)
# there's a single CZ with 0 pills (~2300 people). We are excluding it
cz<-cz %>% filter(pills>0)
# creating a variable with log10 pills and log10 pop (needed for segmented package below)
cz<-cz %>% mutate(logpills=log10(pills),
              logpop=log10(pop))
  


# Linear analysis
coef<-round(summary(lm(log10(pills)~log10(pop), data=cz))$coefficients[2,1], digits=2)
coef # 1.08
temp <- expression("Scaling Coefificent "~beta == 1.08)
# create sextiles of residuals for the map and colors for figure; first duplicate dataset to not mess things up
cz2<-cz
cz2$res<-lm(formula=log10(pills)~log10(pop), data=cz2) %>% augment %>% pull(.resid)
cz2<-cz2 %>% mutate(LM_Code=cz)
cz2$quant<-as.numeric(cut(cz2$res, breaks=quantile(cz2$res, probs=seq(0, 1, by=1/6)), include.lowest = T))
quantiles<-quantile(cz2$res, probs=seq(0, 1, by=1/6))

p1<-ggplot(cz2, aes(x=pop, y=pills))+
  geom_point(aes(color=res>0))+
  scale_color_manual(values=c("darkgreen", "red"))+
  #geom_abline(intercept=0, slope=1, lty=2, color="black")+
  stat_smooth(method="lm", color="black", se=F)+ 
  annotate("text", x = 10^5.5, y = 10^6, hjust=0, vjust=1,
           label = temp, color = "black", parse = T, fontface="bold")+
  scale_x_log10(breaks=10^(3:7),
                labels=c("1K", "10K", "100K", "1M", "10M"))+
  scale_y_log10(breaks=10^c(3, 6, 9),
                labels=c("1K", "1M", "1B"))+
  annotation_logticks()+
  guides(color=F)+
  labs(x="Population size",
       y="Number of Pills",
       tag="A")+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16))
p1

# map (appendix figure 1, B)
# exclude HI and AK
shp = readOGR('Other_data/ERS-2010-2fcqzlr_CZ/', 'ERS10')
exclude<-fread("Other_data/counties10-zqvz0r.csv") %>% 
  filter(floor(FIPS/1000)%in%c(2, 15)) %>% filter(!duplicated(OUT10)) %>% 
  pull(OUT10)
shp<-shp[!shp$LM_Code%in%exclude,]
shp<-merge(fortify(shp), as.data.frame(shp), by.x="id", by.y=0)
# merge with residuals from above
shp_with_data<-cz2 %>% left_join(shp)       
cols<-colorRampPalette(colors = c("blue", "white", "red"))(6)

data("state_laea")
state_map<-state_laea %>% filter(!GEOID%in%c("02", "15"))
state_map<-sf::as_Spatial(state_map$geometry)
quantile_labels<-map_chr(1:6, function(i){
  paste0(format(quantiles[i], digits=1, nsmall=1), 
         " to ", 
         format(quantiles[i+1], digits=1, nsmall=1))
})
p2<-ggplot()+
  geom_polygon(data = shp_with_data,  #  add polygon layer
               aes(x = long, y = lat,group = group, fill=as.factor(quant)),
               color='black',
               size = .2, alpha =0.8)+
  geom_polygon(data = state_map,  #  add polygon layer
               aes(x = long, y = lat,group = group),
               color='black',fill=NA,
               size = .5, alpha =.8)+
  scale_fill_manual(values=cols,labels=quantile_labels,
                    name="Residuals (SD)")+
  guides(fill=guide_legend())+
  labs(tag="B") +
  theme_void()
p2

# put figures together
pall<-arrangeGrob(grobs=list(p1, p2), ncol=2, widths=c(4.5, 5.5))
ggsave(pall, file="final_results/Figure_Arcos_Appendix1.pdf", width=15, height=5)


# lets take a look at residuals
ggplot(cz2, aes(x=pop, y=res)) +
  geom_point()+
  stat_smooth(method="loess")+
  scale_x_log10(breaks=10^(3:7),
                labels=c(paste0(c(1, 10, 100), "K"),
                         paste0(c(1, 10), "M"))) + 
  labs(x="Population", y="Residuals")+
  annotation_logticks(sides="b") +
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16))
ggsave("Final_results/Appendix_Residual_Figure.pdf", width=10, height=7.5)
# residual vs size shows mostly negative residuals in larger cities
# the trend  starts somewhere around 100k

# we'll find the best fitting knot position
m<-lm(formula=logpills~logpop, data=cz)
spline_knot<-10^segmented.lm(m, npsi=1)$psi[2]
#82k

# we'll stratify the analysis by whether CZ pop is above or below the knot
cz$popcat<-cut(cz$pop, breaks = c(min(cz$pop), spline_knot, max(cz$pop)), include.lowest = T)
cz %>% group_by(popcat) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop), data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  })
# compare AICs
AIC(lm(log10(pills)~log10(pop), data=cz))
AIC(lm(log10(pills)~log10(pop)*popcat,data=cz))


# New Figure with spline at median
coef1 <- expression("Scaling Coefificent "~beta == 1.36)
coef2 <- expression("Scaling Coefificent "~beta == 0.92)

ggplot(cz, aes(x=pop, y=pills))+
  geom_vline(xintercept = spline_knot, lty=2)+
  annotate("text", label="Knot at a population of 82,363", 
           x=spline_knot, y=10^9, hjust=-0.1, vjust=1)+
  geom_point(pch=21, color="black", fill="gray")+
  scale_color_manual(values=c("blue", "red"))+
  stat_smooth(
    formula=y~lspline(x, knots=spline_knot), 
    aes(color=popcat),
    method="lm", se=F)+ 
  annotate("text", x = 10^4, y = 10^5, hjust=0.5, vjust=1,
           label = coef1, color = "blue", parse = T, fontface="bold")+
  annotate("text", x = 10^6, y = 10^7, hjust=0.5, vjust=1,
           label = coef2, color = "red", parse = T, fontface="bold")+
  scale_x_log10(breaks=10^(3:7),
                labels=c("1K", "10K", "100K", "1M", "10M"))+
  scale_y_log10(breaks=10^c(3, 6, 9),
                labels=c("1K", "1M", "1B"))+
  annotation_logticks()+
  guides(color=F)+
  labs(x="Population size",
       y="Number of Pills",
       tag="")+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16))
ggsave("Final_results/Figure1_ARCOS.pdf", width=10, height=7.5)
ggsave("Final_results/Figure1_ARCOS.tiff", width=10, height=7.5)

# to complete table 1, get region-specific estimates
# for the analysis by region, get a crosswalk of states and census regions
census_codes<-read_excel(path = "Other_data/state-geocodes-v2015.xls", skip=5)
regions<-census_codes[census_codes$Division==0,]
divisions<-census_codes[census_codes$`State (FIPS)`=="00"&census_codes$Division!=0,]
regions<-regions[,c("Name", "Region")]
colnames(regions)[1]<-"Region_Name"
divisions<-divisions[,c("Name", "Division")]
colnames(divisions)[1]<-"Division_Name"
census_codes<-census_codes[census_codes$`State (FIPS)`!="00",]
colnames(census_codes)[3:4]<-c("state_code", "State_Name")
census_codes<-merge(census_codes, regions, by="Region")
census_codes<-merge(census_codes, divisions, by="Division")
# now we have a dataset with all states, their division, region, and name
census_codes<-census_codes[,c("Division_Name", "Region_Name", "State_Name", "state_code")]
census_codes<-census_codes %>% mutate(state=as.numeric(state_code))
# create a crosswalk of CZ with census region; assign CZs to the region they have most of their pop in
cwregions<-both %>% group_by(cz, fips) %>% summarise(pop_avg=mean(population)) %>% 
  mutate(state=floor(fips/1000)) %>% 
  left_join(census_codes) %>% 
  arrange(cz, desc(pop_avg)) %>% 
  filter(!duplicated(cz)) %>% 
  select(cz, Region_Name) %>% 
  left_join(cz %>% select(cz, states))
cz<-cz %>% left_join(cwregions)

cz %>% group_by(Region_Name, popcat) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop), data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  }) %>% 
  select(Region_Name, popcat, estimate) %>% 
  spread(popcat, estimate)

# Table 1: first unadjusted by region
row1<-cz %>% group_by(popcat) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop), data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  }) %>% 
  select(popcat, estimate, std.error) %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         coef=paste0(format(estimate, digits=2, nsmall=2),
                     " (",
                     format(lci, digits=2, nsmall=2),
                     "-",
                     format(uci, digits=2, nsmall=2),
                     ")")) %>% 
  select(popcat, coef) %>% 
  spread(popcat, coef) %>% 
  rename(b1=1,
         b2=2) %>% 
  mutate(model="Unadjusted") %>% 
  select(model, b1, b2)
# then adjusted by region
row2<-cz %>% group_by(popcat) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop)+Region_Name, data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  }) %>% 
  select(popcat, estimate, std.error) %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         coef=paste0(format(estimate, digits=2, nsmall=2),
                     " (",
                     format(lci, digits=2, nsmall=2),
                     "-",
                     format(uci, digits=2, nsmall=2),
                     ")")) %>% 
  select(popcat, coef) %>% 
  spread(popcat, coef) %>% 
  rename(b1=1,
         b2=2) %>% 
  mutate(model="Adjusted for Region") %>% 
  select(model, b1, b2)
# stratified by region
row3<-cz %>% group_by(Region_Name) %>% 
  group_modify(~{
    # .x$popcat<-cut(.x$pop, 
    #                       quantile(.x$pop, probs=seq(0,1,by=1/2)), include.lowest = T)
    .x %>% group_by(popcat) %>% 
      group_modify(~{
        lm(log10(pills)~log10(pop), data=.x) %>% 
          tidy %>% 
          filter(term=="log10(pop)")    
      }) %>% 
      select(popcat, estimate, std.error) %>% 
      mutate(lci=estimate-1.96*std.error,
             uci=estimate+1.96*std.error,
             coef=paste0(format(estimate, digits=2, nsmall=2),
                         " (",
                         format(lci, digits=2, nsmall=2),
                         "-",
                         format(uci, digits=2, nsmall=2),
                         ")")) %>% 
      select(popcat, coef) %>% 
      spread(popcat, coef) %>% 
      rename(b1=1,
             b2=2)
  }) %>% 
  rename(model=Region_Name) %>% 
  select(model, b1, b2)
# put all together in table 1
t1<-bind_rows(row1, row2, 
              data.frame(model="Stratified by Region",b1="", b2="", stringsAsFactors = F),
              row3) %>% 
  bind_cols(n=c(nrow(cz), nrow(cz), "", table(cz$Region_Name))) %>% 
  select(model, n, b1, b2)
fwrite(t1, file="Final_results/table1.csv")


# check when happens when removing the big 3 outliers at the lower end
cz$exclude_outlier<-(abs(lm(formula=log10(pills)~log10(pop)*popcat, data=cz) %>% augment %>% pull(.resid))>=1)
# excluding
outlier_table<-cz %>% filter(exclude_outlier==F) %>% 
  group_by(popcat) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop), data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  }) %>% 
  select(popcat, estimate, std.error) %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         coef=paste0(format(estimate, digits=2, nsmall=2),
                     " (",
                     format(lci, digits=2, nsmall=2),
                     "-",
                     format(uci, digits=2, nsmall=2),
                     ")")) %>% 
  select(popcat, coef) %>% 
  spread(popcat, coef) %>% 
  rename(b1=1,
         b2=2) %>% 
  mutate(model="outlier_exclusion",
         n=sum(!cz$exclude_outlier)) %>% 
  select(model, n, b1, b2)
outlier_table
# superlinearity at <median is weaker, but still clearly superlinear 
  
# check whether esults change after age adjustment what happens when doing age adjustment
load("Other_data/pop_by_age.rdata")
popage<-popage %>% 
  left_join(cw %>% select(fips, cz)) %>% 
  group_by(cz, age_cat) %>% 
  summarise(pop=sum(pop)) %>% 
  spread(age_cat, pop) %>% 
  mutate(all=sum(c(`0-14`, `15-64`, `65+`))) %>% 
  mutate(p014=`0-14`/all,
         p1564=`15-64`/all,
         p65=`65+`/all) %>% 
  select(cz, p014, p1564, p65)

age_table<-cz %>% left_join(popage) %>% 
  group_by(popcat) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop)+p014+p65, data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  }) %>% 
  select(popcat, estimate, std.error) %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         coef=paste0(format(estimate, digits=2, nsmall=2),
                     " (",
                     format(lci, digits=2, nsmall=2),
                     "-",
                     format(uci, digits=2, nsmall=2),
                     ")")) %>% 
  select(popcat, coef) %>% 
  spread(popcat, coef) %>% 
  rename(b1=1,
         b2=2) %>% 
  mutate(model="age_adjusted",
         n=nrow(cz)) %>% 
  select(model, n, b1, b2)
age_table
# same results
# Sensitivity Table
sens<-t1 %>% filter(model=="Unadjusted") %>%
  mutate(n=as.numeric(n)) %>%
  bind_rows(age_table, outlier_table)
sens
fwrite(sens, "Final_results//sens_analysis_table.csv")

# figure 2 (map)
# create sextiles of residuals
cz2<-cz
cz2$res<-lm(formula=log10(pills)~log10(pop)*popcat, data=cz2) %>% augment %>% pull(.resid)
cz2<-cz2 %>% mutate(LM_Code=cz)
cz2$quant<-as.numeric(cut(cz2$res, breaks=quantile(cz2$res, probs=seq(0, 1, by=1/6)), include.lowest = T))
quantiles<-quantile(cz2$res, probs=seq(0, 1, by=1/6))
shp_with_data<-cz2 %>% left_join(shp)       
cols<-colorRampPalette(colors = c("blue", "white", "red"))(6)

quantile_labels<-map_chr(1:6, function(i){
  paste0(format(quantiles[i], digits=1, nsmall=1), 
         " to ", 
         format(quantiles[i+1], digits=1, nsmall=1))
})
p2<-ggplot()+
  geom_polygon(data = shp_with_data,  #  add polygon layer
               aes(x = long, y = lat,group = group, fill=as.factor(quant)),
               color='black',
               size = .2, alpha =0.8)+
  geom_polygon(data = state_map,  #  add polygon layer
               aes(x = long, y = lat,group = group),
               color='black',fill=NA,
               size = .5, alpha =.8)+
  scale_fill_manual(values=cols,labels=quantile_labels,
                    name="Residuals (SD)")+
  guides(fill=guide_legend())+
  labs(tag="") +
  theme_void()
p2
ggsave("Final_results/Figure2_ARCOS.pdf", p2, width=12, height=7.5)
ggsave("Final_results/Figure2_ARCOS.tiff", p2, width=12, height=7.5)

# last, repeat analysis for CBSAs
# reload dataset
load("Other_data/ARCOS_county_API.rdata")

# Using 2013 delineations
cw<-read_excel("other_data/list1_Feb_2013.xls") %>% 
  slice(-1) %>% 
  slice(-1) %>% 
  select(1, 5, 10, 11) %>% 
  rename(cbsa=1, msa=2,
         statefips=3,
         countyfips=4) %>% 
  mutate(msa=ifelse(grepl("Micro", msa),
                    "micro", "metro"),
         cbsa=as.numeric(cbsa),
         fips=paste0(statefips, countyfips),
         fips=as.numeric(fips)) %>% 
  filter(!is.na(cbsa)) %>% 
  #filter(!duplicated(cbsa)) %>% 
  select(fips, cbsa, msa) %>% 
  # merge bedford county and city, clifton forge and Alleghany, and change shannon county and oglala lakota
  mutate(fips=replace(fips, fips==51515, 51019)) %>%
  mutate(fips=replace(fips, fips==51560, 51005)) %>%
  mutate(fips=replace(fips, fips==46113, 46102)) %>%
  # exclude AK, HI, and territories
  filter(!round(fips/1000)%in%c(2, 15, 72, 78)) %>% 
  filter(!duplicated(fips)) %>% 
  filter(!is.na(cbsa))

length(unique(both$fips))
length(unique(cw$fips))
#3108 counties in pills, 1798 counties in cbsa (not all are included)
both<-both %>% full_join(cw)
summary(both)
# no missing pop or pills
both %>% filter(is.na(cbsa))%>% pull(fips) %>% unique %>% length + 
  both %>% filter(!is.na(cbsa))%>% pull(fips) %>% unique %>% length
# no pop has missing: all CBSAs counties have a pop
# but not all have a CBSA, that's fine...# and they add up to 3109

# get states in each cbsa
cbsa<-both %>% 
  full_join(both %>% 
              group_by(cbsa) %>% 
              summarise(states=paste(unique(BUYER_STATE), collapse="-"))) %>% 
  filter(!is.na(population))

cbsa<-cbsa %>% group_by(cbsa, states, msa, year) %>% 
  summarise(pills=sum(pills),
            population=sum(population))
cbsa<-cbsa %>% group_by(cbsa, msa, states) %>% summarise(pills=sum(pills), pop=mean(population))
cbsa %>% filter(pills==0)
summary(cbsa)

# get residuals
cbsa$res<-lm(formula=log10(pills)~log10(pop), data=cbsa) %>% augment %>% pull(.resid)

cbsa %>% group_by(msa) %>% 
  group_modify(~{
    lm(log10(pills)~log10(pop), data=.x) %>% 
      tidy %>% 
      filter(term=="log10(pop)")
  }) %>% 
  select(msa, estimate) %>% 
  spread(msa, estimate) %>% 
  mutate(type="cbsa",n=nrow(cbsa)) %>% 
  rename(b1=micro, b2=metro) %>% 
  select(type, n,b1, b2) 
# Figure with spline at metro/micro
coef1 <- expression("Scaling Coefificent (micropolitan) "~beta == 1.13)
coef2 <- expression("Scaling Coefificent (metropolitan) "~beta == 0.93)

ggplot(cbsa, aes(x=pop, y=pills))+
  geom_point(pch=21, color="black", aes(fill=msa))+
  scale_fill_manual(values=c("blue", "red"))+
  stat_smooth(data=cbsa %>% filter(msa=="micro"),
              aes(color=msa), color="red",
              method="lm", se=F)+ 
  stat_smooth(data=cbsa %>% filter(msa=="metro"),
              aes(color=msa), color="blue",
              method="lm", se=F)+ 
  annotate("text", x = 10^6, y = 3*10^7, hjust=0.5, vjust=1,
           label = coef2, color = "blue", parse = T, fontface="bold")+
  annotate("text", x = 1*10^5, y = 3*10^6, hjust=0.5, vjust=1,
           label = coef1, color = "red", parse = T, fontface="bold")+
  scale_x_log10(breaks=10^(3:7),
                labels=c("1K", "10K", "100K", "1M", "10M"))+
  scale_y_log10(breaks=10^c(6, 7, 8, 9, 10),
                labels=c("1M", "10M","100M","1B", "10B"))+
  annotation_logticks()+
  guides(color=F, fill=F)+
  labs(x="Population size",
       y="Number of Pills",
       tag="")+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=14),
        plot.title =element_text(color="black", face="bold",size=16))
ggsave("Final_results/AppendixFigure2.pdf", width=10, height=7.5)
