# read in the data
d <- readRDS("C:/Users/robert/Dropbox/Github/data files/ecology_data.rds")
#complete information (i.e. birth year, sex, birth place, occupation, marriage status, last place they lived 
#in Karelia and destination location in Western Finland) is available.
# make models to predict return - binomial
# We will develop a model Binomial generalized linear mixed model to predict the likelihood of returning to Karelia
#as a function of  the following environmental, cultural, geographic and linguistic factors/predictors which will 
#be assessed on the level of the municipality (i.e. municipality from which individuals migrated, municipality 
#where they were evacuated to and if they returned the municipality to which they returned):
#Annual precipitation, Land area of municipality, Lakes (% of total land area of a municipality), 
#Rivers (total river lengths per total land area of a municipality), Moraine (% of total land area of a municipality)
#, Clay (% of total land area of a municipality), Gravel or sand (% of total land area of a municipality), 
#Bedrock (% of total land area of a municipality), Peat (% of total land area of a municipality), 
#Heat summation (ºC), Forest land (% of total land area of a municipality),
#Fields (% of total land area of a municipality), Birth rate (annual mean for 1000 inhabitants), 
#Death rate (annual mean for 1000 inhabitants), Population density (total population/area), 
#Farmed area (% of total land area of a municipality using 1950 census data), 
#Income per capita - using 1930’s pre war data, Taxes per capita - using 1930’s pre war data, 
#Livestock units per 100 ha, Dialect distance matrix between linguistic groups,
#Topography and Geographic distance matrix between municipalities. 
library("tidyverse")
# select data

### dump unds who weer kids kids at evacuation (birth year >1922)
d <- d %>% filter(birthyear<1923) 
# get percent karelians of municiplaities

z <- read.csv("C:/Users/robert/Dropbox/Github/data files/percent_karelian.csv")
z <- z %>% select (1,6)
z <- z[complete.cases(z),]

d <- d %>% left_join(z, by= c("dest_municipal_id" = "dest_municipal_id"))
d$percent<- d$percent/100
#################################################################
#################################################################
#################################################################
#################################################################
# possible subsets
#data2 <- data %>% filter(birthyear < 1909)

################################################
################################################
d <- d %>% filter (returnedkarelia == 0 | ret_muni_id == org_muni_id)
                   
#data <- data[complete.cases(data),]

# 2 hypotheses
# 1 different environment hypothesis
# 2 better environment hypothesis
# DV is binomial response - return vs remained
# Model 1 - different environment hypothesis


### with directions (positives mean more of variable is good and refugees go home, negative coeficients mean more is bad
### and they stay)

d$org_density<- d$org_area/d$org_pop1945
d$dest_density <- d$dest_area/d$dest_pop1945
## scale variables
d$birthyear <- d$birthyear-min(d$birthyear, na.rm=TRUE)
d$birthyear <- d$birthyear/max(d$birthyear, na.rm=TRUE)

d$geog_dist_org_to_evacdest <- d$geog_dist_org_to_evacdest-min(d$geog_dist_org_to_evacdest, na.rm=TRUE)
d$geog_dist_org_to_evacdest <- d$geog_dist_org_to_evacdest/max(d$geog_dist_org_to_evacdest, na.rm=TRUE)

d$ling_dist_org_to_evacdest <- d$ling_dist_org_to_evacdest-min(d$ling_dist_org_to_evacdest, na.rm=TRUE)
d$ling_dist_org_to_evacdest <- d$ling_dist_org_to_evacdest/max(d$ling_dist_org_to_evacdest, na.rm=TRUE)

d$org_farmedperc <- d$org_farmedperc-min(d$org_farmedperc, na.rm=TRUE)
d$org_farmedperc <- d$org_farmedperc/max(d$org_farmedperc, na.rm=TRUE)

d$dest_farmedperc <- d$dest_farmedperc-min(d$dest_farmedperc, na.rm=TRUE)
d$dest_farmedperc <- d$dest_farmedperc/max(d$dest_farmedperc, na.rm=TRUE)

d$org_heatsum <- d$org_heatsum-min(d$org_heatsum, na.rm=TRUE)
d$org_heatsum <- d$org_heatsum/max(d$org_heatsum, na.rm=TRUE)

d$dest_heatsum <- d$dest_heatsum-min(d$dest_heatsum, na.rm=TRUE)
d$dest_heatsum <- d$dest_heatsum/max(d$dest_heatsum, na.rm=TRUE)

d$org_rainfall <- d$org_rainfall-min(d$org_rainfall, na.rm=TRUE)
d$org_rainfall <- d$org_rainfall/max(d$org_rainfall, na.rm=TRUE)

d$dest_rainfall <- d$dest_rainfall-min(d$dest_rainfall, na.rm=TRUE)
d$dest_rainfall <- d$dest_rainfall/max(d$dest_rainfall, na.rm=TRUE)

d$org_taxes <- d$org_taxes-min(d$org_taxes, na.rm=TRUE)
d$org_taxes <- d$org_taxes/max(d$org_taxes, na.rm=TRUE)

d$dest_taxes <- d$dest_taxes-min(d$dest_taxes, na.rm=TRUE)
d$dest_taxes <- d$dest_taxes/max(d$dest_taxes, na.rm=TRUE)

d$org_livestock100ha <- d$org_livestock100ha-min(d$org_livestock100ha, na.rm=TRUE)
d$org_livestock100ha <- d$org_livestock100ha/max(d$org_livestock100ha, na.rm=TRUE)

d$dest_livestock100ha <- d$dest_livestock100ha-min(d$dest_livestock100ha, na.rm=TRUE)
d$dest_livestock100ha <- d$dest_livestock100ha/max(d$dest_livestock100ha, na.rm=TRUE)

d$org_lakeperc <- d$org_lakeperc-min(d$org_lakeperc, na.rm=TRUE)
d$org_lakeperc <- d$org_lakeperc/max(d$org_lakeperc, na.rm=TRUE)

d$dest_lakeperc <- d$dest_lakeperc-min(d$dest_lakeperc, na.rm=TRUE)
d$dest_lakeperc <- d$dest_lakeperc/max(d$dest_lakeperc, na.rm=TRUE)

d$org_clayperc <- d$org_clayperc-min(d$org_clayperc, na.rm=TRUE)
d$org_clayperc <- d$org_clayperc/max(d$org_clayperc, na.rm=TRUE)

d$dest_clayperc <- d$dest_clayperc-min(d$dest_clayperc, na.rm=TRUE)
d$dest_clayperc <- d$dest_clayperc/max(d$dest_clayperc, na.rm=TRUE)

d$org_peatperc <- d$org_peatperc-min(d$org_peatperc, na.rm=TRUE)
d$org_peatperc <- d$org_peatperc/max(d$org_peatperc, na.rm=TRUE)

d$dest_peatperc <- d$dest_peatperc-min(d$dest_peatperc, na.rm=TRUE)
d$dest_peatperc <- d$dest_peatperc/max(d$dest_peatperc, na.rm=TRUE)

d$org_rockperc <- d$org_rockperc-min(d$org_rockperc, na.rm=TRUE)
d$org_rockperc <- d$org_rockperc/max(d$org_rockperc, na.rm=TRUE)

d$dest_rockperc <- d$dest_rockperc-min(d$dest_rockperc, na.rm=TRUE)
d$dest_rockperc <- d$dest_rockperc/max(d$dest_rockperc, na.rm=TRUE)

d$org_heightmedian <- d$org_heightmedian-min(d$org_heightmedian, na.rm=TRUE)
d$org_heightmedian <- d$org_heightmedian/max(d$org_heightmedian, na.rm=TRUE)

d$dest_heightmedian <- d$dest_heightmedian-min(d$dest_heightmedian, na.rm=TRUE)
d$dest_heightmedian <- d$dest_heightmedian/max(d$dest_heightmedian, na.rm=TRUE)

d$org_moraineperc <- d$org_moraineperc-min(d$org_moraineperc, na.rm=TRUE)
d$org_moraineperc <- d$org_moraineperc/max(d$org_moraineperc, na.rm=TRUE)

d$dest_moraineperc <- d$dest_moraineperc-min(d$dest_moraineperc, na.rm=TRUE)
d$dest_moraineperc <- d$dest_moraineperc/max(d$dest_moraineperc, na.rm=TRUE)

d$org_density <- d$org_density-min(d$org_density, na.rm=TRUE)
d$org_density <- d$org_density/max(d$org_density, na.rm=TRUE)

d$dest_density <- d$dest_density-min(d$dest_density, na.rm=TRUE)
d$dest_density <- d$dest_density/max(d$dest_density, na.rm=TRUE)

d$age <- 1970-d$birthyear
d$age <- d$age-min(d$age, na.rm=TRUE)
d$age <- d$age/max(d$age, na.rm=TRUE)
d$birthmunicipality<- as.factor(d$birthmunicipality)
d$birthmunicipality<- as.numeric(d$birthmunicipality)
### 
#   add kids, lotta, served during war, outbred, siblings

p <- readRDS("C:/Users/robert/Dropbox/Github/data files/person_data.rds")

## make a new conservative outbred dummy variable - 
# if primaryperson == TRUE & weddingyear <1940 and birthmunicipality_spouse == dest_muni_finland and birthregion_spouse="other"
p$outbred.cons <- ifelse (p$primaryperson==TRUE & p$weddingyear <1940 & p$outbred==1,1,0)

p <- p %>% select(id,outbred.cons,outbred)
p[is.na(p)] <- 0


d <- d %>% left_join (p, by = c("id"="id") )

d$mbw <- ifelse (d$primaryperson==TRUE & d$weddingyear <1940,1,0)
d$mbw[is.na(d$mbw)] <- 0


# read in the data agriculture only
d2 <- d %>% select(id,education, returnedkarelia,age,sex, agriculture,mbw,
                   org_muni_karelia,dest_muni_finland, 
                   geog_dist_org_to_evacdest,
                   ling_dist_org_to_evacdest,
                   org_farmedperc,dest_farmedperc,
                   org_heatsum,dest_heatsum,
                   org_rainfall,dest_rainfall,
                   org_taxes,dest_taxes,
                   org_livestock100ha,dest_livestock100ha,
                   org_lakeperc,dest_lakeperc,
                   org_clayperc,dest_clayperc,
                   org_peatperc,dest_peatperc,
                   org_rockperc,dest_rockperc,
                   org_heightmedian,dest_heightmedian,
                   org_moraineperc,dest_moraineperc,
                   org_density,dest_density,percent,
                   outbred,outbred.cons,
                   birthmunicipality)

d <- d2[complete.cases(d2), ] 


#### save ecology_data2.rds
saveRDS(d, "ecology_data2.rds")
#########################################################################################
#### Ecology_data2  reads in here
###########################################################################################
d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")

#Key variables - different bad
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)


library(dplyr)
library(lattice)
library(lme4)


##### get raw birth year back
##### Step 1 (VIF)
library(car)
vif(model) # using d2

library(lme4)
### Run agricultural with d1 and non-agricultural model with d2
#### Step 2a (farmers)
model <- glmer(returnedkarelia ~ age+ sex+ 
                 #geog_dist_org_to_evacdest+ # geographic
                 ling_dist_org_to_evacdest +#+ # cultural
                 geog_dist_org_to_evacdest+
                 #education +
                 outbred+
                 #social networks #########
                 mbw+
                 mbw*outbred+
                 ##########################
                 (org_heatsum-dest_heatsum)+
                 (org_rainfall-dest_rainfall)+ 
                 (org_clayperc-dest_clayperc)+
                 (org_heightmedian-dest_heightmedian)+
                 (org_lakeperc-dest_lakeperc)  + 
                 (org_moraineperc-dest_moraineperc) + 
                 (org_peatperc-dest_peatperc)+
                 (org_rockperc-dest_rockperc) + 
                 (org_farmedperc-dest_farmedperc) + 
                 (org_heightmedian-dest_heightmedian)+
                 (org_livestock100ha-dest_livestock100ha)+
                 (org_density-dest_density) +
                 (org_taxes-dest_taxes)+ 
                 (1|birthmunicipality), 
               data=d1,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
#vif(model)
summary(model)

#### Step 2b (non farmers) with education
model <- glmer(returnedkarelia ~ age+ sex+ 
                 #geog_dist_org_to_evacdest+ # geographic
                 ling_dist_org_to_evacdest +#+ # cultural
                 geog_dist_org_to_evacdest+
                 education +
                 outbred+
                 #social networks #########
                 mbw+
                 mbw*outbred+
                 ##########################
               (org_heatsum-dest_heatsum)+
                 (org_rainfall-dest_rainfall)+  
                 (org_clayperc-dest_clayperc)+
                 (org_heightmedian-dest_heightmedian)+
                 (org_lakeperc-dest_lakeperc)  + 
                 (org_moraineperc-dest_moraineperc) + 
                 (org_peatperc-dest_peatperc)+ 
                 (org_rockperc-dest_rockperc) + 
                 (org_farmedperc-dest_farmedperc) + 
                 (org_heightmedian-dest_heightmedian)+
                 (org_livestock100ha-dest_livestock100ha)+
                 (org_density-dest_density) + 
                 (org_taxes-dest_taxes)+ 
                 (1|birthmunicipality), 
               data=d3,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
#vif(model)
summary(model)


## correlate one with all
cor(d1[], d1$geog_dist_org_to_evacdest)


##Step 3 - final models
###############################################################################################
#### Put in variables for which neither farmers nor non-farmers exhibit strong preferenecs
#Density, rock percentage, clay percentage, geo_dist, lingusitic_dist, age and sex are DEFINTELY IN.
#Possible additions are:  rainfall, height_median and heatsum
####RUN THIS HERE!!!!!! Use d1 for all and d2 for only agricultures

## Option A) farmers
model <- glmer(returnedkarelia ~ age+ sex+ 
                 geog_dist_org_to_evacdest+ 
                 ling_dist_org_to_evacdest +
                 outbred+ 
                 mbw+
                 mbw*outbred+
                 abs(org_density-dest_density)+ 
                 abs(org_rockperc-dest_rockperc) +  
                 abs(org_clayperc-dest_clayperc) + 
                 abs(org_livestock100ha-dest_livestock100ha)+
                 abs(org_farmedperc-dest_farmedperc)+
                 abs(org_heatsum-dest_heatsum) +
                 abs(org_heightmedian-dest_heightmedian)+ 
          
                 (1|birthmunicipality), 
               data=d1,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
vif(model)
summary(model)

## Option A1) non-farmers
model <- glmer(returnedkarelia ~ age+ sex+ 
                 geog_dist_org_to_evacdest+ 
                 ling_dist_org_to_evacdest +
                 education +
                 outbred+ 
                 mbw+
                 mbw*outbred+
                 abs(org_density-dest_density)+ 
                 abs(org_rockperc-dest_rockperc) +  
                 abs(org_clayperc-dest_clayperc) + 
                 
                 abs(org_peatperc-dest_peatperc) +
                 abs(org_rainfall-dest_rainfall)+
              
                 
                 (1|birthmunicipality), 
               data=d2,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
vif(model)
summary(model)

## Option B) farmers
### Put all variables in -regardless of net differences
model <- glmer(returnedkarelia ~ age+ sex+ 
                 geog_dist_org_to_evacdest+ 
                 ling_dist_org_to_evacdest +
                 outbred+
                 mbw+
                 mbw*outbred+
                 abs(org_heatsum-dest_heatsum)+
                 abs(org_rainfall-dest_rainfall)+  
                 abs(org_clayperc-dest_clayperc)+
                 abs(org_heightmedian-dest_heightmedian)+
                 abs(org_lakeperc-dest_lakeperc)  + 
                 abs(org_moraineperc-dest_moraineperc) +  
                 abs(org_peatperc-dest_peatperc)+ 
                 abs(org_rockperc-dest_rockperc) + 
                 abs(org_farmedperc-dest_farmedperc) +  
                 abs(org_heightmedian-dest_heightmedian)+
                 abs(org_livestock100ha-dest_livestock100ha)+
                 abs(org_density-dest_density) + 
                 abs(org_taxes-dest_taxes)+ 
                
                 (1|birthmunicipality), 
               data=d1,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
vif(model)
summary(model)

## Option B1) non-farmers
model <- glmer(returnedkarelia ~ age+ sex+ 
                 geog_dist_org_to_evacdest+ 
                 ling_dist_org_to_evacdest +
                 #education+
                 outbred+ 
                 mbw+
                 mbw*outbred+
                 abs(org_heatsum-dest_heatsum)+
                 abs(org_rainfall-dest_rainfall)+ 
                 abs(org_clayperc-dest_clayperc)+
                 abs(org_heightmedian-dest_heightmedian)+
                 abs(org_lakeperc-dest_lakeperc)  + 
                 abs(org_moraineperc-dest_moraineperc) +  
                 abs(org_peatperc-dest_peatperc)+ 
                 abs(org_rockperc-dest_rockperc) + 
                 abs(org_farmedperc-dest_farmedperc) +  
                 abs(org_heightmedian-dest_heightmedian)+
                 abs(org_livestock100ha-dest_livestock100ha)+
                 abs(org_density-dest_density) + 
                 abs(org_taxes-dest_taxes)+ 
                 
                 (1|birthmunicipality), 
               data=d1,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
vif(model)
summary(model)
## add kids, lotta, served during war, outbred, siblings,language, 
# Bayesian models  #### see run.R file
diff_environment_model <- stan_glmer(returnedkarelia ~ geog_dist_org_to_evacdest+ ling_dist_org_to_evacdest,data=data,family=binomial)
#### model selection #############################################
#### 
#### # model selection with absolute values plus directionals diffs
library(lme4)
library(MuMIn)

library(lme4)
model <- glmer(returnedkarelia ~ age+ sex+ 
                 geog_dist_org_to_evacdest+ 
                 ling_dist_org_to_evacdest +
                 education+
                 outbred+ 
                 mbw+
                 mbw*outbred+
                 abs(org_heatsum-dest_heatsum)+
                 abs(org_rainfall-dest_rainfall)+  
                 abs(org_clayperc-dest_clayperc)+
                 abs(org_heightmedian-dest_heightmedian)+
                 abs(org_lakeperc-dest_lakeperc)  + 
                 abs(org_moraineperc-dest_moraineperc) +  
                 #abs(org_peatperc-dest_peatperc)+ 
                 abs(org_rockperc-dest_rockperc) + 
                 abs(org_farmedperc-dest_farmedperc) + 
                 abs(org_heightmedian-dest_heightmedian)+
                 abs(org_livestock100ha-dest_livestock100ha)+
                 abs(org_density-dest_density) + 
                 abs(org_taxes-dest_taxes)+
                 (org_heatsum-dest_heatsum)+
                 (org_rainfall-dest_rainfall)+  
               (org_clayperc-dest_clayperc)+
               (org_heightmedian-dest_heightmedian)+
               (org_lakeperc-dest_lakeperc)  + 
               (org_moraineperc-dest_moraineperc) + 
               #(org_peatperc-dest_peatperc)+ 
               (org_rockperc-dest_rockperc) + 
               (org_farmedperc-dest_farmedperc) + 
               (org_heightmedian-dest_heightmedian)+
               (org_livestock100ha-dest_livestock100ha)+
               (org_density-dest_density) +
              (1|birthmunicipality),  # random effect removed
               data=d2,family=binomial, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
# dredge run
################## DREDGE MODEL RANK ####################
options(na.action = "na.fail") 

modelset<-dredge(model, rank = AICc, trace=FALSE) 
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
write.table(modelset,"C:/Users/rofrly/Desktop/AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")

######################################################################
#####################################################################

######################################################################
######################################################################
###################RUN MODELS IN BRMS
# But you may want to do this directly in an open R console on the CSC cluster
## TELL CSC R where to load and get the rpackages
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
install.packages("brms", repos='http://cran.us.r-project.org',lib=libpath)

library("tidyr")
library("brms")
p <- readRDS("/scratch/project_2000853/ecology_data2.rds")










### Try to load rstanarm
## Read data form following location AFTER Loading it there
library("rstanarm")
p <- readRDS("/scratch/project_2000853/person_data.rds")
saveRDS(p, "person_data.rds")
#


# This command can be used to check that the folder is now visible:
.libPaths() # It should be first on the list

# Package installations should now be directed to the project
# folder by default. You can also specify the path, e.g.:
install.packages("package", lib = libpath)




####extra shit
# Final variables
age
sex
geog_dist_org_to_evacdest
ling_dist_org_to_evacdest
org_rainfall-dest_rainfall
org_lakeperc-dest_lakeperc
org_moraineperc-dest_moraineperc
org_clayperc-dest_clayperc
org_gravelperc-dest_gravelperc
org_peatperc-dest_peatperc
org_rockperc-dest_rockperc
org_heatsum-dest_heatsum
org_farmedperc-dest_farmedperc
org_livestock100ha-dest_livestock100ha
org_heightmedian-dest_heightmedian
org_moraineperc-dest_moraineperc
org_density-dest_density
org_taxes-dest_taxes
# adding number 14 (farm total area) reduces sample to ~7,000 from 22,056

data <- data %>% select(2,6,122,116,117,46,70,47,71,48,72,50,74,
                        51,75,52,76,53,77,55,79,56,80,59,83,
                        64,88,48,72,120,121,60,84)
data <- data[complete.cases(data),]
# frequentist models 
# different environment model

#######

#### add birthmunicipality as a clustering variable to final model - positives means big diff return, negative mean big diff remain
library(lme4)
model <- glm(returnedkarelia ~ age+ sex+ 
               geog_dist_org_to_evacdest+ # geographic
               ling_dist_org_to_evacdest+ # cultural
               abs(org_rainfall-dest_rainfall)  +  abs(org_lakeperc-dest_lakeperc)  + #environmental 
               abs(org_moraineperc-dest_moraineperc) + abs(org_clayperc-dest_clayperc) + #environmental 
               abs(org_gravelperc-dest_gravelperc)+ abs(org_peatperc-dest_peatperc)+ #environmental
               abs(org_rockperc-dest_rockperc) + abs(org_heatsum-dest_heatsum) + #environmental
               abs(org_farmedperc-dest_farmedperc) + abs(org_livestock100ha-dest_livestock100ha)+ #environmental
               abs(org_heightmedian-dest_heightmedian)+abs(org_moraineperc-dest_moraineperc)+ #environmental
               abs(org_density-dest_density) + # cultural or environmental or demographic
               abs(org_taxes-dest_taxes), # economic/cultural
             #(1|birthmunicipality), # random effect
             data=d2,family=binomial)

summary(model)



### get CI's for parameter estimates from bayes models
posterior_interval(m1a, prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)

#### credibility intervals
library('loo')

loo1 <- loo(m1, cores = 2)


m1 <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model1_new_agricultural_directional.rds")
m3 <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3_new_agricultural_absolute_vals.rds")
m1a <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model1A_new_non_agricultural_directional.rds")
m3a <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3A_new_non_agricultural_absolute_vals.rds")


l1 <- loo(m1)
l3 <- loo(m3)

loo_compare(l1, l3)


l1a <- loo(m1a)
l3a <- loo(m3a)
loo_compare(l1a, l3a)


l2 <- loo(m2)
l2a <- loo(m2a)
loo_compare(l1a, l2a, l3a)

loo_compare(l1, l2, l3)
loo_compare(waic(LL), waic(LL - 10))



##################NEW MODELS ###########################################################
##################NEW MODELS ###########################################################
##################NEW MODELS ###########################################################
##################NEW MODELS ###########################################################

# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_d <- d$org_heatsum-d$dest_heatsum
d$rainfall_d <- d$org_rainfall-d$dest_rainfall
d$clayperc_d <- d$org_clayperc-d$dest_clayperc
d$heightmedian_d <- d$org_heightmedian-d$dest_heightmedian
d$lakeperc_d <- d$org_lakeperc-d$dest_lakeperc
d$moraineperc_d <- d$org_moraineperc-d$dest_moraineperc
d$peatperc_d <- d$org_peatperc-d$dest_peatperc
d$rockperc_d <- d$org_rockperc-d$dest_rockperc
d$taxes_d <- d$org_taxes-d$dest_taxes
d$farmedperc_d <- d$org_farmedperc-d$dest_farmedperc
d$heightmedian_d  <- d$org_heightmedian-d$dest_heightmedian
d$livestock100ha_d <- d$org_livestock100ha-d$dest_livestock100ha
d$density_d  <- d$org_density-d$dest_density

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      education+
                      outbred+ # cultural
                      mbw+
                      mbw*outbred+
                      heatsum_d+
                      rainfall_d+  
                      clayperc_d+
                      heightmedian_d +
                      lakeperc_d +  
                      moraineperc_d +  
                      peatperc_d+ 
                      rockperc_d + 
                      taxes_d +
                      farmedperc_d + 
                      heightmedian_d +
                      livestock100ha_d+
                      density_d +
                      (1|birthmunicipality),
                    data=d2,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Model1A_new_non_agricultural_directional.rds"
saveRDS(model, paste0(path, filename))

##########################################################################3
# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_d <- d$org_heatsum-d$dest_heatsum
d$rainfall_d <- d$org_rainfall-d$dest_rainfall
d$clayperc_d <- d$org_clayperc-d$dest_clayperc
d$heightmedian_d <- d$org_heightmedian-d$dest_heightmedian
d$lakeperc_d <- d$org_lakeperc-d$dest_lakeperc
d$moraineperc_d <- d$org_moraineperc-d$dest_moraineperc
d$peatperc_d <- d$org_peatperc-d$dest_peatperc
d$rockperc_d <- d$org_rockperc-d$dest_rockperc
d$taxes_d <- d$org_taxes-d$dest_taxes
d$farmedperc_d <- d$org_farmedperc-d$dest_farmedperc
d$heightmedian_d  <- d$org_heightmedian-d$dest_heightmedian
d$livestock100ha_d <- d$org_livestock100ha-d$dest_livestock100ha
d$density_d  <- d$org_density-d$dest_density

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      #education+
                      outbred+ # cultural
                      mbw+
                      mbw*outbred+
                      heatsum_d+
                      rainfall_d+  
                      clayperc_d+
                      heightmedian_d +
                      lakeperc_d +  
                      moraineperc_d +  
                      peatperc_d+ 
                      rockperc_d + 
                      taxes_d +
                      farmedperc_d + 
                      heightmedian_d +
                      livestock100ha_d+
                      density_d +
                      (1|birthmunicipality),
                    data=d1,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Model1A_new_agricultural_directional.rds"
saveRDS(model, paste0(path, filename))


###################################################################
# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_a <- abs(d$org_heatsum-d$dest_heatsum)
d$rainfall_a <- abs(d$org_rainfall-d$dest_rainfall)
d$clayperc_a <- abs(d$org_clayperc-d$dest_clayperc)
d$heightmedian_a <- abs(d$org_heightmedian-d$dest_heightmedian)
d$lakeperc_a <- abs(d$org_lakeperc-d$dest_lakeperc)
d$moraineperc_a <- abs(d$org_moraineperc-d$dest_moraineperc)
d$peatperc_a <- abs(d$org_peatperc-d$dest_peatperc)
d$rockperc_a <- abs(d$org_rockperc-d$dest_rockperc)
d$taxes_a <- abs(d$org_taxes-d$dest_taxes)
d$farmedperc_a <- abs(d$org_farmedperc-d$dest_farmedperc)
d$heightmedian_a  <- abs(d$org_heightmedian-d$dest_heightmedian)
d$livestock100ha_a <- abs(d$org_livestock100ha-d$dest_livestock100ha)
d$density_a  <- abs(d$org_density-d$dest_density)

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      education+
                      outbred+ # cultural
                      mbw+
                      mbw*outbred+
                      heatsum_a+
                      rainfall_a+  
                      clayperc_a+
                      heightmedian_a +
                      lakeperc_a +  
                      moraineperc_a +  
                      peatperc_a+ 
                      rockperc_a + 
                      taxes_a +
                      farmedperc_a + 
                      heightmedian_a +
                      livestock100ha_a+
                      density_a +
                      (1|birthmunicipality),
                    data=d1,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Model3_new_agricultural_absolute_vals.rds"
saveRDS(model, paste0(path, filename))
###################################################################
# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_a <- abs(d$org_heatsum-d$dest_heatsum)
d$rainfall_a <- abs(d$org_rainfall-d$dest_rainfall)
d$clayperc_a <- abs(d$org_clayperc-d$dest_clayperc)
d$heightmedian_a <- abs(d$org_heightmedian-d$dest_heightmedian)
d$lakeperc_a <- abs(d$org_lakeperc-d$dest_lakeperc)
d$moraineperc_a <- abs(d$org_moraineperc-d$dest_moraineperc)
d$peatperc_a <- abs(d$org_peatperc-d$dest_peatperc)
d$rockperc_a <- abs(d$org_rockperc-d$dest_rockperc)
d$taxes_a <- abs(d$org_taxes-d$dest_taxes)
d$farmedperc_a <- abs(d$org_farmedperc-d$dest_farmedperc)
d$heightmedian_a  <- abs(d$org_heightmedian-d$dest_heightmedian)
d$livestock100ha_a <- abs(d$org_livestock100ha-d$dest_livestock100ha)
d$density_a  <- abs(d$org_density-d$dest_density)

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      education+
                      outbred+ # cultural
                      mbw+
                      mbw*outbred+
                      heatsum_a+
                      rainfall_a+  
                      clayperc_a+
                      heightmedian_a +
                      lakeperc_a +  
                      moraineperc_a +  
                      peatperc_a+ 
                      rockperc_a + 
                      taxes_a +
                      farmedperc_a + 
                      heightmedian_a +
                      livestock100ha_a+
                      density_a +
                      (1|birthmunicipality),
                    data=d2,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Model3A_new_non_agricultural_absolute_vals.rds"
saveRDS(model, paste0(path, filename))


##### Top models (determined by model selection AIC scores)

# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_a <- abs(d$org_heatsum-d$dest_heatsum)
d$rainfall_a <- abs(d$org_rainfall-d$dest_rainfall)
d$clayperc_a <- abs(d$org_clayperc-d$dest_clayperc)
d$heightmedian_a <- abs(d$org_heightmedian-d$dest_heightmedian)
d$lakeperc_a <- abs(d$org_lakeperc-d$dest_lakeperc)
d$moraineperc_a <- abs(d$org_moraineperc-d$dest_moraineperc)
d$peatperc_a <- abs(d$org_peatperc-d$dest_peatperc)
d$rockperc_a <- abs(d$org_rockperc-d$dest_rockperc)
d$taxes_a <- abs(d$org_taxes-d$dest_taxes)
d$farmedperc_a <- abs(d$org_farmedperc-d$dest_farmedperc)
d$heightmedian_a  <- abs(d$org_heightmedian-d$dest_heightmedian)
d$livestock100ha_a <- abs(d$org_livestock100ha-d$dest_livestock100ha)
d$density_a  <- abs(d$org_density-d$dest_density)

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      education+
                      outbred+ # cultural
                      #mbw+
                      #mbw*outbred+
                      heatsum_a+
                      rainfall_a+  
                      #clayperc_a+
                      #heightmedian_a +
                      #lakeperc_a +  
                      moraineperc_a +  
                      peatperc_a+ 
                      #rockperc_a + 
                      taxes_a +
                      #farmedperc_a + 
                      livestock100ha_a+
                      density_a +
                      (1|birthmunicipality),
                    data=d2,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Top_model_non_agricultural_absolute_vals.rds"
saveRDS(model, paste0(path, filename))

#######################################################################################

# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_a <- abs(d$org_heatsum-d$dest_heatsum)
d$rainfall_a <- abs(d$org_rainfall-d$dest_rainfall)
d$clayperc_a <- abs(d$org_clayperc-d$dest_clayperc)
d$heightmedian_a <- abs(d$org_heightmedian-d$dest_heightmedian)
d$lakeperc_a <- abs(d$org_lakeperc-d$dest_lakeperc)
d$moraineperc_a <- abs(d$org_moraineperc-d$dest_moraineperc)
d$peatperc_a <- abs(d$org_peatperc-d$dest_peatperc)
d$rockperc_a <- abs(d$org_rockperc-d$dest_rockperc)
d$taxes_a <- abs(d$org_taxes-d$dest_taxes)
d$farmedperc_a <- abs(d$org_farmedperc-d$dest_farmedperc)
d$heightmedian_a  <- abs(d$org_heightmedian-d$dest_heightmedian)
d$livestock100ha_a <- abs(d$org_livestock100ha-d$dest_livestock100ha)
d$density_a  <- abs(d$org_density-d$dest_density)

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      #geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      #education+
                      outbred+ # cultural
                      mbw+
                      #mbw*outbred+
                      heatsum_a+
                      rainfall_a+  
                      #clayperc_a+
                      heightmedian_a +
                      #lakeperc_a +  
                      moraineperc_a +  
                      #peatperc_a+ 
                      rockperc_a + 
                      taxes_a +
                      #farmedperc_a + 
                      livestock100ha_a+
                      density_a +
                      (1|birthmunicipality),
                    data=d1,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Top_model_agricultural_absolute_vals.rds"
saveRDS(model, paste0(path, filename))

###########################################################
# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_d <- d$org_heatsum-d$dest_heatsum
d$rainfall_d <- d$org_rainfall-d$dest_rainfall
d$clayperc_d <- d$org_clayperc-d$dest_clayperc
d$heightmedian_d <- d$org_heightmedian-d$dest_heightmedian
d$lakeperc_d <- d$org_lakeperc-d$dest_lakeperc
d$moraineperc_d <- d$org_moraineperc-d$dest_moraineperc
d$peatperc_d <- d$org_peatperc-d$dest_peatperc
d$rockperc_d <- d$org_rockperc-d$dest_rockperc
d$taxes_d <- d$org_taxes-d$dest_taxes
d$farmedperc_d <- d$org_farmedperc-d$dest_farmedperc
d$heightmedian_d  <- d$org_heightmedian-d$dest_heightmedian
d$livestock100ha_d <- d$org_livestock100ha-d$dest_livestock100ha
d$density_d  <- d$org_density-d$dest_density

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      #education+
                      outbred+ # cultural
                      mbw+
                      #mbw*outbred+
                      heatsum_d+
                      rainfall_d+  
                      clayperc_d+
                      #heightmedian_d +
                      lakeperc_d +  
                      moraineperc_d +  
                      peatperc_d+ 
                      rockperc_d + 
                      #taxes_d +
                      farmedperc_d + 
                      #heightmedian_d +
                      livestock100ha_d+
                      density_d +
                      (1|birthmunicipality),
                    data=d1,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Top_model_agricultural_directional.rds"
saveRDS(model, paste0(path, filename))

###########################################################
# read in data and packages 
.libPaths(c("/projappl/project_2000853/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
#install.packages("tidyr", repos='http://cran.us.r-project.org',lib=libpath)
#install.packages("rstanarm", repos='http://cran.us.r-project.org',lib=libpath)
# from local
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library("rstanarm")
d <- readRDS("/scratch/project_2000853/ecology_data2.rds")

d$heatsum_d <- d$org_heatsum-d$dest_heatsum
d$rainfall_d <- d$org_rainfall-d$dest_rainfall
d$clayperc_d <- d$org_clayperc-d$dest_clayperc
d$heightmedian_d <- d$org_heightmedian-d$dest_heightmedian
d$lakeperc_d <- d$org_lakeperc-d$dest_lakeperc
d$moraineperc_d <- d$org_moraineperc-d$dest_moraineperc
d$peatperc_d <- d$org_peatperc-d$dest_peatperc
d$rockperc_d <- d$org_rockperc-d$dest_rockperc
d$taxes_d <- d$org_taxes-d$dest_taxes
d$farmedperc_d <- d$org_farmedperc-d$dest_farmedperc
d$heightmedian_d  <- d$org_heightmedian-d$dest_heightmedian
d$livestock100ha_d <- d$org_livestock100ha-d$dest_livestock100ha
d$density_d  <- d$org_density-d$dest_density

# subset data
d1 <- d %>% filter(agriculture==1)
d2 <- d %>% filter(agriculture==0)

### Run in rstanarm
model <- stan_glmer(returnedkarelia ~ age+ sex+ 
                      #geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      education+
                      outbred+ # cultural
                      #mbw+
                      #mbw*outbred+
                      #heatsum_d+
                      rainfall_d+  
                      clayperc_d+
                      #heightmedian_d +
                      lakeperc_d +  
                      #moraineperc_d +  
                      peatperc_d+ 
                      rockperc_d + 
                      #taxes_d +
                      farmedperc_d + 
                      #heightmedian_d +
                      #livestock100ha_d+
                      density_d +
                      (1|birthmunicipality),
                    data=d2,
                    family = binomial(link = "logit"), 
                    prior = normal(0,1),
                    prior_intercept = normal(0,1),
                    chains = 4, iter = 8000, warmup = 2000)



path<- (paste0("results/"))
filename <- "Top_model_non-agricultural_directional.rds"
saveRDS(model, paste0(path, filename))