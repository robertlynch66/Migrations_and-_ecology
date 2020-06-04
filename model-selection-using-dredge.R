d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")
library("dplyr")
library(lme4)
library(MuMIn)

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
modelD1 <- glm(returnedkarelia ~ age+ sex+ 
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
                      livestock100ha_d+
                      density_d, 
               data=d2,family=binomial)

# dredge run
################## DREDGE MODEL RANK ####################
options(na.action = "na.fail") 

modelset<-dredge(modelD1, rank = AICc, trace=2) 
#modelset
summary(modelset)



##Provide an output path for AICc table - NOTE not sorted
write.table(modelset,"model_results/non-ag_directional_AICc_Table.csv",sep=",")
write.table(modelset,"C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/non-ag_directional_AICc_Table.csv",sep=",")

################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################



### repeat above to get best difference value models for non-famers
#ModelD2  and
#Run dredge


d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")


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
modelA2 <- glm(returnedkarelia ~ age+ sex+ 
                      geog_dist_org_to_evacdest+ # geographic
                      ling_dist_org_to_evacdest +#+ # cultural
                      #education+
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
                      livestock100ha_a+
                      density_a, 
                 data=d2,family=binomial)

# dredge run
################## DREDGE MODEL RANK ####################
options(na.action = "na.fail") 

modelset<-dredge(modelA2, rank = AICc,trace=2) 
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
write.table(modelset,"non-ag_absolute_AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")
#################################################

#### Repeat above for non-farmers (d2)

### repeat above to get absolute difference value models for non-famers
#ModelA2  
# and run dredge


library('loo')



TND <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Top_model_non-agricultural_directional.rds")
TNA <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Top_model_non_agricultural_absolute_vals.rds")
MND <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model1A_non_agricultural_ALL_directional.rds")
MNA <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3A_new_non_agricultural_absolute_vals.rds")

TND_l <- loo(TND)
TNA_l <- loo(TNA)
MND_l <- loo(MND)
MNA_l <- loo(MNA)

loo_compare(TND_l, TNA_l,MND_l,MNA_l)


TAD <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Top_model_agricultural_directional.rds")
TAA <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Top_model_agricultural_absolute_vals.rds")
MAD <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model1_new_agricultural_directional.rds")
MAA <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3_new_agricultural_absolute_vals.rds")


TAD_l <- loo(TAD)
TAA_l <- loo(TAA)
MAD_l <- loo(MAD)
MAA_l <- loo(MAA)

loo_compare(TAD_l, TAA_l,MAD_l,MAA_l)




loo_compare(l1, l3)


l1a <- loo(m1a)
l3a <- loo(m3a)
loo_compare(l1a, l3a)


l2 <- loo(m2)
l2a <- loo(m2a)
loo_compare(l1a, l2a, l3a)

loo_compare(l1, l2, l3)
loo_compare(waic(LL), waic(LL - 10))
