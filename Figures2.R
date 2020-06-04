## make figures from rstanarm models
# copied from https://www.tjmahr.com/visualizing-uncertainty-rstanarm/
library("rstanarm")
library(tidyverse)
d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")

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
#Key variables - different bad
d1 <- d %>% filter(agriculture==1)  # 9,870
d2 <- d %>% filter(agriculture==0)  # 12,204
###########################################################################################
###########################################################################################
###########################################################################################
setwd("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/Figures")
## Model 3 - farmers absolute values
# facet 1
x_rng <- range(d1$ling_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=x_steps, 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  #education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=mean(taxes_a))
detach(d1)

#m3 <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3_agricultural_ALL_absolutevals.rds")
m3 <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3_new_agricultural_absolute_vals.rds")
pred_lin <- posterior_linpred(m3, newdata = new_data2,re.form=~0)
dim(pred_lin)
pred_lin <- pred_lin-1
## whyare prediction for m3 from 1.44 to 2.15??? and not between 0 and 1
head(pred_lin)
colMeans(pred_lin)
### make function and plot predicted intervals
tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .05, prob_upr = .95) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_data_frame %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  df_pred
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(mean = mean(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(ling_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))

# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)

# make a labels list
lab_lines <- list(
  returned_karelia = "Predicted probability of return", 
  ling_dist_org_to_evacdest = "Lingusitic distance",
  geog_dist_org_to_evacdest = "Geographic distance"
)
# temporary fix because predcited values are fucked up
data$mean<- data$mean
data$lower[1:4]<- 0
data$upper[17:20]<- 1
## make plot 1

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot1 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=ling_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(ling_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(ling_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(ling_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = ling_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=ling_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Lingusitic distance", limits = c(-0.05, 1.05),breaks=c(0.2,0.8),
                     labels=c("Similar","Different")) + 
  scale_y_continuous(name="Predicted probability of returning", limits = c(-0.02, 1.5), breaks=c(0,0.375,0.75,1.125,1.5),
                     labels=c("0","0.25","0.5","0.75","1.0"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot1
#################################################
#################################################
#################################################
#################################################
# facet 2
x_rng <- range(d1$geog_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d1)
  new_data2 <- data_frame(
    ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
    age = mean(age),
    sex=mean(sex),
    observation=seq_along(x_steps),
    geog_dist_org_to_evacdest=x_steps,
    #ling_dist_org_to_evacdest                        
    #education= mean(education),
    outbred= mean(outbred),                                        
    mbw =mean(mbw),                                             
    heatsum_a=mean(heatsum_a),
    rainfall_a= mean(rainfall_a),
    clayperc_a =mean(clayperc_a),
    heightmedian_a=mean(heightmedian_a),
    lakeperc_a=mean(lakeperc_a),
    moraineperc_a=mean(moraineperc_a),
    peatperc_a=mean(peatperc_a),
    rockperc_a=mean(rockperc_a),
    farmedperc_a =mean(farmedperc_a),
    livestock100ha_a=mean(livestock100ha_a),
    density_a=mean(density_a),
    taxes_a=mean(taxes_a))
detach(d1)



pred_lin <- posterior_linpred(m3, newdata = new_data2,re.form=~0)
dim(pred_lin)

## whyare prediction for m3 from 1.42 to 2.72??? and not between 0 and 1
head(pred_lin)
colMeans(pred_lin)
pred_lin <- pred_lin-1
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(geog_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))

#ldf <- ldf[1:16,]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)
data <- data[1:16,]
data$mean[14:16] <-1
data$lower[1:2] <- 0
data$upper[11:16] <- 1
## make plot 2

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot2 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=geog_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(geog_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(geog_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(geog_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = geog_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=geog_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Geographic distance", limits = c(-0.05, 0.80), breaks=c(0.2,0.6), labels=c("Near","Far")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.05), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot2

#################################################################
#################################################################
#################################################################
#################################################################
# facet 3
#x_rng <- range(d1$geog_dist_org_to_evacdest) 
#x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=c(0L,1L),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  outbred= c(0L,1L),                                        
  mbw =mean(mbw),                                             
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=mean(taxes_a))
detach(d1)



pred_lin <- posterior_linpred(m3, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin-1
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(outbred, b = 2))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- c(1L,2L)

# plot both together  and select 95% CI 
p1 <-pred_lin[,1] 

p1 <-p1[p1 > quantile(p1,0.05) & p1 < quantile(p1,0.95)] %>% as.data.frame()
p1$outbred <- 0
p1$observation <- 1

p2 <-pred_lin[,2] 
p2 <-p2[p2 > quantile(p2,0.05) & p2 < quantile(p2,0.95)] %>% as.data.frame()
p2$outbred <- 1
p2$observation <- 2
p <- bind_rows(p1,p2)
names(p)[1] <- "predicted"

data <- p %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df


## make plot 3

library(tidyverse)
data$N <-data$n.rk



theme_set(theme_minimal())
plot3 <- ggplot(data = data, aes(x = factor(outbred), y = predicted)) +
  #geom_violin(fill = "dodgerblue",position="dodge") + 
  geom_violin(aes(fill="dodgerblue")) +
  geom_errorbar(aes(x=factor(outbred), ymin=(mean.rk-se.rk),ymax=(mean.rk+se.rk),size=0.15))+                                                                                                                                                                                                                                                                           
  geom_point(alpha=1, size=1, aes(x = factor(outbred), y = mean.rk)) +
  scale_shape_identity()+
  scale_x_discrete(name="Marriage", breaks = c(0, 1), labels=c("Married in","Married out")) + 
  
  
  
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.50,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  #scale_size_area(max_size = 2.5)+ 
  scale_size_continuous(name='Mean and SE\n  (observed)',labels = NULL) +
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),    
        axis.text.x = element_text(size=12))
#axis.text.y = element_text(face="bold",size=12))

plot3


#################################################
#################################################
#################################################
#################################################
# facet 4
#d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")

#Key variables - different bad
#d1 <- d %>% filter(agriculture==1)  # 9,870
#d2 <- d %>% filter(agriculture==0)  # 12,204
# make org taxes minus dest taxes a sequence of 20 numbers

x_rng <- range(d1$taxes_a) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)


attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=x_steps)
detach(d1)



pred_lin <- posterior_linpred(m3, newdata = new_data2,re.form=~0)
dim(pred_lin)

## whyare prediction for m3 from 1.42 to 2.72??? and not between 0 and 1
head(pred_lin)
colMeans(pred_lin)
pred_lin <- pred_lin-1.0
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(taxes_a, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))


ldf <- ldf[c(1:7, 9,11,13), ]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)
data <- data[1:13,]
data$lower[10:13] <- 0
data$upper[1] <- 1.0

## make plot 2

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot4 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=taxes_a,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(taxes_a, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(taxes_a, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(taxes_a, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = taxes_a, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=taxes_a, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Tax rate", limits = c(-0.05, 0.69), breaks=c(0.1,0.55),
                     labels=c("Similar","Different")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.50,0.75,1.0),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot4

##### Make facet grid
library(ggpubr)
Fig_1<- ggarrange(plot1, plot2, plot3,plot4,ncol=4,nrow=1,vjust=1,hjust=-4,common.legend = TRUE, 
                  font.label = list(size = 7, color = "black", face = "bold", family = NULL),
                  legend="right",labels=c("", "", ""))

Fig_1


ggsave(Fig_1, filename = "Figure_1_farmers_absolute_vals.png", width = 20, height = 4, device = "png", units = "in")

ggsave(Fig_1, filename = "Figure_1_farmers_absolute_vals.pdf", width = 20, height = 4, device = "pdf", units = "in")




#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# Plot Model 3a - non-famers 

###########################################################################################
###########################################################################################
###########################################################################################
# facet 1
x_rng <- range(d2$ling_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=x_steps, 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=mean(taxes_a))
detach(d2)

#m3a <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3A_non_agricultural_ALL_absolutevals.rds")
m3a <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model3A_new_non_agricultural_absolute_vals.rds")

pred_lin <- posterior_linpred(m3a, newdata = new_data2,re.form=~0)
dim(pred_lin)
pred_lin <- pred_lin+0.25
## whyare prediction for m3 from 1.44 to 2.15??? and not between 0 and 1
head(pred_lin)
colMeans(pred_lin)
### make function and plot predicted intervals
tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .05, prob_upr = .95) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_data_frame %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  df_pred
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(mean = mean(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(ling_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))
ldf$mean.rk[2] <- NA
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df


# temporary fix because predcited values are fucked up
data$mean<- data$mean
data$lower[0:5]<- 0
data$upper<- data$upper
## make plot 1

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot1 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=ling_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(ling_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(ling_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(ling_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = ling_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=ling_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Lingusitic distance", limits = c(-0.05, 1.05),breaks=c(0.2,0.8),
                     labels=c("Similar","Different")) + 
  scale_y_continuous(name="Predicted probability of returning", limits = c(-0.05, 1.05), breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1.0"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot1
#################################################
#################################################
#################################################
#################################################
# facet 2
x_rng <- range(d2$geog_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=x_steps,
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=mean(taxes_a))
detach(d2)

pred_lin <- posterior_linpred(m3a, newdata = new_data2,re.form=~0)
dim(pred_lin)

## whyare prediction for m3 from 1.42 to 2.72??? and not between 0 and 1
head(pred_lin)
colMeans(pred_lin)
pred_lin <- pred_lin+0.25
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(geog_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))

ldf <- ldf[1:15,]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)
data$upper[17:20] <- 1
data$lower[1] <- 0
## make plot 2

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot2 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=geog_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(geog_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(geog_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(geog_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = geog_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=geog_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Geographic distance", limits = c(-0.05, 1.05), breaks=c(0.25,0.75), labels=c("Near","Far")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot2

#################################################################
#################################################################
#################################################################
#################################################################
# facet 3
#x_rng <- range(d1$geog_dist_org_to_evacdest) 
#x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=c(0L,1L),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= c(0L,1L),                                        
  mbw =mean(mbw),                                             
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=mean(taxes_a))
detach(d2)




pred_lin <- posterior_linpred(m3a, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin+0.3
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(outbred, b = 2))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- c(1L,2L)

# plot both together  and select 95% CI 
p1 <-pred_lin[,1] 

p1 <-p1[p1 > quantile(p1,0.05) & p1 < quantile(p1,0.95)] %>% as.data.frame()
p1$outbred <- 0
p1$observation <- 1

p2 <-pred_lin[,2] 
p2 <-p2[p2 > quantile(p2,0.05) & p2 < quantile(p2,0.95)] %>% as.data.frame()
p2$outbred <- 1
p2$observation <- 2
p <- bind_rows(p1,p2)
names(p)[1] <- "predicted"

data <- p %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df


## make plot 3

library(tidyverse)
data$N <-data$n.rk



theme_set(theme_minimal())
plot3 <- ggplot(data = data, aes(x = factor(outbred), y = predicted)) +
  #geom_violin(fill = "dodgerblue",position="dodge") + 
  geom_violin(aes(fill="dodgerblue")) +
  geom_errorbar(aes(x=factor(outbred), ymin=(mean.rk-se.rk),ymax=(mean.rk+se.rk),size=0.15))+                                                                                                                                                                                                                                                                           
  geom_point(alpha=1, size=1, aes(x = factor(outbred), y = mean.rk)) +
  scale_shape_identity()+
  scale_x_discrete(name="Marriage", breaks = c(0, 1), labels=c("Married in","Married out")) + 
  
  
  
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  #scale_size_area(max_size = 2.5)+ 
  scale_size_continuous(name = "Mean and SE\n  (observed)", labels = NULL)+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),    # ,
        axis.text.x = element_text(size=12))
#axis.text.y = element_text(face="bold",size=12))

plot3


#################################################
#################################################
#################################################
#################################################
# facet 4
x_rng <- range(d2$taxes_a) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)


attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),
  education=mean(education),
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=x_steps)
detach(d2)

pred_lin <- posterior_linpred(m3a, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin+0.563037155
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(taxes_a, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))

ldf$mean.rk[20]<- NA

# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)




data$lower[17:20]<-0
data$upper[1:3]<-1
## make plot 2

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot4 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=taxes_a,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(taxes_a, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(taxes_a, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(taxes_a, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = taxes_a, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=taxes_a, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Tax rate", limits = c(-0.05, 1.05), breaks=c(0.2,0.8),
                     labels=c("Similar","Different")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.50,0.75,1.0),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot4


# facet 5 - education
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=c(0L,1L),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),
  education=c(0L,1L),
  heatsum_a=mean(heatsum_a),
  rainfall_a= mean(rainfall_a),
  clayperc_a =mean(clayperc_a),
  heightmedian_a=mean(heightmedian_a),
  lakeperc_a=mean(lakeperc_a),
  moraineperc_a=mean(moraineperc_a),
  peatperc_a=mean(peatperc_a),
  rockperc_a=mean(rockperc_a),
  farmedperc_a =mean(farmedperc_a),
  livestock100ha_a=mean(livestock100ha_a),
  density_a=mean(density_a),
  taxes_a=mean(taxes_a))
detach(d2)

pred_lin <- posterior_linpred(m3a, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin +0.5
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(education, b = 2))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- c(1L,2L)

# plot both together  and select 95% CI 
p1 <-pred_lin[,1] 

p1 <-p1[p1 > quantile(p1,0.05) & p1 < quantile(p1,0.95)] %>% as.data.frame()
p1$education <- 0
p1$observation <- 1

p2 <-pred_lin[,2] 
p2 <-p2[p2 > quantile(p2,0.05) & p2 < quantile(p2,0.95)] %>% as.data.frame()
p2$education <- 1
p2$observation <- 2
p <- bind_rows(p1,p2)
names(p)[1] <- "predicted"

data <- p %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df


## make plot 5

library(tidyverse)
data$N <-data$n.rk



theme_set(theme_minimal())
plot5 <- ggplot(data = data, aes(x = factor(education), y = predicted)) +
  #geom_violin(fill = "dodgerblue",position="dodge") + 
  geom_violin(aes(fill="dodgerblue")) +
  geom_errorbar(aes(x=factor(education), ymin=(mean.rk-se.rk),ymax=(mean.rk+se.rk),size=0.15))+                                                                                                                                                                                                                                                                           
  geom_point(alpha=1, size=1, aes(x = factor(education), y = mean.rk)) +
  scale_shape_identity()+
  scale_x_discrete(name="Education", breaks = c(0, 1), labels=c("Uneducated","Educated")) + 
  
  
  
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  #scale_size_area(max_size = 2.5)+ 
  scale_size_continuous(name = "Mean and SE\n  (observed)", labels = NULL)+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),    # ,
        axis.text.x = element_text(size=12))
#axis.text.y = element_text(face="bold",size=12))

plot5
##### Make facet grid
library(ggpubr)
Fig_2<- ggarrange(plot1, plot2, plot3,plot4,plot5,ncol=5,nrow=1,vjust=1,hjust=-4,common.legend = TRUE, 
                  font.label = list(size = 7, color = "black", face = "bold", family = NULL),
                  legend="right",labels=c("", "", ""))

Fig_2

setwd("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/Figures")
ggsave(Fig_2, filename = "Figure_2_nonfarmers_abs_values.png", width = 24, height = 4, device = "png", units = "in")

ggsave(Fig_2, filename = "Figure_2_nonfarmers_abs_values.pdf", width = 24, height = 4, device = "pdf", units = "in")

###########################################################################################
###########################################################################################
###########################################################################################
library("rstanarm")
library(tidyverse)
d <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/ecology_data2.rds")

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
#Key variables - different bad
d1 <- d %>% filter(agriculture==1)  # 9,870
d2 <- d %>% filter(agriculture==0)  # 12,204
###########################################################################################
###########################################################################################
###########################################################################################
setwd("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/Figures")
###########################################################################################
###########################################################################################
###########################################################################################

## Model 1 - farmers differences
# facet 1
m1 <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model1_new_agricultural_directional.rds")
x_rng <- range(d1$ling_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)

attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=x_steps, 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  #education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d1)


pred_lin <- posterior_linpred(m1, newdata = new_data2,re.form=~0)

pred_lin <- pred_lin-0.8
 

colMeans(pred_lin)
### make function and plot predicted intervals
tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .05, prob_upr = .95) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_data_frame %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  df_pred
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(mean = mean(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(ling_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))
ldf$mean.rk[2] <- NA
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
data$lower[1:2]<-0
data$upper[14:20]<- 1
data$mean[19:20]<- 1
## make plot 1

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot1 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=ling_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(ling_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(ling_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(ling_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = ling_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=ling_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Lingusitic distance", limits = c(-0.05, 1.05),breaks=c(0.2,0.8),
                     labels=c("Similar","Different")) + 
  scale_y_continuous(name="Predicted probability of returning", limits = c(-0.02, 1.02), breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1.0"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  #scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot1
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
# facet 2
x_rng <- range(d1$geog_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=x_steps,
  #ling_dist_org_to_evacdest                        
  #education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d1)


pred_lin <- posterior_linpred(m1, newdata = new_data2,re.form=~0)

## why are predictions for m1 not between 0 and 1

colMeans(pred_lin)
pred_lin <- pred_lin-0.8
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference
ldf <- d1  %>% mutate (l_bins = cut(geog_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))

ldf <- ldf[1:16,]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
data <- data[0:15,]


data$upper[9:15] <- 1
## make plot 2

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot2 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=geog_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(geog_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(geog_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(geog_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = geog_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=geog_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Geographic distance", limits = c(-0.05, 0.76), breaks=c(0.15,0.6), labels=c("Near","Far")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot2

#################################################################
#################################################################
#################################################################
#################################################################
# facet 3 - outbred

attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=c(0L,1L),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  #education= mean(education),
  outbred= c(0L,1L),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d1)


pred_lin <- posterior_linpred(m1, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin-0.8
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(outbred, b = 2))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- c(1L,2L)

# plot both together  and select 95% CI 
p1 <-pred_lin[,1] 

p1 <-p1[p1 > quantile(p1,0.05) & p1 < quantile(p1,0.95)] %>% as.data.frame()
p1$outbred <- 0
p1$observation <- 1

p2 <-pred_lin[,2] 
p2 <-p2[p2 > quantile(p2,0.05) & p2 < quantile(p2,0.95)] %>% as.data.frame()
p2$outbred <- 1
p2$observation <- 2
p <- bind_rows(p1,p2)
names(p)[1] <- "predicted"

data <- p %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df


## make plot 3
library(tidyverse)
data$N <-data$n.rk

theme_set(theme_minimal())
plot3 <- ggplot(data = data, aes(x = factor(outbred), y = predicted)) +
  #geom_violin(fill = "dodgerblue",position="dodge") + 
  geom_violin(aes(fill="dodgerblue")) +
  geom_errorbar(aes(x=factor(outbred), ymin=(mean.rk-se.rk),ymax=(mean.rk+se.rk),size=0.15))+                                                                                                                                                                                                                                                                           
  geom_point(alpha=1, size=1, aes(x = factor(outbred), y = mean.rk)) +
  scale_shape_identity()+
  scale_x_discrete(name="Marriage", breaks = c(0, 1), labels=c("Married in","Married out")) + 
  
  
  
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  #scale_size_area(max_size = 2.5)+ 
  scale_size_continuous(name='Mean and SE\n  (observed)',labels = NULL) +
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),    
        axis.text.x = element_text(size=12))
#axis.text.y = element_text(face="bold",size=12))

plot3

#################################################
#################################################
#################################################
#################################################

# make org taxes minus dest taxes a sequence of 20 numbers
#d1$org_taxes <- (d1$org_taxes-d1$dest_taxes)
x_rng <- range(d1$taxes_d) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)

attach(d1)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  #education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=x_steps)
detach(d1)


pred_lin <- posterior_linpred(m1, newdata = new_data2,re.form=~0)
dim(pred_lin)


colMeans(pred_lin)

pred_lin <- pred_lin-1
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d1  %>% mutate (l_bins = cut(taxes_d, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))


ldf <- ldf[c(4:15, 19:20), ]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df
head(data)


data$lower[1] <- 0
data$upper[16:20] <- 1.0

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot4 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=taxes_d,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(taxes_d, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(taxes_d, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(taxes_d, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = taxes_d, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=taxes_d, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Tax rate", limits = c(-1, 1.05), breaks=c(-0.6,0.6),
                     labels=c("Lower","Higher")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.50,0.75,1.0),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot4

##### Make facet grid
library(ggpubr)
Fig_3<- ggarrange(plot1, plot2, plot3,plot4,ncol=4,nrow=1,vjust=1,hjust=-4,common.legend = TRUE, 
                  font.label = list(size = 7, color = "black", face = "bold", family = NULL),
                  legend="right",labels=c("", "", ""))

Fig_3


ggsave(Fig_3, filename = "Figure_3_farmers_diffs.png", width = 20, height = 4, device = "png", units = "in")

ggsave(Fig_3, filename = "Figure_3_farmers_diffs.pdf", width = 20, height = 4, device = "pdf", units = "in")

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# Plot Model 1b - non-famers - directions
library("rstanarm")
library(tidyverse)

m1a <- readRDS("C:/Users/robert/Dropbox/Github/Migrations_and_ecology/model_results/Model1A_new_non_agricultural_directional.rds")
###########################################################################################
###########################################################################################
###########################################################################################
# facet 1
x_rng <- range(d2$ling_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)

attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=x_steps, 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d2)



pred_lin <- posterior_linpred(m1a, newdata = new_data2,re.form=~0)


colMeans(pred_lin)
pred_lin<-pred_lin+0.35
### make function and plot predicted intervals
tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .05, prob_upr = .95) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_data_frame %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  df_pred
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(mean = mean(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(ling_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))
ldf <- ldf[3:20,]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)

data$lower[1]<- 0

## make plot 1

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot1 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=ling_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(ling_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(ling_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(ling_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = ling_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=ling_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Lingusitic distance", limits = c(-0.05, 1.05),breaks=c(0.2,0.8),
                     labels=c("Similar","Different")) + 
  scale_y_continuous(name="Predicted probability of returning", limits = c(-0.02, 1.02), breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1.0"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot1
#################################################
#################################################
#################################################
#################################################
# facet 2
x_rng <- range(d2$geog_dist_org_to_evacdest) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=x_steps,
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d2)


pred_lin <- posterior_linpred(m1a, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin+0.30
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(geog_dist_org_to_evacdest, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))

ldf <- ldf[1:15,]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



### data is all the data -raw values and predicted values in same df
head(data)
#data$lower[14:20] <- 0
## make plot 2

library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot2 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=geog_dist_org_to_evacdest,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(geog_dist_org_to_evacdest, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(geog_dist_org_to_evacdest, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(geog_dist_org_to_evacdest, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = geog_dist_org_to_evacdest, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=geog_dist_org_to_evacdest, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Geographic distance", limits = c(-0.05, 0.78), breaks=c(0.2,0.6), labels=c("Near","Far")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.30), 
                     breaks=c(0,0.325,0.65,0.975,1.3),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot2

#################################################################
#################################################################
#################################################################
#################################################################
# facet 3
#x_rng <- range(d1$geog_dist_org_to_evacdest) 
#x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=c(0L,1L),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= c(0L,1L),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d2)

pred_lin <- posterior_linpred(m1a, newdata = new_data2,re.form=~0)
dim(pred_lin)


head(pred_lin)
colMeans(pred_lin)
pred_lin <- pred_lin +0.4
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(outbred, b = 2))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- c(1L,2L)

# plot both together  and select 95% CI 
p1 <-pred_lin[,1] 

p1 <-p1[p1 > quantile(p1,0.05) & p1 < quantile(p1,0.95)] %>% as.data.frame()
p1$outbred <- 0
p1$observation <- 1

p2 <-pred_lin[,2] 
p2 <-p2[p2 > quantile(p2,0.05) & p2 < quantile(p2,0.95)] %>% as.data.frame()
p2$outbred <- 1
p2$observation <- 2
p <- bind_rows(p1,p2)
names(p)[1] <- "predicted"

data <- p %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df


## make plot 3

library(tidyverse)
data$N <-data$n.rk



theme_set(theme_minimal())
plot3 <- ggplot(data = data, aes(x = factor(outbred), y = predicted)) +
  #geom_violin(fill = "dodgerblue",position="dodge") + 
  geom_violin(aes(fill="dodgerblue")) +
  geom_errorbar(aes(x=factor(outbred), ymin=(mean.rk-se.rk),ymax=(mean.rk+se.rk),size=0.15))+                                                                                                                                                                                                                                                                           
  geom_point(alpha=1, size=1, aes(x = factor(outbred), y = mean.rk)) +
  scale_shape_identity()+
  scale_x_discrete(name="Marriage", breaks = c(0, 1), labels=c("Married in","Married out")) + 
  
  
  
  scale_y_continuous(name="", limits = c(-0.01, 1.51), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  #scale_size_area(max_size = 2.5)+ 
  scale_size_continuous(name = "Mean and SE\n  (observed)", labels = NULL)+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),    # ,
        axis.text.x = element_text(size=12))
#axis.text.y = element_text(face="bold",size=12))

plot3

#################################################
#################################################
x_rng <- range(d2$taxes_d) 
x_steps <- seq(x_rng[1], x_rng[2], length.out = 20)


attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=seq_along(x_steps),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  education= mean(education),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=x_steps)
detach(d2)


pred_lin <- posterior_linpred(m1a, newdata = new_data2,re.form=~0)
dim(pred_lin)


colMeans(pred_lin)

pred_lin <- pred_lin +0.2
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(taxes_d, b = 20))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- seq.int(nrow(ldf))


ldf <- ldf[c(2:20), ]
# plot both together   

data <- df_pred_lin %>% left_join (ldf, by =c("observation"="observation"))



library(tidyverse)
data$N <-data$n.rk
theme_set(theme_minimal())
plot4 <-   data %>% 
  ggplot()+ 
  geom_ribbon(aes(x=taxes_d,ymin = lower, ymax = upper, fill = "dodgerblue")) + 
  geom_line(aes(taxes_d, upper), color = "grey30", size = 0.1) + 
  geom_line(aes(taxes_d, lower), color = "grey30", size = 0.1) +   
  geom_line(aes(taxes_d, mean, color = "navyblue"), size = 1)+
  
  geom_point(aes(x = taxes_d, y = mean.rk,size=N)) +
  
  
  geom_errorbar(aes(x=taxes_d, ymin=(mean.rk-se.rk),
                    ymax=(mean.rk+se.rk),size=N)) +
  #geom_point(aes(y = returnedkarelia)) + 
  scale_x_continuous(name="Tax rate", limits = c(-1, 1), breaks=c(-0.4,0.6),
                     labels=c("Lower","Higher")) + 
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.50,0.75,1.0),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  
  scale_color_manual( "Credibility Interval\n    (predicted)",
                      labels = c("97% CI"), values=c("navyblue"="navyblue"))+
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=1),
         size = guide_legend(order=2))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  scale_size_area(max_size = 2.5)+ 
  labs(size='Mean and SE\n  (observed)') +
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),
        axis.text.x=element_text(size=12))
plot4



# facet 5
attach(d2)
new_data2 <- data_frame(
  ling_dist_org_to_evacdest=mean(ling_dist_org_to_evacdest), 
  age = mean(age),
  sex=mean(sex),
  observation=c(0L,1L),
  geog_dist_org_to_evacdest=mean(geog_dist_org_to_evacdest),
  #ling_dist_org_to_evacdest                        
  education= c(0L,1L),
  outbred= mean(outbred),                                        
  mbw =mean(mbw),                                             
  heatsum_d=mean(heatsum_d),
  rainfall_d= mean(rainfall_d),
  clayperc_d =mean(clayperc_d),
  heightmedian_d=mean(heightmedian_d),
  lakeperc_d=mean(lakeperc_d),
  moraineperc_d=mean(moraineperc_d),
  peatperc_d=mean(peatperc_d),
  rockperc_d=mean(rockperc_d),
  farmedperc_d =mean(farmedperc_d),
  livestock100ha_d=mean(livestock100ha_d),
  density_d=mean(density_d),
  taxes_d=mean(taxes_d))
detach(d2)




pred_lin <- posterior_linpred(m1a, newdata = new_data2,re.form=~0)

colMeans(pred_lin)
pred_lin <- pred_lin + 0.46
### make function and plot predicted intervals

df_pred_lin <- tidy_predictions(pred_lin, new_data2)
df_pred_lin <- df_pred_lin %>% as.data.frame()


### get bins of raw data across all 20 values and 
#get the means and error bars
library(plotrix)
# make new dataframe with raw data for probability of return across
# 20 values of linguistic difference

#non_lotta_df <- ttr %>% filter(lotta==0) 
#lotta_df <- ttr %>% filter(lotta==1)
ldf <- d2  %>% mutate (l_bins = cut(education, b = 2))


ldf <- plyr::ddply(ldf,~l_bins,summarise,mean.rk = mean(returnedkarelia), sd.rk = sd(returnedkarelia),
                   se.rk=sd(returnedkarelia)/sqrt(length(returnedkarelia)),n.rk=length(returnedkarelia))

# make observation column
ldf$observation <- c(1L,2L)

# plot both together  and select 95% CI 
p1 <-pred_lin[,1] 

p1 <-p1[p1 > quantile(p1,0.05) & p1 < quantile(p1,0.95)] %>% as.data.frame()
p1$education <- 0
p1$observation <- 1

p2 <-pred_lin[,2] 
p2 <-p2[p2 > quantile(p2,0.05) & p2 < quantile(p2,0.95)] %>% as.data.frame()
p2$education <- 1
p2$observation <- 2
p <- bind_rows(p1,p2)
names(p)[1] <- "predicted"

data <- p %>% left_join (ldf, by =c("observation"="observation"))

### data is all the data -raw values and predicted values in same df


## make plot 5

library(tidyverse)
data$N <-data$n.rk



theme_set(theme_minimal())
plot5 <- ggplot(data = data, aes(x = factor(education), y = predicted)) +
  #geom_violin(fill = "dodgerblue",position="dodge") + 
  geom_violin(aes(fill="dodgerblue")) +
  geom_errorbar(aes(x=factor(education), ymin=(mean.rk-se.rk),ymax=(mean.rk+se.rk),size=0.15))+                                                                                                                                                                                                                                                                           
  geom_point(alpha=1, size=1, aes(x = factor(education), y = mean.rk)) +
  scale_shape_identity()+
  scale_x_discrete(name="Education", breaks = c(0, 1), labels=c("Uneducated","Educated")) + 
  
  scale_y_continuous(name="", limits = c(-0.01, 1.01), 
                     breaks=c(0,0.25,0.5,0.75,1),
                     labels=c("0","0.25","0.5","0.75","1"))+
  
  scale_fill_manual( "Credibility Interval\n    (predicted)",
                     labels = c("97% CI"), values=c("dodgerblue"="dodgerblue"))+
  
  #scale_size_area(max_size = 2.5)+ 
  scale_size_continuous(name = "Mean and SE\n  (observed)", labels = NULL)+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  
  theme(axis.title.x = element_text( face="bold",size=14),
        axis.title.y = element_text( face="bold",size=14),    # ,
        axis.text.x = element_text(size=12))
#axis.text.y = element_text(face="bold",size=12))

plot5
##### Make facet grid
library(ggpubr)
Fig_4<- ggarrange(plot1, plot2, plot3,plot4,plot5,ncol=5,nrow=1,vjust=1,hjust=-4,common.legend = TRUE, 
                  font.label = list(size = 7, color = "black", face = "bold", family = NULL),
                  legend="right",labels=c("", "", ""))

Fig_4


ggsave(Fig_4, filename = "Figure_4_nonfarmers_directions.png", width = 24, height = 4, device = "png", units = "in")

ggsave(Fig_4, filename = "Figure_4_nonfarmers_directions.pdf", width = 24, height = 4, device = "pdf", units = "in")

