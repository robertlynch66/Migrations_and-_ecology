#The geographical distances between municipalities and the linguistic distances. 
#Geographical distances are planar straight-line distances between municipal population centres (in metres).
# Linguistic distances are rough equivalents of Seguys dialect distance metric, 
# i.e. a percentage of disagreeing linguistic features between pairs of municipal dialects. 

#In the distance matrices there is only the project ID of each municipality present so on 
#the third sheet I put which project ID matches with which municipality. 
# sheet names are:
#  munic_name_matching, ling_dist, geog_dist

library("openxlsx")
g <- read.xlsx("geog_ling_dists.xlsx", sheet = "geog_dist", startRow = 1, colNames = TRUE)
#I also found that I've turned the matrices to vectors with 'melt' function in 'reshape' package
#I had used it like this: 
library("reshape")
langdist1 <- melt(g)[melt(upper.tri(g))$value,]
names(langdist1) <- c("c1", "c2", "lang_dist") 

# change git repository