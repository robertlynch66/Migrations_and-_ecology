#The geographical distances between municipalities and the linguistic distances. 
#Geographical distances are planar straight-line distances between municipal population centres (in metres).
# Linguistic distances are rough equivalents of Seguys dialect distance metric, 
# i.e. a percentage of disagreeing linguistic features between pairs of municipal dialects. 

#In the distance matrices there is only the project ID of each municipality present so on 
#the third sheet I put which project ID matches with which municipality. 
# sheet names are:
#  munic_name_matching, ling_dist, geog_dist

# the main problem here is that the sheets need to be read in amatrices and must inlcude the 1st column as row numbers
#key lines header = TRUE and row.names = (column) 1
geogdist<-read.table("geog_dist.csv", sep= ",",header=TRUE, row.names=1) #read in the language data
geogdist<-as.dist(geogdist) # turn the data into distances 
geogmatx<-as.matrix(geogdist) #makes a distance matrix
library("reshape")
geogdist1 <- melt(geogmatx)[melt(upper.tri(geogmatx))$value,]
names(geogdist1) <- c("m1", "m2", "geog_dist") 

# get lingusitic distances
lingdist<-read.table("ling_dist.csv", sep= ",",header=TRUE, row.names=1) #read in the language data
lingdist<-as.dist(lingdist) # turn the data into distances 
lingmatx<-as.matrix(lingdist) #makes a distance matrix
library("reshape")
lingdist1 <- melt(lingmatx)[melt(upper.tri(lingmatx))$value,]
names(lingdist1) <- c("m1", "m2", "ling_dist") 

# match the names to the ids
muni_names <- read.table("name_matches.csv", sep=",",header=TRUE)
library(dplyr)
# left join ids to ling and geog flattened matrices and add actual names
data <- geogdist1 %>% left_join(lingdist1, by=c("m1"="m1","m2"="m2"))
data <- data %>% left_join(muni_names, by=c("m1"="id")) 

data <- plyr::rename(data, c("municipality"="municipality_1"))

data <- data %>% left_join(muni_names, by=c("m2"="id")) 
data <- plyr::rename(data, c("municipality"="municipality_2"))

saveRDS(data, "distance_matrices")