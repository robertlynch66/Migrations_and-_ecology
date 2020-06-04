#The geographical distances between municipalities and the linguistic distances. 
#Geographical distances are planar straight-line distances between municipal population centres (in metres).
# Linguistic distances are rough equivalents of Seguys dialect distance metric, 
# i.e. a percentage of disagreeing linguistic features between pairs of municipal dialects. 

#In the distance matrices there is only the project ID of each municipality present so on 
#the third sheet I put which project ID matches with which municipality. 
#load_pops_from_googlesheets <- function() {
# Read in the csv files populations with social status
library(googlesheets)
library(dplyr)
(my_sheets <- gs_ls())
my_sheets %>% glimpse()
# get the populations google sheet
  # get the geographic distances google sheet
  geogdist <- gs_title("geo_dists.csv")
  # get the sheet
  geogdist <- gs_read(ss=geogdist, ws = "Geogrdist betw munic(in metres)", header=TRUE, sep= ",",row.names=1)
  # the main problem here is that the sheets need to be read in amatrices and must inlcude the 1st column as row numbers
  #key lines header = TRUE and row.names = (column) 1
  geogdist<-as.dist(geogdist) # turn the data into distances 
  geogmatx<-as.matrix(geogdist) #makes a distance matrix
  library("reshape")
  #geogdist1 <- melt(geogmatx)[melt(upper.tri(geogmatx))$value,]
  geogdist <- melt(geogmatx)
  names(geogdist) <- c("m1", "m2", "geog_dist") 
  
  
  # Read in lingusitic distances 
  # ling_dists.csv
  lingdist <- gs_title("ling_dists.csv")
  # get the sheet
  gs_ws_ls(lingdist)
  lingdist<-gs_read(ss=lingdist, ws="Sheet1", header=TRUE, sep= ",",row.names=1)
  lingdist<-as.dist(lingdist) # turn the data into distances 
  lingmatx<-as.matrix(lingdist) #makes a distance matrix
  library("reshape")
  #geogdist1 <- melt(geogmatx)[melt(upper.tri(geogmatx))$value,]
  lingdist <- melt(lingmatx)
  names(lingdist) <- c("m1", "m2", "ling_dist") 

  lingdist <- as.data.frame(lingdist)
 
  # read in names
  # "ProjID-munic.name matching"
  muninames <- gs_title("geo_dists.csv")
  #list the sheets
  gs_ws_ls(muninames)
  muninames <- gs_read(ss=muninames, ws = "ProjID-munic.name matching", header=TRUE, sep= ",")
  muninames <- as.data.frame(muninames)

library(dplyr)
# left join ids to ling and geog flattened matrices and add actual names
data <- geogdist %>% left_join(lingdist, by=c("m1"="m1","m2"="m2"))
data <- data %>% left_join(muninames, by=c("m1"="PROJECT_ID")) 

data <- plyr::rename(data, c("Municipality"="municipality_1"))

data <- data %>% left_join(muninames, by=c("m2"="PROJECT_ID")) 
data <- plyr::rename(data, c("Municipality"="municipality_2"))


# read in key variables
keyvars <- gs_title("key_variables.csv")
# get the sheet
gs_ws_ls(keyvars)
keyvars <- gs_read(ss=keyvars, ws = "Sheet 1", header=TRUE, sep= ",")
keyvars <- as.data.frame(keyvars)

# link key variables to 'project id'
keyvars$Municipality <- as.character(keyvars$Municipality)
muninames$Municipality <- as.character(muninames$Municipality)
keyvars <- keyvars %>% left_join(muninames, by=c("Municipality"="Municipality"))
keyvars$muni_id <- keyvars$PROJECT_ID
keyvars$PROJECT_ID <- NULL
# read in person _data one folder up

path <- "../data files/"
file<- "person_data.rds"
p <- readRDS(paste0(path, file))



## make a dummy variable for farm owners (conservative)
# \\< denotes beginnning of word and \\> denotes the end
p$farmers <- ifelse(grepl("\\<maanviljelijä\\>",p$profession),1,
                    ifelse(grepl("\\<emäntä\\>",p$profession),1,
                           ifelse(grepl("\\<pienviljelijä\\>",p$profession),1, 
                               ifelse(grepl("\\<maanvilj\\>",p$profession),1,0))))



# filter data to farmers

# select the variables you need

p <- p %>% select ("id","sex","primaryperson","birthyear","birthmunicipality","returnedkarelia","org_muni_karelia","dest_muni_finland",
                   "ret_muni_karelia","kids","birthregion","profession","farmers","farmtotalarea",
                   "weddingyear", "profession_en","agriculture","education","movesafter1945")


# filter data - all born in karelia and birthyears you want
p <- p %>% filter(birthregion == "karelia" & primaryperson==TRUE & birthyear<1940 & birthyear>1870)

# link person_data to bedlan data and matrices - only 36000 do we have the first destination in Finland
# link to key vars
# origin and destination
p$birthmunicipality <- tolower(p$birthmunicipality)
keyvars$Municipality <- tolower(keyvars$Municipality)
p <- p %>% left_join(keyvars, by=c("birthmunicipality"="Municipality"))%>% as.data.frame()
# rename variables
names(p) <- c("id","sex","primaryperson","birthyear","birthmunicipality","returnedkarelia","org_muni_karelia","dest_muni_finland",
              "ret_muni_karelia","kids","birthregion",  
              "profession","farmers","farmtotalarea","weddingyear","profession_en","agriculture","education","movesafter1945",
              "birth_muni_id","birth_area","birth_rainfall",
              "birth_lakeperc","birth__moraineperc","birth__riverperc","birth__clayperc","birth_org_gravelperc","birth__peatperc",
              "birth__rockperc",
              "birth__forestperc","birth__heatsum","birth__farmedperc","birth__fieldperc","birth__income","birth__livestock100ha",
              "birth__taxes",          
              "birth__pop1945","birth__heightrange","birth__heightmean","birth__heightmedian",       
              "birth__lat","birth__long","birth_municipal_id")
#p$birth_municipal_id <- p$birth_muni_id
#p$birth_muni_id <- NULL
## get origin variables
names(keyvars) <- c("municipality","org_muni_id","org_area","org_rainfall",
                 "org_lakeperc","org_moraineperc","org_riverperc","org_clayperc","org_gravelperc","org_peatperc","org_rockperc",
                 "org_forestperc","org_heatsum","org_farmedperc","org_fieldperc","org_income","org_livestock100ha","org_taxes",
                "org_pop1945","org_heightrange","org_heightmean","org_heightmedian",       
                 "org_lat","org_long","org_municipal_id")
#vars$x <- NULL
p$org_muni_karelia <- tolower(p$org_muni_karelia)
keyvars$municipality <- tolower(keyvars$municipality)
p <- p %>% left_join(keyvars, by=c("org_muni_karelia"="municipality"))
# get destination variables
# rename vars before joining
names(keyvars) <- c("municipality","dest_muni_id","dest_area","dest_rainfall",
"dest_lakeperc","dest_moraineperc","dest_riverperc","dest_clayperc","dest_gravelperc","dest_peatperc","dest_rockperc",
"dest_forestperc","dest_heatsum","dest_farmedperc","dest_fieldperc","dest_income","dest_livestock100ha","dest_taxes",          
"dest_pop1945","dest_heightrange","dest_heightmean","dest_heightmedian",       
"dest_lat","dest_long","dest_municipal_id")
#vars$x <- NULL
p$dest_muni_finland <- tolower(p$dest_muni_finland)
keyvars$municipality <- tolower(keyvars$municipality)
p <- p %>% left_join(keyvars, by=c("dest_muni_finland"="municipality"))

# get return destination variables
# rename vars before joining
names(keyvars) <- c("municipality","ret_muni_id","ret_area","ret_rainfall",
                 "ret_lakeperc","ret_moraineperc","ret_riverperc","ret_clayperc","ret_gravelperc","ret_peatperc","ret_rockperc",
                 "ret_forestperc","ret_heatsum","ret_farmedperc","ret_fieldperc","ret_income","ret_livestock100ha","ret_taxes",          
                 "ret_pop1945","ret_heightrange","ret_heightmean","ret_heightmedian",       
                 "ret_lat","ret_long","ret_municipal_id")
p$ret_muni_karelia <- tolower(p$ret_muni_karelia)
keyvars$municipality <- tolower(keyvars$municipality)
p <- p %>% left_join(keyvars, by=c("ret_muni_karelia"="municipality"))


# link the geo and lingusitic distnace matrices to p
# get ling distance between birthplace and evacuation place
# get all possible links

data$municipality_1 <- tolower(data$municipality_1)
data$municipality_2 <- tolower(data$municipality_2)
p <- p %>% left_join(data,by = c("org_muni_karelia"="municipality_1","dest_muni_finland"="municipality_2"))

p$muni_id <- NULL
p$m1 <- NULL
p$m2 <- NULL
colnames(p)[colnames(p)=="geog_dist"] <- "geog_dist_org_to_evacdest"
colnames(p)[colnames(p)=="ling_dist"] <- "ling_dist_org_to_evacdest"


p <- p %>% left_join(data,by = c("dest_muni_finland"="municipality_1","ret_muni_karelia"="municipality_2"))

p$muni_id <- NULL
p$m1 <- NULL
p$m2 <- NULL
colnames(p)[colnames(p)=="geog_dist"] <- "geog_dist_evacdest_to_retdest"
colnames(p)[colnames(p)=="ling_dist"] <- "ling_dist_evacdest_to_retdest"

write.csv(p,"ecology_data.csv")
# count missing data

saveRDS(p, "../data files/ecology_data.rds")


## read in data
ecology_data <- readRDS("C:/Users/rofrly/Dropbox/Github/data files/ecology_data.rds")