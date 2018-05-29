###########################################
### Initialization + Import Parameters  ###
###########################################

options(java.parameters = "-Xmx96g")  # Arbitrarily set to 96GB; doesn't seem to matter if too high
source("P:/ene.general/DecentLivingEnergy/Surveys/Generic function to access database.R")

# Working directory is set by .Rprofile when loading this project
workdir <- paste0(getwd(), '/')
# datadir <- "../../../Data/Food-BRA/"
datadir <- "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/Food/Brazil/Mapping collaboration/"
localdatadir <- "H:/MyDocuments/Data/Food-BRA/"
gamsdir <- "../diet_gms/"

# Keep .gms file from other temporary intermediate files (.gdx and .log) for the sake of OneDrive (trying to synch all the files..)
# old_path <- "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/"   # Old diet_gms folder for keeping temp files
# interim_path <- "H:/MyDocuments/Analysis/Food/Brazil/GAMS_runfiles/"

#connect to database
# require(RColorBrewer)
require(foreign)
require(RJDBC)
require(readxl)
require(Hmisc)
require(rms)
require(doParallel)
require(data.table)
require(plyr)  # Needed by 'caret'; must be loaded before dplyr
require(dplyr)
require(tidyr)
require(caret)
require(gbm)
require(ggplot2)
require(gridExtra)
require(stringr)
require(stringi)
require(grid)
require(scales)
require(readxl)
require(xlsx)

# Brazilian states abbreviations
states.BR <- read.csv(paste0(datadir, "../States.csv"), header=TRUE, as.is=T) %>% select(abbr, state, Reg) %>% slice(-c(28:29))

### Basic parameters
dris <- read.csv(paste0(datadir, "../DRI-India.csv"), header=TRUE)  # NEED TO ADJUST
CU <- read.csv(paste0(datadir, "../cu_eq.csv"), header=TRUE)  

### Functions
# Function to return the household-specific nutrient DRIs given composition of age and adult/minor ###
getDRI= function(group, nutrient) {
  x = dris %>%
    filter(group==Group & nutrient==Nutrient) %>%
    select(DRI)
  return(as.numeric(x))
}

# Function that returns the cons eq of the hh member (male adult=1, fem ad=0.77, children=0.60)
getcu= function(group) {
  x = CU %>%
    filter(group==Group) %>%
    select(cu_eq)
  return(as.numeric(x))
}

# function returns the nutritional gap per cu-eq for a given nutrient and member type, where positive indicates intake deficiency
get_gap=function(group,nutrient,amount) {
  
  dri = getDRI(group, nutrient)
  consum = getcu(group)*amount
  x = (dri-consum)/dri
  
  return(x)
}

# is.nan for data.frame
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))


### POF data is not in DLE DB. Instead, it is under the survey drive as raw files.
# setwd("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/")
setwd(workdir)
path_POF <- "P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Data/"

### Read in POF data and set up the master table
source("POF_data_read_in.R")

  
### Groundwork for GAMS Analysis

# Derive nutritional content by key food item 
# Emission factor will be added here later (Alex)
nutri.content <- data.frame(food.master %>% ungroup() %>% group_by(item) %>% 
  summarise_at(vars(kcal:vita), funs(weighted.mean(., w=weight, na.rm=TRUE)))) %>% rename(energy=kcal) %>%
  mutate(ef_per_kg_eaten = runif(dim(.)[1])) %>% # Placeholder
  filter(item!="eatout or prepared")
nutri.content[is.nan(nutri.content)] <- 0

# Derive avg price per kg & consumption kg by cluster
# Only for households with eatout.share < outeat.threshold
avg.price.kg <- food.master %>% filter(!is.na(qty_tot)) %>% mutate(avg.price = val_tot/qty_tot) %>%
  filter(eatout.share < outeat.threshold) %>%
  group_by(cluster, item) %>% 
  summarise(avg.price=weighted.mean(avg.price, weight=qty_tot, na.rm=T), 
            avg.kg.per.cu=sum(qty_tot*weight)/sum(cu_eq*weight))%>% filter(item!="eatout or prepared") %>%
  left_join(nutri.content) %>% left_join(unique(food.group %>% select(item, group)))

# avg hh size (in cu) by cluster
# Only for households with eatout.share < outeat.threshold
hh_size_cls <- hh %>% group_by(cluster) %>% left_join(outeaters) %>% filter(!outeater) %>%
  summarise_at(vars(male_adult:male_minor, female_adult:female_minor), 
               funs(weighted.mean(., w=weight, na.rm = TRUE))) %>% rename(clsname=cluster)

# avg.price by cluster (wide format)
price_by_cls <-  avg.price.kg %>% select(cluster, item, avg.price) %>% arrange(cluster) %>%
  spread(key=cluster, value = avg.price) 
names(price_by_cls)[-1] <- paste0("price_", unique(hh_size_cls$clsname)) 

# avg.kg by cluster (wide format)
kg_by_cls <-  avg.price.kg %>% select(cluster, item, avg.kg.per.cu) %>% 
  spread(key=cluster, value = avg.kg.per.cu) 
names(kg_by_cls)[-1] <- paste0("kg_", unique(hh_size_cls$clsname)) 


# Generating variables and matching names to the India GAMS code
items_to_optimize <- nutri.content %>% select(item)

food_wgrp <- unique(food.group %>% select(group, item)) %>% filter(item!="eatout or prepared") 
grp_names <- unique(food_wgrp$group)
group_map <- data.frame(food_wgrp, grp=matrix(0, ncol=length(grp_names), nrow=dim(food_wgrp)[1]))
for(i in 1:length(grp_names)) {
  group_map[,2+i] <- as.numeric(group_map$group==grp_names[i])
}
group_map <- group_map %>% select(-group)
names(group_map)[-1] <- grp_names

ef_all <- nutri.content %>% select(item, ef_per_kg_eaten) 

# Need to ignore items with zero consumption from the optimization
ignore <- kg_by_cls # same dimension
ignore[,-1] <- 0

cal.share <- avg.price.kg %>% left_join(food_wgrp) %>% mutate(kcal.tot=energy*avg.kg.per.cu*10) %>% group_by(cluster, group) %>%
  mutate(kcal.share=kcal.tot/sum(kcal.tot)) %>% 
  select(cluster, item, group, kcal.tot, kcal.share) %>%
  arrange(cluster, group, item) %>% ungroup()
cal.share <- cal.share %>% select(cluster, item, kcal.share) %>% arrange(cluster) %>%
  spread(key=cluster, value = kcal.share) 
idx_nocon <- which(cal.share[,-1]<0.01 | is.na(cal.share[,-1]), arr.ind = TRUE)

ignore[,-1][idx_nocon] <- 1
names(ignore)[-1] <- paste0("ign_", unique(hh_size_cls$clsname))

# We need to finalize the group names and change gms files too (fg_opt and fg_oth) before running.
# Also need to update DRI in .gms file too
RunFoodOpt("tc_min", nutri=nutri.content)
