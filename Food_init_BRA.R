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
# gamsdir <- "../diet_gms/"

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


### Basic parameters
dris <- read.csv(paste0(datadir, "../DRI-India.csv"), header=TRUE)  # NEED TO ADJUST
cu_eqs <- read.csv(paste0(datadir, "../cu_eq.csv"), header=TRUE)  

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
  x = cu_eqs %>%
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


### POF data is not in DLE DB. Instead, it is under the survey drive as raw files.
# setwd("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/")
setwd("H:/MyDocuments/Analysis/Food/Brazil")
path_POF <- "P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Data/"
source("POF_data_read_in.R")
