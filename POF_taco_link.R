# This is run from POF_data_read_in.R


### Function to find out where each POF code belongs to
get.idx <- function(y) {
  idx <- which(sapply(POF.mapping, function(x) {y %in% x}))
  return(idx)
}



### 1. Read in Claudia's mapping
# Nutrition info is based on "Taco_4a_edicao_2011 - TACO.xls" (2011)
# POF.nutri <- read_xlsx(paste0(datadir,"171023 Nutricional values food POF2008-9 - Claudia.xlsx"), 
#                        range = "A4:J109", 
#                        col_types=c("text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) 
# names(POF.nutri) <- c("item.prt", "item.eng", "POF.num", "cons.percap", "kcal", "protein", "iron", "zinc", "vita", "taco.num")
# POF.nutri <- POF.nutri %>% mutate(item.num=row_number()) %>% select(item.num, everything()) 

POF.nutri <- read_xlsx(paste0(datadir,"171023 Nutricional values food POF2008-9 - Claudia.xlsx"), sheet=2,
                       range = "A4:O109", 
                       col_types=c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) 
names(POF.nutri) <- c("fgroup.agg", "fgroup", "item.prt", "item.eng", "POF.num", "cons.percap", 
                      "kcal", "protein", "fat", "carb", "fiber", "iron", "zinc", "vita", "taco.num")
POF.nutri <- POF.nutri %>% mutate(item.num=row_number()) %>% select(item.num, everything()) 

# Mapped POF
POF.mapping <- strsplit(POF.nutri$POF.num, c("[,'´`]"))
POF.mapping <- lapply(POF.mapping, function(x) {as.numeric(x[-1])})
names(POF.mapping) <- POF.nutri$item.eng

# List of POF item numbers (7 & 5 digits) covered by Claudia's mapping
POF.covered <- unlist(POF.mapping)
POF.covered5 <- floor(POF.covered/100)

POF7.items <- data.frame(item.eng=names(POF.covered), code7=POF.covered) %>% mutate(item.eng=gsub("\\d", "", item.eng)) %>% 
  left_join(POF.nutri %>% select(fgroup.agg, item.eng)) %>% rename(grp=item.eng, wgrp=fgroup.agg)

# Rearrange in terms of POF item number
POF.nutri.p <- data.frame(code7 = POF.covered, code5 = POF.covered5, item.num=sapply(POF.covered, get.idx)) %>% 
  left_join(POF.nutri %>% select(item.num, item.eng, kcal:taco.num)) %>% arrange(code7) %>%
  select(-item.num, -taco.num)
# POF.nutri.p <- unique(POF.nutri.p %>% select(-code7))

# Assign nutritional info for major missing items
# Values for 69003 to 69002/69087 (Crystal sugar) and 69086/69001 (Refined sugar)
# 65010 (wheat flour) from #35 (Farinha, de trigo) of Taco_4a_edicao_2011
POF.nutri.p <- POF.nutri.p %>%
  rbind(POF.nutri.p %>% filter(code5==69003) %>% slice(1) %>% mutate(item.eng="crystal sugar", code5=69002, code7=NA)) %>%
  rbind(POF.nutri.p %>% filter(code5==69003) %>% slice(1) %>% mutate(item.eng="crystal sugar", code5=69087, code7=NA)) %>%
  rbind(POF.nutri.p %>% filter(code5==69003) %>% slice(1) %>% mutate(item.eng="refined sugar", code5=69001, code7=NA)) %>%
  rbind(POF.nutri.p %>% filter(code5==69003) %>% slice(1) %>% mutate(item.eng="refined sugar", code5=69086, code7=NA)) %>%
  rbind(data.frame(code7=NA, code5=65010, item.eng="wheat flour", kcal=360, protein=9.8, fat=1.4, carb=75.1, fiber=2.3, iron=1.0, zinc=0.8, vita=NA)) 



### 2. TACO (2008-2009) based on liv5002.pdf
taco.raw <- read_xls(paste0(datadir, "Tables of Nutritional Composition of Food - IBGE liv50002.xls"), skip=3, n_max=1971) 
taco <- taco.raw %>% select(1:3, 7:13, 15:18, 21, 23:24, 32:34, 42:43) 
names(taco) <- c("code7", "item", "prep.type", "kcal", "Protein", 
                 "Fat", "Carb", "Fiber", "Calcium", "Magnesium", "Phosphorus", "Iron", 
                 "Sodium", "Sodium.added", "Zinc", 
                 "Retinol", "Retinol.eq", "Vitd", "Vite","Vitc", "Sugar.total", "Sugar.added")
taco <- taco %>% mutate(code5 = floor(code7/100)) %>% 
  mutate_at(vars(kcal:Sugar.added), as.numeric) %>%
  mutate(Vita=ifelse(is.na(Retinol.eq), Retinol, Retinol.eq)) %>% select(-Retinol, -Retinol.eq) %>% ungroup() 
  # rowwise() %>% mutate(vita=sum(retinol, retinol.eq, na.rm = TRUE)) %>% select(-retinol, -retinol.eq) %>% ungroup() 

# Let's remove unnecessary preparation types

# Items with only one observation or multiple obs with CRU (preperation=1)
# - Choose CRU (1) if there are both 1 and 99
taco.CRU <- taco %>%
  group_by(code7) %>% mutate(n_preps = n()) %>% 
  filter(n_preps==1 | (n_preps>1 & preparation==1) | (n_preps>1 & preparation==99)) %>% 
  mutate(n_preps = n()) %>% slice(1) %>% select(-n_preps) 

# Item numbers having multiple obs but preperation!=1 or 99
no.CRU.items <- unique(taco$code7)[which((unique(taco$code7) %in% unique(taco.CRU$code7))==FALSE)]

taco.CRU <- taco.CRU %>% 
  # Then I select whatever first obs from the no.CRU group
  rbind(taco %>% filter(code7 %in% no.CRU.items) %>% group_by(code7) %>% slice(1)) %>% #
  arrange(code7) %>% select(code7, code5, everything()) %>% ungroup()

