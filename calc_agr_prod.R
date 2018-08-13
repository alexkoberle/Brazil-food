
library(dplyr)

load( file = "POF7 personal consumption & nutrition intake - group.Rda" )

dat = a %>% ungroup() %>% select(item,grp,wgrp, qty_tot)



cereals = dat %>% filter(wgrp == "Cereals")



# Select and calculate rice primary consumption ---------------------------

rice_all = cereals %>% filter(grepl("ARROZ",item))

# first select already primary agricultural rice
rice_prim_cats = c("ARROZ INTEGRAL","ARROZ ORGANICO", "ARROZ  ORGANICO",
              "ARROZ INTEGRAL ORGANICO","ARROZ (POLIDO, PARBOILIZADO, AGULHA, AGULHINHA, ETC)" )

rice_raw = rice_all %>% filter(item %in% rice_prim_cats)

#then select cooked processed rice
rice_proc = rice_all %>% filter( !(item %in% rice_prim_cats) )

#define weight ratio of cooked to uncooked rice
cook_ratio = 1/3 # https://www.cooksinfo.com/rice

# calculate weight of agricultural rice in processed;cooked rice
rice_uncook = rice_proc %>% mutate(qty_tot = qty_tot * cook_ratio)

# gather
rice_prim = rbind(rice_raw, rice_uncook)

# sum weight of all agricultural rice# 
rice_qty = rice_prim %>% ungroup %>% select(qty_tot)  %>% colSums()


# Sort processed rice/dishes to identify common items
rice_dishes = unique(rice_proc$item)

rice_dishes_qty = aggregate(rice_proc, by = list("item"), FUN=sum)
                                                              