### 
### Estimate average intake and deficiency by population group (region/urban/income)
###

# based on NRao's code for India (Food analysis-byStatewIncome_withCU.R)
# This code takes the food consumption data from the Oracle DB (shown below), right now for Brazizl,
# and calculates the macro- and micro-nutrient content of each food item and creates a summary by state/urb-rur of totals

# Run after POF_data_read_in.R

### Summarize over each household

### Get total daily average intakes by household
# qty_tot in kg
# Derive intake per day per hh for the mapped items, and identify the unmapped val_tot
# without outeaters
sum.hh <- 
  # food.master %>%
  food.master.excl.outeater %>%
  mutate_at(vars(kcal:vita), funs(tot=.*qty_tot*10/365)) %>% group_by(id, mapped) %>% 
  # left_join(outeaters) %>% filter(!outeater) %>%
  summarise_at(vars(ends_with("_tot")), sum, na.rm=TRUE) %>% select(-qty_tot)

# Estimate nutrition intake from unmapped food (scale based on val_tot)
# Note: It appears that without this step of extrapolation, mean kcal estimates are much closer to IBGE means.
# Thus, I may need to skip line 27-34.
# estim.unmapped <- (sum.hh %>% filter(mapped==1) %>% 
#                      left_join(sum.hh %>% filter(mapped==0) %>% select(id, val=val_tot))) %>% 
#   mutate_at(vars(kcal_tot:vita_tot), funs(.*val/val_tot)) %>% 
#   select(-val_tot) %>% rename(val_tot=val) %>% mutate(mapped=0)
# 
# # Summarize the total intake and derive per cu amount
# sum.hh <- sum.hh %>% filter(mapped==1) %>% rbind(estim.unmapped) %>% arrange(id) %>% group_by(id)

sum.hh <- sum.hh %>% summarise_all(sum, na.rm=TRUE) %>% 
  # left_join(hh.excl.outeater) %>%
  left_join(hh) %>%
  data.frame() %>% 
  mutate_at(vars(kcal_tot:vita_tot), funs(percu=./cu_eq, percap=./hh_size)) 


### Derive gap sizes for each household
gap_threshold <- 0
gap.hh <- sum.hh %>% # Daily intake/CU 
  
  mutate(cal_gap_ma=get_gap("male_adult", "calorie", kcal_tot_percu),
         cal_gap_fa=get_gap("female_adult", "calorie", kcal_tot_percu),
         cal_gap_mm=get_gap("male_minor", "calorie", kcal_tot_percu),
         cal_gap_fm=get_gap("female_minor","calorie", kcal_tot_percu)) %>%
  mutate(cal_gap=(cal_gap_ma*male_adult+cal_gap_fa*female_adult+cal_gap_mm*male_minor+cal_gap_fm*female_minor)/hh_size, # avg gap per hh member
         cal_def=ifelse(cal_gap>gap_threshold, 1, 0),
         cal_surp=ifelse(cal_gap<0, 1, 0)) %>%
  
  mutate(zinc_gap_ma=get_gap("male_adult", "zinc", zinc_tot_percu),
         zinc_gap_fa=get_gap("female_adult", "zinc", zinc_tot_percu),
         zinc_gap_mm=get_gap("male_minor", "zinc", zinc_tot_percu),
         zinc_gap_fm=get_gap("female_minor", "zinc", zinc_tot_percu)) %>%
  mutate(zinc_gap=(zinc_gap_ma*male_adult+zinc_gap_fa*female_adult+zinc_gap_mm*male_minor+zinc_gap_fm*female_minor)/hh_size, # avg gap per hh member
         zinc_def=ifelse(zinc_gap>gap_threshold, 1, 0),
         zinc_surp=ifelse(zinc_gap<0, 1, 0)) %>%
  
  mutate(iron_gap_ma=get_gap("male_adult", "iron", iron_tot_percu),
         iron_gap_fa= get_gap("female_adult", "iron", iron_tot_percu),
         iron_gap_mm=get_gap("male_minor", "iron", iron_tot_percu),
         iron_gap_fm=get_gap("female_minor", "iron", iron_tot_percu)) %>%
  mutate(iron_gap=(iron_gap_ma*male_adult+iron_gap_fa*female_adult+iron_gap_mm*male_minor+iron_gap_fm*female_minor)/hh_size, # avg gap per hh member
         iron_def=ifelse(iron_gap>gap_threshold, 1, 0),
         iron_surp=ifelse(iron_gap<0, 1, 0)) %>%
  
  mutate(protein_gap_ma=get_gap("male_adult", "protein", protein_tot_percu),
         protein_gap_fa= get_gap("female_adult", "protein", protein_tot_percu),
         protein_gap_mm=get_gap("male_minor", "protein", protein_tot_percu),
         protein_gap_fm=get_gap("female_minor", "protein", protein_tot_percu)) %>%
  mutate(protein_gap=(protein_gap_ma*male_adult+protein_gap_fa*female_adult+protein_gap_mm*male_minor+protein_gap_fm*female_minor)/hh_size, # avg gap per hh member
         protein_def=ifelse(protein_gap>gap_threshold, 1, 0),
         protein_surp=ifelse(protein_gap<0, 1, 0)) %>%
  
  mutate(vita_gap_ma=get_gap("male_adult", "vita", vita_tot_percu),
         vita_gap_fa= get_gap("female_adult", "vita", vita_tot_percu),
         vita_gap_mm=get_gap("male_minor", "vita", vita_tot_percu),
         vita_gap_fm=get_gap("female_minor", "vita", vita_tot_percu))%>%
  mutate(vita_gap=(vita_gap_ma*male_adult+vita_gap_fa*female_adult+vita_gap_mm*male_minor+vita_gap_fm*female_minor)/hh_size, # avg gap per hh member
         vita_def=ifelse(vita_gap>gap_threshold, 1, 0),
         vita_surp=ifelse(vita_gap<0, 1, 0))


### Gap size and count by nutrient for each pop group
cal.gap.group <- gap.hh %>% group_by(cluster, cal_def) %>% summarise(cal_gap_avg = weighted.mean(cal_gap, hh_size*weight, na.rm=TRUE),
                                                                     cal_def_ct = sum(hh_size*weight, na.rm=TRUE)) %>%
    gather(variable, value, cal_gap_avg:cal_def_ct) %>% unite(temp, cal_def, variable) %>% spread(temp, value) %>% 
    setNames(c("cluster", "cal_surp_ct", "cal_surp_avg", "cal_def_ct", "cal_def_avg"))
  
prtn.gap.group  <- gap.hh %>% group_by(cluster, protein_def) %>% summarise(protein_gap_avg = weighted.mean(protein_gap, hh_size*weight, na.rm=TRUE),
                                                                     protein_def_ct = sum(hh_size*weight, na.rm=TRUE)) %>%
    gather(variable, value, protein_gap_avg:protein_def_ct) %>% unite(temp, protein_def, variable) %>% spread(temp, value) %>% 
    setNames(c("cluster", "protein_surp_ct", "protein_surp_avg", "protein_def_ct", "protein_def_avg"))

iron.gap.group <- gap.hh %>% group_by(cluster, iron_def) %>% summarise(iron_gap_avg = weighted.mean(iron_gap, hh_size*weight, na.rm=TRUE),
                                                                     iron_def_ct = sum(hh_size*weight, na.rm=TRUE)) %>%
    gather(variable, value, iron_gap_avg:iron_def_ct) %>% unite(temp, iron_def, variable) %>% spread(temp, value) %>% 
    setNames(c("cluster", "iron_surp_ct", "iron_surp_avg", "iron_def_ct", "iron_def_avg"))
  
zinc.gap.group <- gap.hh %>% group_by(cluster, zinc_def) %>% summarise(zinc_gap_avg = weighted.mean(zinc_gap, hh_size*weight, na.rm=TRUE),
                                                                     zinc_def_ct = sum(hh_size*weight, na.rm=TRUE)) %>%
    gather(variable, value, zinc_gap_avg:zinc_def_ct) %>% unite(temp, zinc_def, variable) %>% spread(temp, value) %>% 
    setNames(c("cluster", "zinc_surp_ct", "zinc_surp_avg", "zinc_def_ct", "zinc_def_avg"))
  
vita.gap.group <- gap.hh %>% group_by(cluster, vita_def) %>% summarise(vita_gap_avg = weighted.mean(vita_gap, hh_size*weight, na.rm=TRUE),
                                                                     vita_def_ct = sum(hh_size*weight, na.rm=TRUE)) %>%
    gather(variable, value, vita_gap_avg:vita_def_ct) %>% unite(temp, vita_def, variable) %>% spread(temp, value) %>% 
    setNames(c("cluster", "vita_surp_ct", "vita_surp_avg", "vita_def_ct", "vita_def_avg"))
  


# Deficiency and average intake by group (region, urban, income) 
# sum.group <- sum.hh %>% group_by(cluster) %>% summarise_at(vars(kcal_tot:vita_tot), funs(sum(.*weight/sum(weight*cu_eq)))) %>%
#   left_join(sum.hh %>% group_by(cluster) %>% summarise(total_pop=sum(hh_size*weight)))
# This has Weighted per-cu intake/day, total population by group, too.
gap.group <- gap.hh %>% group_by(cluster) %>% summarise_at(vars(kcal_tot_percu:vita_tot_percap), funs(sum(.*weight/sum(weight)))) %>%
  left_join(gap.hh %>% group_by(cluster) %>% summarise(total_pop=sum(hh_size*weight), total_cu=sum(cu_eq*weight))) %>%  # 
  left_join(cal.gap.group) %>% left_join(prtn.gap.group) %>% 
  left_join(iron.gap.group) %>% left_join(zinc.gap.group) %>% left_join(vita.gap.group) 

write.xlsx(gap.group, "summary.cluster_hh.excl.outeater.xlsx") # with hh.excl.outeater
# write.xlsx(gap.group, "summary.cluster_hh.xlsx") # with hh
gap.tot <- gap.hh %>% summarise_at(vars(kcal_tot_percu:vita_tot_percap), funs(sum(.*weight/sum(weight)))) %>% 
  mutate(total_pop=gap.hh %>% summarise(total_pop=sum(hh_size*weight)))
gap.urban.rural <- gap.hh %>% group_by(urban) %>% summarise_at(vars(kcal_tot_percu:vita_tot_percap), funs(sum(.*weight/sum(weight)))) %>% 
  mutate(total_pop=gap.hh %>% group_by(urban) %>% summarise(total_pop=sum(hh_size*weight)))
gap.region <- gap.hh %>% group_by(Reg) %>% summarise_at(vars(kcal_tot_percu:vita_tot_percap), funs(sum(.*weight/sum(weight)))) %>% 
  mutate(total_pop=gap.hh %>% group_by(Reg) %>% summarise(total_pop=sum(hh_size*weight)))
write.table(gap.tot, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

a <- food.tbl %>%
  left_join(data.table(hh, key="id"), by="id") %>% data.frame() %>%
  left_join(outeaters, by="id") %>% 
  group_by(cluster) %>% summarise(eo.share=weighted.mean(eatout, val_tot*weight, na.rm=TRUE))

a <- a %>% left_join(hh %>% left_join(outeaters) %>% group_by(cluster) %>% 
  summarise(eo.pop=weighted.mean(outeater, hh_size*weight, na.rm=TRUE),
              # sum(outeater*hh_size*weight)/sum(hh_size*weight),
            tot.pop=sum(hh_size*weight)))
                                  
