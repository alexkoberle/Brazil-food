##################
### POF Data read-in for food analysis.
### Note: The first non-food part is adapted from 
###       P:\ene.general\DecentLivingEnergy\Surveys\Brazil\POF 2008-2009\Processed\02 POF 2008-2009 Data processing script.R
##################

library(readxl)
library(data.table)
library(stringr)
library(dplyrExtras)

setup.scripts = list.files("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/", pattern="*.R$", full.names=T, ignore.case=T)

survey.code = "BRA0"
survey.name = "BRA POF 2008-2009"

source(setup.scripts[[1]])
source(setup.scripts[[2]]) # load functions like genvar
source(setup.scripts[[3]]) 

# Function to convert character vectors in data frame to numeric (via type.convert), when possible
char2num = function(d) {
  d[] = lapply(d, function(x) if (class(x)=="character") type.convert(x, as.is=T) else x)  # Convert character to numeric, if possible
  return(d)
}

# Excel file linking survey's item codes to item names
ce_code = read_excel("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/BRA POF 2008-2009 CE Codes.xlsx") %>% data.table(key="code") %>%
  mutate(code7=as.numeric(code)) %>% filter(main=="Food and beverage")
# Excel file linking state numeric codes to state name
geo_code = read_excel("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/BRA POF 2008-2009 Geographic Codes.xlsx")

# File path to .RData object containing monthly PPP adustment factors for desired base year
# Extract uniform PPP adjustment factor to apply to all values
# Annualized values in survey already inflated/deflated to January 2009 prices in local currency
ppp_path = "P:/ene.general/DecentLivingEnergy/Surveys/Consumer Prices/Monthly PPP adjustment factors for base year 2010.RData"
ppp_fact = filter(readRDS(ppp_path), iso3=="BRA", year==2009, month==1)$ppp_fact



### Load raw POF data
load(paste0(path_POF, "t_caderneta_despesa_s.rda"))
load(paste0(path_POF, "t_despesa_individual_s.rda"))
load(paste0(path_POF, "codigos de alimentacao.rda"))
load(paste0(path_POF, "t_domicilio_s.rda"))
load(paste0(path_POF, "poststr.rda"))
load(paste0(path_POF, "t_morador_s.rda"))
load(paste0(path_POF, "t_despesa_individual_s.rda"))
# load(paste0(path_POF, "t_consumo_s.rda"))
# load(paste0(path_POF, "t_despesa_90dias_s.rda"))
# load(paste0(path_POF, "t_despesa_12meses_s.rda"))
# load(paste0(path_POF, "t_despesa_veiculo_s.rda"))
# load(paste0(path_POF, "t_outras_despesas_s.rda"))  # Other expenditures
# load(paste0(path_POF, "t_aluguel_estimado_s.rda")) # Other expenditures


d = char2num(t_domicilio_s)
post = char2num(poststr)

# Information used to define 'estrato' variable values that identify rural housholds
estrato.cats <- c( 7 , 3 , 9 , 3 , 9 , 4 , 6 , 13 , 10 , 24 , 9 , 10 , 16 , 9 , 8 , 22 , 28 , 10 , 31 , 31 , 19 , 14 , 19 , 9 , 11 , 18 , 8 )
names(estrato.cats) <- unique(d$cod_uf)

d1 = d %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(control=as.integer(paste0(cod_uf, str_pad(num_seq,3,pad=0), num_dv))) %>%  # Variable used to merge with post-stratification object
  left_join(post) %>%  # Join information from the post-stratification table
  rename(weight=fator_expansao2, income=renda_total) %>%  # fator_expansao2 is identical to post-stratifying weights using Census 2010 data
  mutate(income=income*12) %>%  # Original income variable is monthly HH income; this converts to annual
  mutate(date_int=as.Date("2008-05-19")+(perd_cod_p_visit_realm_em-1)*7) %>% # Raw variable gives number of weeks beginning May 19 2008
  left_join(geo_code) %>%   # cod_uf is numeric link to state name
  rename(state=reg1) %>%
  mutate(urban=ifelse(estrato >= estrato.cats[match(cod_uf, names(estrato.cats))] , 0 , 1 ))  %>%
  select(id, weight, date_int, state, urban, income)



### Process person data
d2 = char2num(t_morador_s) %>%
  # mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(cid=paste0(id,cod_unid_consumo)) %>%
  mutate(pid=paste0(id,cod_unid_consumo,num_informante)) %>%
  rename(weight=fator_expansao2, age=idade_anos, pheight=altura_imputado, pweight=peso_imputado) %>%
  genvar(male, cod_sexo, c(1,0), c(1,2)) %>%
  genvar(rel, cod_rel_pess_refe_uc, c('Head','Spouse','Child','Other relative','Other non-relative','Tenant','Domestic employee','Relative of domestic employee'), 1:8) %>%
  genvar(head, rel, T, "Head", other=F) %>%
  genvar(race, cod_cor_raca, c('White','Black','Yellow','Brown','Indigenous','Do not know'), c(1:5,9)) %>%
  genvar(educ_level, cod_nivel_instr, c('Nursery school','Preschool','Child literacy class',
                                        'Adult literacy class','Old primary','Old gymnasium',
                                        'Old classic, scientific','Regular basic education',
                                        'Young and adult education for elementary school',
                                        'Regular high school','Young and adult education for high school',
                                        'Technological college','Pre-college','Undergraduate',
                                        'Professional degree','Masters or doctorate'), 1:16) %>%
  left_join(educ_years) %>% # Add years of schooling completed
  mutate(student=as.integer(cod_curso_freq>0)) %>%
  mutate(earner=as.integer(cod_sit_receita==1)) %>%
  data.table(key = "id") %>%
  select(id, cid, pid, weight, head, rel, male, age, race, educ_level, educ_years, student, earner, pheight, pweight)



### Create household summary variables from person records
pp = d2[,list(
  hh_size = .N,
  male = male[head],
  age = age[head],
  race = race[head],
  educ_level = educ_level[head],
  # educ_years = educ_years[head],
  minor = sum(age<18), # Number of minors (less than 18 years old)
  # student = sum(student),
  # earner = sum(earner),
  male_adult = sum(age>=18 & male==1),
  male_minor = sum(age<18 & male==1),
  age_adult = mean(age[age>=18]),  # Mean age of people over 18 (can be NA in case of minor head of household)
  age_minor = mean(age[age<18])  # Mean age of people under 18 (will be NA if no minors in household)
), by=id]

# If multiple household heads specified, retain information for eldest head only (male if age is a tie)
#check = pp[,list(n = .N), by=id] %>% filter(n>1)
pp = pp %>%
  arrange(id, -age, -male) %>%
  unique(by="id")

#--------------------------
#--------------------------

### Combine household level variables
hh = Reduce(function(...) left_join(...), list(d1,pp)) 
#--------------------------
#--------------------------



### From here, food-specific data are imported.
source("POF_taco_link.R")  # derives as result 'POF.nutri.p' which has POF-TACO mapping

# All POF Food consumption records
# First join nutri data based on code7
# All quantity unit in kg
food = char2num(t_caderneta_despesa_s) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(code7=as.numeric(paste0(prod_num_quadro_grupo_pro, str_pad(cod_item,width=5,pad=0)))) %>%
  filter(code7 < 8600000) %>%
  mutate(period=7, kg=quant_kg, unit="kg") %>%  # Food consumption reported in one-week diary; all quantities are kg
  mutate(value=val_despesa_corrigido*ppp_fact*365/period) %>%  # Inflation-adjusted value (Jan 2009) converted to $PPP in specified base year
  mutate(kg=kg*365/period) %>%    # Derive annual kg consumption
  mutate_cond(kg==0, kg=NA) %>%   # This ensures that NA is returned for item where quantities are not possible
  select(id:value, -period) %>% 
  group_by(id, code7) %>%  
  summarise(val_tot=sum(value), qty_tot=sum(kg)) %>%  # Sum values by household and item
  filter(val_tot>0 | is.na(val_tot)) %>%
  left_join(taco %>% select(-preparation, -code5), by="code7") %>%
  mutate(code5=floor(code7/100)) %>%
  rename(POF.item=item)

# Nutritional table based on code5 (from Claudia)
a <- unique(POF.nutri.p %>% select(-code7)) %>% group_by(code5) %>% mutate(count=n()) %>% arrange(code5, desc(kcal)) %>% slice(1) 

# Map unmapped food consumption items based on code5
food.unmapped <- food %>% filter(is.na(kcal)) %>% select(id:qty_tot, code5) %>% left_join(a %>% select(-count), by="code5") 

# Merge all food obs (at home) and add eatout flag
food.tbl <- food %>% filter(!is.na(kcal)) %>% select(id, code7, code5, POF.item, val_tot, qty_tot, kcal:vita) %>%
  rbind(food.unmapped %>% select(id, code7, code5, POF.item=item.eng, val_tot, qty_tot, kcal:vita)) %>% 
  arrange(id, code7) %>%
  mutate(eatout=ifelse(floor(code5/1e3)==85, 1, 0)) %>%  # Add to-go (viagem) food as eatout
  data.table(key=c("id", "code7", "code5")) 



### Combining eatout or prepared food from 'other consumption' (from POF "individual" table)
# This is covering the whole hh of POF.
# Food items here are anything consumed outside home.
# No mapping exists for these items.
cons = char2num(t_despesa_individual_s) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(code7=as.numeric(paste0(str_pad(num_quadro,width=2,pad=0), str_pad(cod_item,width=5,pad=0)))) %>%
  mutate(period=365/fator_anual) %>%
  mutate(value=val_despesa_corrigido*ppp_fact*365/period) %>%  # Inflation-adjusted value (Jan 2009) converted to $PPP in specified base year
  group_by(id, code7) %>%
  summarise(val_tot=sum(value)) %>%  # Sum and annualize values
  filter(val_tot>0 | is.na(val_tot)) %>%
  mutate(eatout=1) %>%  # All items here are outside consumption
  data.table(key="code7") %>%
  inner_join(ce_code %>% mutate(code7=as.numeric(code)), by="code7") %>%  # Keep only food items
  select(id, code7, POF.item=product, val_tot, eatout) 



# Add more to hh
hh <- hh %>% mutate(income = income * CPI.r / PPP$PA.NUS.PRVT.PP) %>% 
  mutate(inc.percap = income/hh_size) %>% ungroup() %>%
  # income groups, decile, quartile separately determined for urban and rural
  group_by(urban) %>%
  arrange(urban, inc.percap) %>%
  mutate(cumpop = cumsum(hh_size*weight)/sum(weight*hh_size)) %>%
  mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1), labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE)) %>%
  mutate(quartile = cut(cumpop, breaks = seq(0, 1, 0.25), labels=paste0("quartile", 1:4), include.lowest = TRUE, ordered=TRUE)) %>%
  select(-cumpop) %>%
  # mutate(inc_grp=as.integer(cut(inc.percap,breaks=c(0, 1.4*365, 2.8*365, 5.6*365, max(inc.percap)*365), labels=c(1:4)))) %>% # NEED TO ADJUST!
  mutate(inc_grp=as.numeric(quartile)) %>% 
  mutate(female_adult=hh_size-minor-male_adult, female_minor=minor-male_minor) %>%
  mutate(cu_eq=(male_adult*getcu("male_adult")+female_adult*getcu("female_adult")+
                  male_minor*getcu("male_minor")+female_minor*getcu("female_minor"))) %>%
  left_join(states.BR) %>%
  mutate(cluster=paste0(Reg, urban, '.', inc_grp))   # By region or state?


### Combine food group information

food.group <- read_excel("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/POF_2008-2009_Codigos_de_alimentacao.xls",
                         skip=2) %>% select(-2:-3) %>% setnames(c("code5", "group.id1", "descr1", "group.id2", "descr2")) %>%
  filter(!is.na(code5))
food.wgrp.names <- unique(read_excel("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/POF_2008-2009_Codigos_de_alimentacao.xls",
                                     sheet=3, skip=2) %>% select(3,7)) %>% setnames(c("group.id1", "wgroup"))
food.grp.names <- unique(read_excel("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009/Documentation/POF_2008-2009_Codigos_de_alimentacao.xls",
                                    sheet=3, skip=2) %>% select(5,8)) %>% setnames(c("group.id2", "group"))

# English names for groups + All eatout items combined
food.group <- food.group %>% 
  left_join(food.wgrp.names) %>% 
  left_join(food.grp.names) %>% 
  select(code5, group.id1, wgroup, group.id2, group) %>%
  mutate_cond(is.na(group.id2), group.id2="2", group="eatout or prepared" ) %>%
  mutate_cond(group=="Others" | group=="Organic" | group=="Light and Diet", group=paste(group, wgroup, sep="_")) %>%
  rename(item=group, group=wgroup)
# write.table(food.cat.names, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)


# Master data table for food analysis
# Merge hh characteristics
# kcal:vita are nutrient/100g food.
food.tbl <- food.tbl %>% rbind.fill(cons) %>% 
  mutate(code5=floor(code7/100)) %>% group_by(id) %>%
  mutate(eatout.share = sum(eatout*val_tot) / sum(val_tot),
         mapped = ifelse(is.na(kcal), 0, 1))  %>%
  # left_join(data.table(hh, key="id"), by="id") %>% data.frame() %>%
  left_join(food.group) 



### Income conversion to $2010PPP and create income groups

library(WDI)
PPP <- WDI(country = "BR", indicator = c("PA.NUS.PRVT.PP"), start = 2010, end = 2010, extra = FALSE, cache = NULL)  #[LCU/$]
CPI <- WDI(country = "BR", indicator = "FP.CPI.TOTL", start = 2008, end = 2010, extra = FALSE, cache = NULL)
CPI.r <- as.numeric(CPI %>% filter(year==2010) %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2008) %>% select(FP.CPI.TOTL))
# EXR <- WDI(country = "BR", indicator = "PA.NUS.FCRF", start = 2008, end = 2008, extra = FALSE, cache = NULL) # Exchange rate (MER) [LCU/$]



### Identify outeaters
outeat.threshold <- 0.3
outeaters <- food.tbl %>% group_by(id) %>% summarise(eatout.share=first(eatout.share)) %>% mutate(outeater=(eatout.share > outeat.threshold))

hh.excl.outeater <- hh %>% left_join(outeaters) %>% filter(!outeater) 
# Recalculate income groups without outeaters
hh.excl.outeater <- hh.excl.outeater %>% ungroup() %>%
  # income groups, decile, quartile separately determined for urban and rural
  group_by(urban) %>%
  arrange(urban, inc.percap) %>%
  mutate(cumpop = cumsum(hh_size*weight)/sum(weight*hh_size)) %>%
  mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1), labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE)) %>%
  mutate(quartile = cut(cumpop, breaks = seq(0, 1, 0.25), labels=paste0("quartile", 1:4), include.lowest = TRUE, ordered=TRUE)) %>%
  select(-cumpop) %>%
  mutate(inc_grp=as.numeric(quartile)) %>%
  mutate(cluster=paste0(Reg, urban, '.', inc_grp))   # Redefine cluster

# Find cutoffs for income quartiles
hh %>% group_by(cluster) %>% summarise(cut.incgrp=first(inc.percap)/365)
hh.excl.outeater %>% group_by(cluster) %>% summarise(cut.incgrp=first(inc.percap)/365)


# Master data table for food analysis
# Merge hh characteristics
# kcal:vita are nutrient/100g food.
food.master <- food.tbl %>%
  inner_join(data.table(hh, key="id"), by="id") %>% data.frame() %>%
  left_join(outeaters %>% select(-eatout.share), by="id") %>% filter(!outeater) 
food.master.excl.outeater <- food.tbl %>%
  inner_join(data.table(hh.excl.outeater, key="id"), by="id") %>% data.frame() 
