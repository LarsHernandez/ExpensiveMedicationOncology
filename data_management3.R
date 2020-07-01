

# Expensive medication data management script -----------------------------
# Lars B. Nielsen - lars.borty@gmail.com
# Spring 2020 - thesis in economics

# The script consists of three parts
# 1. Data from PAS
# 2. Data from Apovision (hospital pharmacy)
# 3. Data from MedOnc 

# The finished product is 2 files
# mss  = dataframe of subset of treatments (ones that carry signigicant cost)
#        with a price for each treatment
# mssg = The dataset above grouped be individuals

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(haven)
library(lubridate)
library(magrittr)
library(haven)

t <- Sys.time()



# PAS ---------------------------------------------------------------------
# Data on diagnosis, gender and age is extracted from the Danish PAS system
# Only necessary id's are extracted by filtering for medonc_IDNr.rdata

# Load data ---------------------------------------------------------------

load("Z:/lars_speciale/generated_data/medonc_IDNr2.rdata")

icd <- read_csv("external_data/icd_categories.csv", col_names = FALSE) %>% rename(ICD = X1, diagtxt = X2)
civ <- read_sas("Z:/001_Data/PAS_GRUNDDATA/20200316/onc_0004_02_age_sex_civ.sas7bdat", NULL, 
                col_select = c("koen","FOEDDATO","mors_dato_cpr","mors_dato_pas","IDNr")) %>% filter(IDNr %in% medonc_IDNr)
gen <- read_sas("Z:/001_Data/PAS_GRUNDDATA/20200316/stindfil2.sas7bdat", NULL, 
                col_select = c("IndDatD","UdsDatD","StmSgh","HvDiag","Kommun","Alder","IDNr","Kon","DiaKodA","TYPE")) %>% filter(IDNr %in% medonc_IDNr)
#pac <- read_sas("Z:/001_Data/PAS_GRUNDDATA/kraeftpakker.sas7bdat", NULL, 
#                col_select = c("IDNr","pakkenavn","PakkeForlobNr","start_dato","slut_dato","Bekraeft")) %>% filter(IDNr %in% medonc_IDNr)

# Age ---------------------------------------------------------------------

age <- civ %>% 
  mutate(death_date = if_else(is.na(mors_dato_cpr), mors_dato_pas, mors_dato_cpr)) %>% #dual death registrations, cpr is best but with a few missing
  rename(gender     = koen,
         birth_date = FOEDDATO) %>% 
  select(-mors_dato_cpr, -mors_dato_pas) %>% 
  mutate(age = if_else(is.na(death_date), 
                       round(as.numeric(difftime(as.Date("2018-01-01"),birth_date, units = "weeks")/52.25)),
                       round(as.numeric(difftime(death_date,birth_date, units = "weeks")/52.25))))

# Diag --------------------------------------------------------------------

diag <- gen %>% 
  filter(IndDatD > as.Date("2007-01-01")) %>% 
  filter(substr(DiaKodA,1,2) == "DC"| DiaKodA=="" & substr(HvDiag,1,2)=="DC") %>% 
  mutate(DiaKodA = if_else(DiaKodA=="",HvDiag,DiaKodA)) %>% 
  mutate(diag = substr(DiaKodA,1,4)) %>% 
  group_by(diag, IDNr) %>% 
  summarize(IndDatD = first(IndDatD),
            sgh     = first(StmSgh),
            full_diag = first(DiaKodA))

diag <- diag %>% 
  group_by(IDNr) %>% 
  summarize(diag1 = nth(diag,1),
            diag2 = nth(diag,2),
            diag3 = nth(diag,3),
            diag4 = nth(diag,4),
            diag5 = nth(diag,5),
            diag6 = nth(diag,6),
            sgh = first(sgh),
            IndDatD = first(IndDatD),
            full_diag = first(full_diag))

diag <- diag %>% 
  mutate(ICD = substr(diag1,2,4)) %>% 
  left_join(icd, by="ICD") %>% 
  mutate(diagtxt2 = str_replace(diagtxt, "Malignant neoplasm of ",""))

diag <- diag %>% 
  group_by(IDNr) %>% 
  mutate(ndiag=sum(!is.na(diag1), 
                   !is.na(diag2), 
                   !is.na(diag3), 
                   !is.na(diag4), 
                   !is.na(diag5), 
                   !is.na(diag6)))

save(age,  file="generated_data/age2.rdata")
save(diag, file="generated_data/diag2.rdata")





# Apovision ---------------------------------------------------------------
# The data originates from the hospital pharmacy. Data extracted 23-01-2020
# In the data some errors occour, this means that to adjust accounts sometimes
# the cost is negative. Other errors occurs from drugs not being registred 
# right in package size, strength or unit. Most are obvious from looking at
# text strings provided from the pharmacy. Only a few are not and they are 
# all commented in the code.

# Load data ---------------------------------------------------------------

onc_pharm <- read_excel("Z:/lars_speciale/external_data/onc_pharm_2020.xlsx")
names(onc_pharm) <- c("atc_code_description", "info","brand_name","form","pkg_size","pkg_unit",
                      "pkg_text","str_num","str_unit", "str_text","year","month","ym","cost","n")

pharm <- onc_pharm %>% 
  filter(cost != 0) %>% 
  filter(between(year,2008,2019)) %>% 
  filter(n    != 0) %>% 
  mutate(mon = substr(month,1,3)) %>% 
  mutate(month = case_when(mon == "jan" ~ 1,  mon == "feb" ~ 2,  mon == "mar" ~ 3,
                           mon == "apr" ~ 4,  mon == "maj" ~ 5,  mon == "jun" ~ 6,
                           mon == "jul" ~ 7,  mon == "aug" ~ 8,  mon == "sep" ~ 9,
                           mon == "okt" ~ 10, mon == "nov" ~ 11, mon == "dec" ~ 12)) %>% 
  mutate(date = as.Date(paste0(year,"-",month,"-01")))

total_cost_unadjusted <- sum(pharm$cost, na.rm=T)/1e6

# adjusting for inflation
defl <- statsDK::sdk_retrieve_data("PRIS112") %>% 
  filter(HOVED == "Average, yearly", between(TID, 2008, 2019)) %>% 
  mutate(index = as.numeric(INDHOLD)/103, year = TID) %>% 
  select(year, index)

pharm <- pharm %>% 
  left_join(defl, by = "year") %>% 
  mutate(cost = cost/index)

total_cost <- sum(pharm$cost, na.rm=T)/1e6

pharm <- pharm %>% 
  separate(atc_code_description, c("atc_code","generic_name"), " ", extra = "merge") %>% 
  mutate(generic_name = str_trim(generic_name)) %>% 
  separate(info, c("id","MIX"), " ", extra = "merge") %>% 
  mutate(str_num = as.numeric(str_replace(str_num, pattern = ",", replacement = "."))) %>% 
  mutate(pkg_cost = cost/n)

pharm <- pharm %>% 
  mutate(atc_code = if_else(id %in% c("802809", "831545"), "fee_injmix", atc_code)) %>% # atc_code for mixing fee from the hospital pharmacy
  mutate(atc_code = if_else(atc_code == "V07AY**",         "non_thp",    atc_code)) %>% # non-therepeutical products primarily infusers
  mutate(atc_code = if_else(atc_code == "L******",         "L04AA34",    atc_code))     # Most likely error in dataset, name says product name

# Error correction --------------------------------------------------------

pharm <- pharm %>% 
  mutate(str_num  = if_else(atc_code == "L01CD04" & str_unit == "-", 45, str_num), # Some kind of change of products, my best guess is that it's 45mg packages as it says 60mg/1.5Ml
         str_unit = if_else(atc_code == "L01CD04" & str_unit == "-", "mg", str_unit)) %>% 
  mutate(pkg_size = if_else(id == "832220", 1,    pkg_size),
         pkg_size = if_else(id == "831727", 1,    pkg_size),
         pkg_size = if_else(id == "832287", 1,    pkg_size),
         pkg_size = if_else(id == "832949", 1,    pkg_size),
         pkg_size = if_else(id == "056936", 1,    pkg_size),
         pkg_size = if_else(id == "010070", 1,    pkg_size),
         pkg_size = if_else(id == "803836", 100,  pkg_size),
         pkg_size = if_else(id == "803902", 50,   pkg_size),
         pkg_size = if_else(id == "804012", 25,   pkg_size), # This is a pure guess, as it doesn't make sense it should be 1
         pkg_size = if_else(id == "066242", 50,   pkg_size),
         pkg_size = if_else(id == "579240", 10,   pkg_size),
         pkg_size = if_else(id == "539385", 4,    pkg_size)) %>% 
  mutate(str_unit = if_else(id == "017331", "mg", str_unit),
         str_num  = if_else(id == "017331", 95,   str_num),   
         str_unit = if_else(id == "042067", "mg", str_unit),
         str_num  = if_else(id == "042067", 95,   str_num),
         str_unit = if_else(id == "426978", "mg", str_unit),
         str_num  = if_else(id == "426978", 95,   str_num),
         str_unit = if_else(id == "072366", "mg", str_unit),
         str_num  = if_else(id == "072366", 95,   str_num),            
         str_unit = if_else(id == "057979", "mg", str_unit), # the pills seem to come in different sizes, i take the mean mg per pill
         str_num  = if_else(id == "057979", 6.25, str_num),
         str_unit = if_else(id == "516182", "mg", str_unit),
         str_num  = if_else(id == "516182", 9,    str_num),         
         str_unit = if_else(id == "803878", "mg", str_unit),
         str_num  = if_else(id == "803878", 600,  str_num),
         str_unit = if_else(id == "460339", "mg", str_unit),
         str_num  = if_else(id == "460339", 600,  str_num),
         str_num  = if_else(id == "099204", 250,  str_num))

pharm <- pharm %>% 
  group_by(atc_code) %>% 
  mutate(sum = sum(cost)) %>%
  filter(str_unit %in% c("mg","mg/ml","-","g","g/l")) %>% 
  mutate(ppmg = case_when(str_unit == "mg"    ~ pkg_cost / (pkg_size * str_num),
                          str_unit == "mg/ml" ~ pkg_cost / (pkg_size * str_num),
                          str_unit == "g"     ~ pkg_cost / (pkg_size * str_num * 1000),
                          str_unit == "g/l"   ~ pkg_cost / (pkg_size * str_num * 1000),
                          str_unit == "-"     ~ pkg_cost))

total_cost_filtered <- sum(pharm$cost, na.rm=T)/1e6

top <- pharm %>% 
  filter(atc_code == "fee_injmix" | substr(atc_code,1,1) == "L") %>% 
  group_by(atc_code) %>% 
  summarize(sum = sum(cost)) %>% 
  filter(sum > 1130000) %>% # Filtering for drugs with a total cost less than 1m still gives most expenses, adjusted to hit 48
  arrange(sum) %>% 
  select(atc_code)

pharmtop <- pharm %>% 
  filter(atc_code %in% top$atc_code)

ppmg_yr <- pharmtop %>% 
  filter(n > 0) %>% 
  group_by(atc_code, year) %>% 
  summarize(ppmg = mean(ppmg),
            cost = sum(cost))

ppmg_mo <- pharmtop %>% 
  filter(n > 0) %>% 
  group_by(date) %>% 
  summarize(cost = sum(cost))

final_cost      <- sum(ppmg_yr$cost, na.rm=T)/1e6
final_drug_cost <- sum(ppmg_yr[ppmg_yr$atc_code!="fee_injmix",]$cost, na.rm=T)/1e6

save(ppmg_mo, file = "generated_data/ppmg_mo.rdata")
save(ppmg_yr, file = "generated_data/ppmg_yr.rdata")
save(pharm,   file = "generated_data/pharm.rdata")




# MedOnc ------------------------------------------------------------------
# Data comes from Oncological department in aalborg in the period 2008 - 
# 2019 and...


# Loading data ------------------------------------------------------------

load("Z:/lars_speciale/generated_data/ppmg_yr.rdata")
load("Z:/lars_speciale/generated_data/atc_lookup.rdata")
load("Z:/lars_speciale/generated_data/age2.rdata")
load("Z:/lars_speciale/generated_data/diag2.rdata")

atc_lookup <- atc_lookup %>% filter(!is.na(atc_code))

medonc_reg <- read_excel("Z:/lars_speciale/external_data/medonc_regimens.xlsx")
medonc     <- read_sas("Z:/001_Data/MedOnc/s2008_2019_fil_med_navne.sas7bdat", NULL)
bmi        <- read_sas("Z:/001_Data/MedOnc/bmi_20200311.sas7bdat", NULL)

bmi <- bmi %>% group_by(IDNr) %>% summarize(bmi = mean(value, na.rm=T))

mo_1 <- paste(nrow(medonc), "~",length(unique(medonc$IDNr)))

ms <- medonc %>% 
  rename("id"      = "ref",
         "moname"  = "drug",
         "txt"     = "dosage_notes",
         "dtime"   = "date_time2",
         "dose"    = "tmpdose",
         "unit"    = "dose_unit",
         "regimen" = "protocol",
         "codes"   = "cycle_day") %>%
  mutate(date = as.Date(substr(dtime,1,10))) %>% 
  mutate(time = substr(dtime,12,20),
         xct  = as.POSIXct(paste0(date,":",time), format = "%Y-%m-%d:%H:%M")) %>% 
  mutate(dose = as.numeric(sub(",", ".", dose, fixed = TRUE))) %>% 
  mutate(moname = tools::toTitleCase(moname)) %>% 
  left_join(atc_lookup, by = "moname") %>%
  filter(placebo != 1) %>% 
  filter(!is.na(IDNr)) %>% 
  select(id, date, time, atc_code, moname, dose, unit, txt, regimen, codes, IDNr, xct)

ms <- ms %>% 
  left_join(medonc_reg[,c(1,3)], by = c("regimen" = "regimen_name"))

# Regex parsing -----------------------------------------------------------

ms <- ms %>% 
  ungroup() %>% 
  mutate(origtxt = txt) %>% 
  mutate(pnttxt  = str_extract(txt, "\\(.+?\\)+"),
         txt     = str_replace(txt, "\\(.+?\\)+","")) %>% 
  mutate(dosetxt = str_extract(txt, "^[:digit:]*\\,*\\.*[[:digit:]]*\\s*-*\\s*[:digit:]+\\,*\\.*[[:digit:]]*"),
         txt     = str_replace(txt, "^[:digit:]*\\,*\\.*[[:digit:]]*\\s*-*\\s*[:digit:]+\\,*\\.*[[:digit:]]*", "")) %>% 
  mutate(unittxt = str_extract(txt, "mg|ml|%|(25 mg/mL)|Capsule|dose(s)|drop(s)|G|g/day|international units|mcg|mg - Units|mg/0.5mL|mg/24hr|mg/d|mg/day|mg/mL|mL|Units"),
         txt     = str_replace(txt, "mg|ml|%|(25 mg/mL)|Capsule|dose(s)|drop(s)|G|g/day|international units|mcg|mg - Units|mg/0.5mL|mg/24hr|mg/d|mg/day|mg/mL|mL|Units", "")) %>% 
  mutate(formtxt = str_extract(txt, paste0("Subcutaneous|Intramuscular|Tablet Oral|Intravenous|Injection|Implant|Capsule Oral|Capsule|Gas Oral",
                                           "|Solution Oral|Suppository Rectal|Suppository|Solution|effervescent Oral|Oral|Tablet")),
         txt     = str_replace(txt, paste0("Subcutaneous|Intramuscular|Tablet Oral|Intravenous|Injection|Implant|Capsule Oral|Capsule|Gas Oral",
                                           "|Solution Oral|Suppository Rectal|Suppository|Solution|effervescent Oral|Oral|Tablet"), "")) %>% 
  mutate(numtxt  = str_extract(txt, "x\\s[:digit:]"),
         txt     = str_replace(txt, "x\\s[:digit:]","")) %>% 
  mutate(txt     = str_replace(txt, "dgl|daily",""),
         txt     = str_replace(txt, "PRN","")) %>% 
  mutate(daytxt  = str_extract(txt, "for [:digit:]+ [:alpha:]+"),
         txt     = str_replace(txt, "for [:digit:]+ [:alpha:]+","")) %>%
  mutate(fortxt  = str_trim(str_extract(txt, "(continuous)*[:blank:]*over [:digit:]+ [:alpha:]+")),
         txt     = str_trim(str_replace(txt, "(continuous)*[:blank:]*over [:digit:]+ [:alpha:]+","")))

# Corrections -------------------------------------------------------------

ms <- ms %>% 
  mutate(form = case_when(str_detect(origtxt, "Intravenous")   ~ "inj",
                          str_detect(origtxt, "Injection")     ~ "inj",
                          str_detect(origtxt, "Tablet")        ~ "tab",
                          str_detect(origtxt, "Oral")          ~ "tab",
                          str_detect(origtxt, "Intramuscular") ~ "mus",
                          str_detect(origtxt, "Subcutaneous")  ~ "sub"))

ms <- ms %>% 
  mutate(day = case_when(str_detect(daytxt, "day")   ~ as.numeric(str_extract(daytxt, "[:digit:]+")),
                         str_detect(daytxt, "days")  ~ as.numeric(str_extract(daytxt, "[:digit:]+")),
                         str_detect(daytxt, "dose")  ~ as.numeric(str_extract(daytxt, "[:digit:]+")),
                         str_detect(daytxt, "week")  ~ as.numeric(str_extract(daytxt, "[:digit:]+")) * 7,
                         str_detect(daytxt, "weeks") ~ as.numeric(str_extract(daytxt, "[:digit:]+")) * 7,
                         is.na(daytxt)               ~ 1)) %>% 
  mutate(num = case_when(!is.na(numtxt) ~ as.numeric(str_extract(numtxt, "[:digit:]+")),
                         is.na(numtxt) ~ 1)) %>%  
  mutate(dosetxt = str_replace(dosetxt, "\\.",""),
         dosetxt = str_replace(dosetxt, "\\,","."),
         dosetxt = as.numeric(dosetxt))

inj_yr <- ppmg_yr %>% 
  filter(atc_code == "fee_injmix") %>% 
  mutate(form = "inj", injcost = ppmg) %>% 
  ungroup() %>% 
  select(year, injcost, form)

ms <- unique(ms[,]) # remove rows that are exact dublicates

ms <- ms %>% # Some doses are extreme, i change them to the parsed which is much lower
  mutate(dose = if_else(dose == 7050 &    IDNr == "43221831", as.numeric(dosetxt), dose),
         dose = if_else(dose == 2240 &    IDNr == "17168251", as.numeric(dosetxt), dose),
         dose = if_else(dose == 2240 &    IDNr == "24355251", as.numeric(dosetxt), dose),
         dose = if_else(dose == 67500 &   IDNr == "36940768", as.numeric(dosetxt), dose),
         dose = if_else(dose == 25000 &   IDNr == "37590892", as.numeric(dosetxt), dose),
         dose = if_else(dose == 1039700 & IDNr == "41450499", as.numeric(dosetxt), dose),
         dose = if_else(dose == 30750 &   IDNr == "43794705", as.numeric(dosetxt), dose),
         dose = if_else(dose == 22500 &   IDNr == "26068153", as.numeric(dosetxt), dose)) %>% 
  mutate(day  = if_else(atc_code == "L01CB01", 1, day), # There seems to be an error in how the days are registred because most are as 3 but fits much better with 1 to true costs
         day  = if_else(atc_code == "L01XC06" & origtxt == "1.000 mg (at 500 mg/m2) Intravenous daily for 2 weeks", 1, day),
         day  = if_else(atc_code == "L01XC07" & origtxt == "413 mg (at 7,5 mg/kg) Intravenous daily for 3 weeks", 1, day),
         day  = if_else(atc_code == "L02BA03" & origtxt == "500 mg Injection daily every other week for 4 weeks", 1, day)) %>% 
  mutate(day  = if_else(txt == "every other week", day/2, day)) %>% 
  filter(day < 181) # filter for some with extreme long period of medication which are most likely errors

  
ms <- ms %>% 
  mutate(year = year(date)) %>% 
  left_join(ppmg_yr, by = c("atc_code","year")) %>% 
  left_join(inj_yr,  by = c("form","year")) %>% 
  mutate(med_price  = ppmg * dose * day * num,
         med_price  = ifelse(med_price == Inf | med_price == -Inf, 0, med_price),
         inj_price  = ifelse(is.na(injcost), 0, injcost),
         full_price   = med_price + inj_price) %>% 
  mutate(datey = as.Date(paste0(year(date),"-01-01"))) %>% 
  left_join(age,  by = "IDNr") %>% 
  left_join(diag, by = "IDNr") %>% 
  left_join(bmi,  by = "IDNr") %>% 
  group_by(IDNr) %>% 
  mutate(event     = if_else(is.na(first(death_date)), 0, 1),
         surv      = if_else(event == 1, 
                            as.numeric(difftime(first(death_date),min(date), units = "days")),
                            as.numeric(difftime("2020-01-01",     min(date), units = "days"))),
         surv      = if_else(surv<1, 1, surv),
         surv_day  = as.numeric(difftime(date,min(date), units = "days"))) %>% 
  mutate(firstdate = min(date),
         ndrug     = length(unique(atc_code)),
         ntreat    = n()) %>% 
  mutate(grp       = if_else(diagtxt2 %in% c("breast", "bronchus and lung", "colon", "rectum",
                                             "pancreas", "ovary", "prostate", "stomach", "brain"), 
                             diagtxt2, "other")) %>% 
  mutate(grp       = if_else(grp == "bronchus and lung", "Lung", grp)) %>% 
  mutate(grp       = tools::toTitleCase(grp))  %>% 
  ungroup()

mo_2 <- paste(nrow(ms), "~",length(unique(ms$IDNr)))

mss <- ms %>% 
  filter(atc_code %in% ppmg_yr$atc_code) %>% 
  filter(med_price > 0)

mo_3 <- paste(nrow(mss), "~",length(unique(mss$IDNr)))

mssg <- mss %>% group_by(IDNr) %>% 
  summarize(med_price   = sum(med_price, na.rm=T),
            full_price  = sum(full_price, na.rm=T),
            grp         = first(grp),
            diag        = first(diagtxt2),
            diag1       = first(diag1),
            ndiag       = first(ndiag),
            ndrug       = first(ndrug),
            ntreat      = first(ntreat),
            gender      = first(gender),
            age         = first(age),
            bmi         = first(bmi),
            regi        = first(regimen_category),
            event       = first(event),
            surv        = first(surv),
            fdate       = min(date))


medonc_med_price  <- mss$med_price %>% sum(na.rm=T)/1e6
medonc_full_price <- mss$full_price %>% sum(na.rm=T)/1e6
diags         <- unique(mss$grp)
medonc_IDNr   <- unique(medonc$IDNr)




save(medonc_IDNr,  file = "generated_data/medonc_IDNr2.rdata")
save(ms,           file = "generated_data/ms2.rdata")
save(mss,          file = "generated_data/mss2.rdata")
save(mssg,         file = "generated_data/mssg2.rdata")


# Time to run -------------------------------------------------------------

Sys.time() - t
















