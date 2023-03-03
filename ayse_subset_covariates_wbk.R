

rm(list=ls())
library(tidyverse)
source(here::here("0-config.R"))

#Read in master dataset
d <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Caitlin/caitlin full join kenya ee.csv")

head(d)

df <- d %>% select(hhid, childid, birthord, ageday_at1, ageday_at2, ageday_at3)
head(df)

df %>% filter(childid==200901101)


write.csv(df,"C:/Users/andre/Dropbox/AB use/data/wbk_ee_birthord_anthro_age.csv")


d <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Caitlin/washb-kenya-allkids-ee-med-age-no-dob.csv")

table(d$nulliparous)

df <- d %>% mutate(
  birthord=case_when(
    nulliparous=="yes" ~ "1",
    nulliparous=="no" ~ "2+",
    nulliparous=="" ~ "missing"
  )) %>% 
    rename(ageday1=ageday_mt1, ageday2=ageday_mt2, ageday3=ageday_mt3) %>%
  select(hhid, childid, birthord, sex, ageday1, ageday2, ageday3)

df %>% filter(childid==200901101)

write.csv(df,"C:/Users/andre/Dropbox/AB use/data/wbk_ee_birthord_anthro_age_update.csv")
