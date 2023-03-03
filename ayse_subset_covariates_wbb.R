

rm(list=ls())

source(here::here("0-config.R"))

#Read in master dataset
d <- box_read("871638120165") %>% filter(.data[["eed_growth"]]==1)
head(d)

d$ageday_at1
df <- d %>% select(dataid, childid, birthord, ageday_at1, ageday_at2, ageday_at3)
head(df)

write.csv(df,"C:/Users/andre/Dropbox/AB use/data/wbb_ee_birthord_anthro_age.csv")

