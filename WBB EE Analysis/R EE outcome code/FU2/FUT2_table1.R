# Install and load necessary packages
library(tidyverse)
library(tableone)
library(officer)
library(flextable)

d<- readRDS(file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_clean_data.RDS")

#distinct to hoursehold
head(d)
dim(d)
d <- d %>% filter(!is.na(mFU2) | !is.na(cFU2)) %>%
  filter(!is.na(dmy(st_date)) | !is.na(dmy(ur_date)) | !is.na(an_aged)) %>%
  distinct(dataid, .keep_all = TRUE) 
dim(d)

table(is.na(d$st_date))

table(is.na(d$cFU2) & is.na(d$mFU2))

# Suppose 'data' is your dataframe and 'tr' is your treatment variable
Wvars<-c(
         'sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
         'walls', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile',
         'n_cows', 'n_goats', 'n_chickens')
tab1 <- CreateTableOne(vars = Wvars, strata = "tr", data = d, test = FALSE)

# Convert tableone object to dataframe for use with flextable
tab1_df <- print(tab1, printToggle = FALSE, quote = FALSE, noSpaces = TRUE)
row_names <- rownames(tab1_df)
rownames(tab1_df) <- NULL
tab1_df <- data.frame(row_names, tab1_df)
# Create a flextable
tab1_flex <- flextable::flextable(tab1_df)

# Create a new Word document and add the table
doc <- officer::read_docx()
doc <- body_add_flextable(doc, tab1_flex)
print(doc, "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FUT2_table1.docx")  # replace "TableOne.docx" with your preferred filename
