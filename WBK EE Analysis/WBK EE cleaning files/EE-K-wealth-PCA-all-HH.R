#PCA for Kenya data
rm(list=ls())
library(tidyverse)

ee_covars <- readRDS("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-covariates.rds")
colnames(ee_covars)
head(ee_covars)

     
       
assets <- ee_covars %>%
  select(hhid, clusterid, #n_cows, n_goats, n_chickens, #n_dogs,
         #floor, roof, #wall,
         elec, 
         asset_radio, asset_tv,    asset_mobile,asset_clock,
         asset_bike,  asset_moto,  asset_stove, asset_cooker,asset_car ) %>% distinct()

summary(assets)
table(assets$asset_bike)
table(assets$asset_moto)
table(assets$asset_cooker)

#replace "Don't Know" with NA and convert to indicators
for (i in 3:ncol(assets)) {
  assets[,i] <- as.character(assets[,i])
  assets[,i] = ifelse(assets[,i] == "Missing/DK", NA, assets[,i])
  assets[,i] = ifelse(grepl("No ",assets[,i]), 0, ifelse(is.na(assets[,i]),NA,1))
}

table(assets$asset_bike)
table(assets$asset_cooker)

#remove rows with mostly missing data
table(rowSums(is.na(assets)))
assets <- assets %>%
  mutate(missing = rowSums(is.na(assets))) %>%
  filter(missing < 14) %>%
  select(-missing)
table(rowSums(is.na(assets)))

#Convert to numeric indicators
assets <- droplevels(assets)



#impute remaining missing values using median
assets.impute <- assets
for(i in 3:ncol(assets.impute)){
  assets.impute[is.na(assets.impute[,i]), i] <- median(assets.impute[,i], na.rm = TRUE)
}

#perform pca
pca <- prcomp(assets.impute[,4:ncol(assets.impute)], center = TRUE, scale = TRUE)
summary(pca)
pca$rotation[,1]

#visualize PC1 and PC2
#biplot(pca)

#predict HHwealth using 1st PC
assets.impute$HHwealth <- predict(pca)[,1]

assets.impute %>% mutate(wealth_cat=ntile(HHwealth,4)) %>% group_by(wealth_cat) %>%
  summarise_all(mean)

summary(assets.impute$HHwealth)

wealth <- assets.impute %>%select(hhid, clusterid, HHwealth)

#export data
write.csv(wealth, file = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/Kenya-full-wealth-index.csv")
