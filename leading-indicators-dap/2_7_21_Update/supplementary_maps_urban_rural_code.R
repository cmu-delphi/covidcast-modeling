
#Vishnu Shankar
#Visualize Relationships


#How to draw maps with F1 SCORE?
library(maps)
library(dplyr)
data(county.fips)

data(county.fips)
precision_recall_df=get_county_level_precision_recall(final_cases_indicator_list = fb_survey_cases_indicator_list,
                                                      min_window=3, max_window=14)
precision_recall_df[[1]]$County_Code = as.integer(precision_recall_df[[1]]$County_Code)
counties <- county.fips %>% left_join(precision_recall_df[[1]], by=c('fips'='County_Code'))
counties=counties[-which(is.na(counties$F1_Score)),]

counties$color = ifelse(counties$F1_Score > 0.5, "chartreuse", "black")
map("county",fill=TRUE, col=counties$color )


#URBAN v. RURAL DISTINCTION 
urban_rural=read.csv(file="census_data_rural.csv",header=T, stringsAsFactors = F)
summary(urban_rural$X2010.Census..Percent.Rural)
urban_rural$Rural = ifelse(urban_rural$X2010.Census..Percent.Rural >= 50.0, "Rural", "Urban")
urban_rural = merge(as.data.table(urban_rural), as.data.table(precision_recall_df[[1]]), by.x="FIPS_county_code",
                    by.y = "County_Code")
library(ggpubr)
ggboxplot(urban_rural, x = "Rural", y = "F1_Score") + stat_compare_means(method="t.test")
