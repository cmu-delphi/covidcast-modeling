#devtools::install_github("cmu-delphi/covidcast", ref = "main",
#                         subdir = "R-packages/covidcast")
library(covidcast)
library(stringr)
meta <- covidcast_meta()

#fips code
#clarke county 13059
#tuscaloosa 01125
#richland 45079
#champaign 17019
#johnson 19103
college_county_codes=c("13059", "01125", "45079", "17019", "19103")

cli <- suppressMessages(
  covidcast_signal(data_source = "fb-survey", signal = "smoothed_cli",
                   start_day = "2020-08-19", end_day = "2020-09-11",
                   geo_type = "county")
)
cli=cli[which(cli$geo_value %in% college_county_codes),]

cmty_cli<-suppressMessages(
  covidcast_signal(data_source = "fb-survey", signal = "smoothed_hh_cmnty_cli",
                   start_day = "2020-08-19", end_day = "2020-09-11",
                   geo_type = "county")
)
cmty_cli=cmty_cli[which(cmty_cli$geo_value %in% college_county_codes),]

case_counts=covidcast_signal(data_source ="indicator-combination", signal = "confirmed_7dav_incidence_num",
                             geo_type = "county", start_day = "2020-08-19", end_day = "2020-09-11")
santaclara_counts=case_counts[which(case_counts$geo_value == "06085"),]
case_counts=case_counts[which(case_counts$geo_value %in% college_county_codes),]

#case counts signal
#clarke county 13059
#tuscaloosa 01125
#richland 45079
#champaign 17019
#johnson 19103
#top 5 counties
par(mfrow=c(2,5))
plot(x = case_counts[which(case_counts$geo_value == "13059"),]$time_value,
     y=case_counts[which(case_counts$geo_value == "13059"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Clarke County (GA)",
     col = 'orange')
plot(x = case_counts[which(case_counts$geo_value == "01125"),]$time_value,
     y=case_counts[which(case_counts$geo_value == "01125"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Tuscaloosa County (AL)",
     col='red')
plot(x = case_counts[which(case_counts$geo_value == "45079"),]$time_value,
     y=case_counts[which(case_counts$geo_value == "45079"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Richland County (SC)",
     col = 'blue')
plot(x = case_counts[which(case_counts$geo_value == "17019"),]$time_value,
     y= case_counts[which(case_counts$geo_value == "17019"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Champaign County (IL)",
     col = 'green')
plot(x = case_counts[which(case_counts$geo_value == "19103"),]$time_value,
     y= case_counts[which(case_counts$geo_value == "19103"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Johnson County (IA)",
     col = 'purple')
plot(x = santaclara_counts$time_value,
     y= santaclara_counts$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Santa Clara (CA)",
     col = 'black')


#Smoothed FB Survey Signal
#top 5 counties
plot(x = cli[which(cli$geo_value == "13059"),]$time_value,
     y=cli[which(cli$geo_value == "13059"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Clarke County (GA)",
     col = 'orange')
plot(x = cli[which(cli$geo_value == "01125"),]$time_value,
     y=cli[which(cli$geo_value == "01125"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Tuscaloosa County (AL)",
     col='red')
plot(x = cli[which(cli$geo_value == "45079"),]$time_value,
     y=cli[which(cli$geo_value == "45079"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Richland County (SC)",
     col = 'blue')
plot(x = cli[which(cli$geo_value == "17019"),]$time_value,
     y= cli[which(cli$geo_value == "17019"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Champaign County (IL)",
     col = 'green')
plot(x = cli[which(cli$geo_value == "19103"),]$time_value,
     y= cli[which(cli$geo_value == "19103"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Johnson County (IA)",
     col = 'purple')

plot(x = cmty_cli[which(cmty_cli$geo_value == "13059"),]$time_value,
     y=cmty_cli[which(cmty_cli$geo_value == "13059"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Clarke County (GA)",
     col = 'orange')
plot(x = cmty_cli[which(cmty_cli$geo_value == "01125"),]$time_value,
     y=cmty_cli[which(cmty_cli$geo_value == "01125"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Tuscaloosa County (AL)",
     col='red')
plot(x = cmty_cli[which(cmty_cli$geo_value == "45079"),]$time_value,
     y=cmty_cli[which(cmty_cli$geo_value == "45079"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Richland County (SC)",
     col = 'blue')
plot(x = cmty_cli[which(cmty_cli$geo_value == "17019"),]$time_value,
     y= cmty_cli[which(cmty_cli$geo_value == "17019"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Champaign County (IL)",
     col = 'green')
plot(x = cmty_cli[which(cmty_cli$geo_value == "19103"),]$time_value,
     y= cmty_cli[which(cmty_cli$geo_value == "19103"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Johnson County (IA)",
     col = 'purple')

#sample size
plot.df=data.frame(Date=c(cmty_cli[which(cli$geo_value == "13059"),]$time_value, 
                  cmty_cli[which(cmty_cli$geo_value == "01125"),]$time_value,
                  cmty_cli[which(cmty_cli$geo_value == "45079"),]$time_value,
                  cmty_cli[which(cmty_cli$geo_value == "17019"),]$time_value,
                  cmty_cli[which(cmty_cli$geo_value == "19103"),]$time_value),
           Sample_Size=c(cmty_cli[which(cli$geo_value == "13059"),]$sample_size, 
                         cmty_cli[which(cmty_cli$geo_value == "01125"),]$sample_size,
                         cmty_cli[which(cmty_cli$geo_value == "45079"),]$sample_size,
                         cmty_cli[which(cmty_cli$geo_value == "17019"),]$sample_size,
                         cmty_cli[which(cmty_cli$geo_value == "19103"),]$sample_size),
           County = c(rep("Clarke County (GA)", length(cmty_cli[which(cli$geo_value == "13059"),]$time_value)),
                      rep("Tuscaloosa County (AL)", length(cmty_cli[which(cli$geo_value == "01125"),]$time_value)),
                      rep("Richland County (GA)", length(cmty_cli[which(cli$geo_value == "45079"),]$time_value)), 
                      rep("Champaign County (GA)", length(cmty_cli[which(cli$geo_value == "17019"),]$time_value)),
                      rep("Johnson County (GA)", length(cmty_cli[which(cli$geo_value == "19103"),]$time_value))),
           stringsAsFactors = F)
library(ggpubr)

p1=ggline(data=plot.df, x = "Date", y = "Sample_Size", color ="County",
       ylab = "Sample Size", palette = c("green", "orange", "purple", "blue", "red"))+grids(linetype="dashed")+
  theme(legend.position = "top", axis.title = element_text(size=18),
        legend.text = element_text(size=12))
p1


#Away from Home 3-6hr (SafeGraph)
away_home <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "part_time_work_prop",
                   start_day = "2020-08-19", end_day = "2020-09-11",
                   geo_type = "county")
)
santaclara_county=away_home[which(away_home$geo_value %in% "06085"),]
away_home=away_home[which(away_home$geo_value %in% college_county_codes),]
plot(x = away_home[which(away_home$geo_value == "13059"),]$time_value,
     y=away_home[which(away_home$geo_value == "13059"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Clarke County (GA)",
     col = 'orange')
plot(x = away_home[which(away_home$geo_value == "01125"),]$time_value,
     y=away_home[which(away_home$geo_value == "01125"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Tuscaloosa County (AL)",
     col='red')
plot(x = away_home[which(away_home$geo_value == "45079"),]$time_value,
     y=away_home[which(away_home$geo_value == "45079"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Richland County (SC)",
     col = 'blue')
plot(x = away_home[which(away_home$geo_value == "17019"),]$time_value,
     y= away_home[which(away_home$geo_value == "17019"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Champaign County (IL)",
     col = 'green')
plot(x = away_home[which(away_home$geo_value == "19103"),]$time_value,
     y= away_home[which(away_home$geo_value == "19103"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Johnson County (IA)",
     col = 'purple')
plot(x = santaclara_county$time_value,
     y= santaclara_county$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Santa Clara (CA)",
     col = 'black')

doctor_visits <- suppressMessages(
  covidcast_signal(data_source = "doctor-visits", signal = "smoothed_adj_cli",
                   start_day = "2020-08-19", end_day = "2020-09-11",
                   geo_type = "county")
)
doctor_visits=doctor_visits[which(doctor_visits$geo_value %in% college_county_codes),]
plot(x = doctor_visits[which(doctor_visits$geo_value == "13059"),]$time_value,
     y=doctor_visits[which(doctor_visits$geo_value == "13059"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Clarke County (GA)",
     col = 'orange')
plot(x = doctor_visits[which(doctor_visits$geo_value == "01125"),]$time_value,
     y=doctor_visits[which(doctor_visits$geo_value == "01125"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Tuscaloosa County (AL)",
     col='red')
plot(x = doctor_visits[which(doctor_visits$geo_value == "45079"),]$time_value,
     y=doctor_visits[which(doctor_visits$geo_value == "45079"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Richland County (SC)",
     col = 'blue')
plot(x = doctor_visits[which(doctor_visits$geo_value == "17019"),]$time_value,
     y= doctor_visits[which(doctor_visits$geo_value == "17019"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Champaign County (IL)",
     col = 'green')
plot(x = doctor_visits[which(doctor_visits$geo_value == "19103"),]$time_value,
     y= doctor_visits[which(doctor_visits$geo_value == "19103"),]$value,
     xlab ="Date", ylab = "Value", type = 'b', pch = 19, lwd=2, main = "Johnson County (IA)",
     col = 'purple')


