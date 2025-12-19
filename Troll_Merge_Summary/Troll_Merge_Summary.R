# this script is from 2022 and will likely need refinement 
### CODE TO TAKE SINGLE .csv TROLL FILES AND COMBINE THEM INTO ONE TABLE###
### EITHER SAVE WHOLE TABLE AS .CSV AND USE PIVOT TABLES IN EXCEL, OR
### CONTINUE SCRIPT TO END TO GET SUMMARY STATS (MEAN, MAX, MIN, COUNT)

### Make sure each troll file has the same heading order. If they differ, save files with
### the same heading orders in different folders and run script separately for each folder

library(readr)
library(tidyverse)
library(data.table)
library(gdata)



# In the Files folder, save all Troll files you want to combine and summarize with 
# naming convention Site_date mmddyyyy like: RM21_07052021. This step is required 
# for script to work properly so data can be traced back to the site it was taken at.

# open folder with Troll files, copy and paste path into function below. Make sure 
# to use "/" instead of "\"

files <- list.files("C:/R/AquaTroll/Files", full.names = TRUE) ###USER INPUT

#create an empty list to store dataframes
ldf <- list()

#read in each file, ignoring top 18 rows and last 2-3 columns (marked, lat, long)
#because not all files have these columns so it confuses R. Add a column with 
# source id $Site. this is the file path to trace where data came from

for (i in 1:length(files)){
  ldf[[i]] <- fread(files[i], select = c(1:18), skip = 19)
  ldf[[i]]$Site <- files[i]
}
str(ldf[[1]])

#combine tables into one table "Troll" and add header colnames

Troll <- bind_rows(ldf)

colnames <- c("DateTime", "Turbidity_NTU", "pH", "pH_mV",
              "ORP_mV", "RDO_Concentration_mgL", "RDO_Saturation",
              "Oxy_Par_Pres_TORR", "Act_Conductivity_S_cm",
              "Spe_Conductivity_uS_cm", "Salinity_PSU",
              "TDS_ppt", "Resistivity",
              "Density_cm3", "Wat_Temp_C", "Ext_Voltage_V",
              "Barometric_Pres_mbar", "Air_Temp_C", "Site") ###user input if structure of file differs

colnames(Troll) <- c(colnames) 

#edit source file column $Site to Site_date

Troll$Site <- gsub(".*/","",Troll$Site)
Troll$Site <- gsub(".csv", "", Troll$Site)

#Select columns in order you want them in table. Column names listed above in cnames.

Troll$DateTime <- as.POSIXct(strptime(Troll$DateTime, "%m/%d/%Y %H:%M"))
Troll <- select(Troll, Site, DateTime, Wat_Temp_C, RDO_Concentration_mgL, pH, 
                Salinity_PSU, Turbidity_NTU, Spe_Conductivity_uS_cm, TDS_ppt)

#Export as csv to complete statistics in excel

write.csv(Troll, file = "All_Troll_Data.csv", row.names = FALSE) ###Can change name of file here

#######Summary Statistics of columns selected above. Avg, min/max, n. #######

#create table containing averages
averages <- Troll %>%
  group_by(Site) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE))

#create table containing mins and max of pH and datetime
min_max <- Troll %>%
  group_by(Site) %>%
  summarise(pH_min = min(pH, na.rm=T), pH_max = max(pH, na.rm=T), 
            DateTime_Start = min(DateTime),
            DateTime_End = max(DateTime)) %>%
  mutate(minutes = DateTime_End - DateTime_Start)

#create table with n for each variable
n <-  as.data.frame(Troll)

n <- n %>%
  group_by(Site) %>% 
  summarise_at(vars(Wat_Temp_C:TDS_ppt), ~sum(!is.na(.))) %>%
  rename_at(vars(-Site), ~ paste0(., '_n'))

#combine averages, min_max, and n tables created above
Troll_Summary <- bind_cols(averages, min_max, n)

#format dates and times 
Troll_Summary$Start_Date <- as.Date(Troll_Summary$DateTime_Start) 
Troll_Summary$Start_Time <- format(as.POSIXct(Troll_Summary$DateTime_Start),  format = "%H:%M:%S")
Troll_Summary$End_Date <- as.Date(Troll_Summary$DateTime_End) 
Troll_Summary$End_Time <- format(as.POSIXct(Troll_Summary$DateTime_End),  format = "%H:%M:%S")

#remove date from Site (now rm12.75 instead of rm12.75_06122021)
Troll_Summary$SiteName <- gsub('.{9}$', '', Troll_Summary$Site...1)

#select columns for template
Troll_Summary <- select(Troll_Summary, SiteName, Start_Date, Start_Time, End_Date,
                        End_Time, minutes,
                        Wat_Temp_C_mean, Wat_Temp_C_n, 
                        RDO_Concentration_mgL_mean,  RDO_Concentration_mgL_n,
                        pH_mean, pH_min, pH_max, pH_n,
                        Salinity_PSU_mean, Salinity_PSU_n,
                        Turbidity_NTU_mean, Turbidity_NTU_n,
                        Spe_Conductivity_uS_cm_mean, Spe_Conductivity_uS_cm_n,
                        TDS_ppt_mean, TDS_ppt_n)

write.csv(Troll_Summary, "Troll_Summary.csv") #can change name of file here
