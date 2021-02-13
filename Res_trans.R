library(dplyr)
# install.packages("shiny")
library(shiny)
#install.packages("tidyr")
library(ggplot2)

#Reading files

Bion_Am<-read.csv("Bionomics_Americas.csv")
Bion_Af<-read.csv("Bionomics_Africa.csv")
Bion_AsPac<-read.csv("Bionomics_Asia_Pacific.csv")

#Clean datasets


#Select relevant variables

Af_clean<-select(Bion_Af,country, species, indoor_host, outdoor_host, combined_host, host_unit, indoor_biting, outdoor_biting, indoor_outdoor_biting_units, combined_1830_2130, combined_2130_0030, combined_0030_0330, combined_0330_0630,citation)

Am_clean<-select(Bion_Am,country, species, indoor_host, outdoor_host, combined_host, host_unit, indoor_biting, outdoor_biting, indoor_outdoor_biting_units, combined_1830_2130, combined_2130_0030, combined_0030_0330, combined_0330_0630,citation)

AsPac_clean<-select(Bion_AsPac,country, species, indoor_host, outdoor_host, combined_host, host_unit, indoor_biting, outdoor_biting, indoor_outdoor_biting_units, combined_1830_2130, combined_2130_0030, combined_0030_0330, combined_0330_0630,citation)

#Remove NAs from certain columns

#Removing entries with no info on HBI

Af_HBI<-Af_clean[!with(Af_clean,is.na(indoor_host)& is.na(outdoor_host)),]

Am_HBI<-Am_clean[!with(Am_clean,is.na(indoor_host)& is.na(outdoor_host)),]

AsPac_HBI<-AsPac_clean[!with(AsPac_clean,is.na(indoor_host)& is.na(outdoor_host)),]

#Removing entries with no info on exophily/endophily

Af_in_out<-Af_clean[!with(Af_clean,is.na(indoor_biting)& is.na(outdoor_biting)),]

Am_in_out<-Am_clean[!with(Am_clean,is.na(indoor_biting)& is.na(outdoor_biting)),]

AsPac_in_out<-AsPac_clean[!with(AsPac_clean,is.na(indoor_biting)& is.na(outdoor_biting)),]

#Saving files

#write.csv(Af_clean, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\Africa_clean.csv", row.names=FALSE)


# write.csv(Am_clean, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\America_clean.csv", row.names=FALSE)
# 
# write.csv(AsPac_clean, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\AsiaPacific_clean.csv", row.names=FALSE)
# 
# write.csv(Af_HBI, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\Africa_HBI.csv", row.names=FALSE)
# 
# write.csv(Af_in_out, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\Africa_in_out.csv", row.names=FALSE)
# 
# write.csv(Am_HBI, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\America_HBI.csv", row.names=FALSE)
# 
# write.csv(Am_in_out, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\America_in_out.csv", row.names=FALSE)
# 
# write.csv(AsPac_HBI, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\AsPac_HBI.csv", row.names=FALSE)
# 
# write.csv(AsPac_in_out, "C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Residual malaria transmission\\AsPac_in_out.csv", row.names=FALSE)


message("successfully loaded data")

# Combine all into one
df <-
  Af_clean %>% mutate(region = 'Africa') %>%
  bind_rows(Am_clean %>% mutate(region = 'Americas')) %>%
  bind_rows(AsPac_clean %>% mutate(region = 'Asian Pacific'))

# Make a quick and dirty plot
pd <- df %>%
  dplyr::select(hbi = indoor_host,
                indoor_biting, region,
                contains('combined_')) %>%
  tidyr::gather(key, value, combined_1830_2130:combined_0330_0630) %>%
  mutate(key = gsub('combined_', '', key))