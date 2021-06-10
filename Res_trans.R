library(dplyr)
# install.packages("shiny")
library(shiny)
#install.packages("tidyr")
library(ggplot2)
#install.packages("plotly")
library(plotly)
library(writexl)

#Reading files

Bion_Am<-read.csv("Bionomics_Americas.csv")
Bion_Af<-read.csv("Bionomics_Africa.csv")
Bion_AsPac<-read.csv("Bionomics_Asia_Pacific.csv")

#Clean datasets


#Select relevant variables

#Af_clean<-select(Bion_Af,country, species, indoor_host, outdoor_host, combined_host, host_unit, indoor_biting, outdoor_biting, indoor_outdoor_biting_units, combined_1830_2130, combined_2130_0030, combined_0030_0330, combined_0330_0630,citation)

#Am_clean<-select(Bion_Am,country, species, indoor_host, outdoor_host, combined_host, host_unit, indoor_biting, outdoor_biting, indoor_outdoor_biting_units, combined_1830_2130, combined_2130_0030, combined_0030_0330, combined_0330_0630,citation)

#AsPac_clean<-select(Bion_AsPac,country, species, indoor_host, outdoor_host, combined_host, host_unit, indoor_biting, outdoor_biting, indoor_outdoor_biting_units, combined_1830_2130, combined_2130_0030, combined_0030_0330, combined_0330_0630,citation)

#Remove NAs from certain columns

#Removing entries with no info on HBI

#Af_HBI<-Af_clean[!with(Af_clean,is.na(indoor_host)& is.na(outdoor_host)),]

#Am_HBI<-Am_clean[!with(Am_clean,is.na(indoor_host)& is.na(outdoor_host)),]

#AsPac_HBI<-AsPac_clean[!with(AsPac_clean,is.na(indoor_host)& is.na(outdoor_host)),]

#Removing entries with no info on exophily/endophily

#Af_in_out<-Af_clean[!with(Af_clean,is.na(indoor_biting)& is.na(outdoor_biting)),]

#Am_in_out<-Am_clean[!with(Am_clean,is.na(indoor_biting)& is.na(outdoor_biting)),]

#AsPac_in_out<-AsPac_clean[!with(AsPac_clean,is.na(indoor_biting)& is.na(outdoor_biting)),]

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
  Bion_Af %>% mutate(region = 'Africa') %>%
  bind_rows(Bion_Am %>% mutate(region = 'Americas')) %>%
  bind_rows(Bion_AsPac %>% mutate(region = 'Asian Pacific'))

#Calculating the overall HBI (including outdoors, indoors, combine and others)

df1 <-df%>%rowwise()%>% mutate(total_host =sum(indoor_host_n,outdoor_host_n, combined_host_n,
                       other_host_n,na.rm=TRUE)/
                    sum(indoor_host_total, outdoor_host_total, 
                        combined_host_total,
                        other_host_total, na.rm=TRUE)*100)

# Make a quick and dirty plot using "matches" instead of "contains"

#We now use the df2 dataframe, constructed based on the "bionomics" file

#df2 is a version of the original file with less variables

#it also has complementary information to fill blank spaces of interest

pd <- df2%>%
  dplyr::select(hbi = total_host,
                indoor_biting, region,species, host_unit,
                matches('combined_|indoor_|outdoor_')) %>%
  tidyr::gather(key, value, combined_1830_2130:combined_0330_0630|indoor_1830_2130:indoor_0330_0630|outdoor_1830_2130:outdoor_0330_0630) %>%
  mutate(key = gsub('combined_|indoor_|outdoor_', '', key))%>%
  filter(value!="")
  

#write_xlsx(df1,"C:\\Users\\juanc\\Desktop\\Seminarios\\Clases\\Rstudio\\test1\\Residual_transmission\\bionomics.xlsx")


#Trying Plotly

fig3D <- plot_ly(pd, x = ~key, y = ~hbi, z = ~indoor_biting)%>%
  add_markers(color= ~species)
fig3D

