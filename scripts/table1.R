
# load packages

library(here)
library(tidyverse)
library(dplyr)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
library(here)

# load data 

here()

# load dataset from project folder "original"

rawdat <- read.csv(here("original", "final_conflict.csv"), header = TRUE)
rawdat <- as.tibble(rawdat)
summary(rawdat)

#Remove unneccessary columns
rawdat <- select(rawdat, !X.1 & !X)
  
  
#Rename columns
rawdat_renamed <- rawdat
rawdat_renamed <- rawdat_renamed %>% rename(Total_Droughts = total_droughts, Total_Earthquakes = total_earthquakes,Total_Conflict_Deaths = Total_Best, Conflict = bin_conflict, 
                                     Country_Name = country_name, Region = region,GDP_per_1000 = gdp1000, OECD_2023 = OECD2023, Population_Density = popdens, Urbanization_Rate = urban,
                                     Age_Dependency = agedep, Male_Education_Rate = male_edu, Ambient_Temperature = temp, Rainfall_1000_Year_Event = rainfall1000, 
                                    Maternal_Mortality = Maternal.Mortality, Infant_Mortality = Infant.Mortality, Neonatal_Mortality = Neonatal.Mortality, Under_5_Years_Mortality = Under.5.Mortality)

#Make a Table 1 using our data
table <- rawdat %>% 
  group_by(bin_conflict) %>% 
  summarize(
    Count = n(),
    mean_GDP_Per1k = mean(gdp1000, na.rm = TRUE),
    mean_Population_Density = mean(popdens, na.rm = TRUE),
    mean_Urbanization_Rate = mean(urban, na.rm = TRUE),
    mean_Age_Dependency = mean(agedep, na.rm = TRUE),
    mean_Male_Education = mean(male_edu, na.rm = TRUE),
    mean_Ambient_Temperature = mean(temp, na.rm = TRUE),
    mean_Rainfall_Per1000Event = mean(rainfall1000, na.rm = TRUE),
    mean_Maternal_Mortality = mean(Maternal.Mortality, na.rm = TRUE),
    mean_Infant_Mortality = mean(Infant.Mortality, na.rm = TRUE),
    mean_Neonatal_Mortality = mean(Neonatal.Mortality, na.rm = TRUE),
    mean_Under5_Mortality = mean(Under.5.Mortality, na.rm = TRUE),
    mean_Total_Droughts = mean(total_droughts, na.rm = TRUE),
    mean_Total_Earthquakes = mean(total_earthquakes, na.rm = TRUE),
    mean_Total_Conflict_Deaths = mean(Total_Best, na.rm = TRUE),
    sd_GDP_Per1k = sd(gdp1000, na.rm = TRUE),
    sd_Population_Density = sd(popdens, na.rm = TRUE),
    sd_Urbanization_Rate = sd(urban, na.rm = TRUE),
    sd_Age_Dependency = sd(agedep, na.rm = TRUE),
    sd_Male_Education = sd(male_edu, na.rm = TRUE),
    sd_Ambient_Temperature = sd(temp, na.rm = TRUE),
    sd_Rainfall_Per1000Event = sd(rainfall1000, na.rm = TRUE),
    sd_Maternal_Mortality = sd(Maternal.Mortality, na.rm = TRUE),
    sd_Infant_Mortality = sd(Infant.Mortality, na.rm = TRUE),
    sd_Neonatal_Mortality = sd(Neonatal.Mortality, na.rm = TRUE),
    sd_Under5_Mortality = sd(Under.5.Mortality, na.rm = TRUE),
    sd_Total_Droughts = sd(total_droughts, na.rm = TRUE),
    sd_Total_Earthquakes = sd(total_earthquakes, na.rm = TRUE),
    sd_Total_Conflict_Deaths = sd(Total_Best, na.rm = TRUE)
  )

t_table <- as.data.frame(table)
t_table <- mutate(t_table, across(where(is.numeric), round, 2))


t_table <- as.tibble(t_table[,1:16])

pivoted_data <- t_table %>%  
  pivot_longer(
    cols = -bin_conflict,
    names_to = "Variable",
    values_to = "Value"
  ) 

pivoted_data <- pivoted_data %>% 
  pivot_wider(
    names_from = bin_conflict,
    values_from = Value
  )

pivoted_data <- as.data.frame(pivoted_data)

for (j in (2:3)) {
  for (i in (2:16)) {
    pivoted_data[i,j] <- paste(pivoted_data[i,j],"(",pivoted_data[i+14,j],")")
  }
}

  
pivoted_data <- as_tibble(pivoted_data[1:15,])
pivoted_data[,1] <- c("N", "GDP per 1000", "Population Density",
                             "Urbanization Rate", "Age Dependency",
                             "Male Education", "Ambient Temperature"
                             ,"1000 Year Rainfall Event", "Maternal Mortality"
                             , "Infant Mortality", "Neonatal Mortality",
                             "Under 5 Years Mortality", "Total Droughts",
                             "Total Earthquakes", "Total Conflict Deaths")

pivoted_data %>% 
  kbl(caption = "Table 1 - Descriptive Characteristics of Armed Conflict Data 
      statified by conflict", col.names = c("","No Conflict", "Conflict")) %>% 
  kable_classic(full_width = F, html_font = "Times New Roman") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% 
  footnote(general = "Data is summarized over years and country and cells present the mean and (standard deviation)")
 
  
  