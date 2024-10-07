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

# select only needed variables (Country name, ISO, Year, Maternal mortality) and select for only years <2018
df_trend <- rawdat %>% select(country_name, ISO, Year, Maternal.Mortality) %>% 
  filter(Year < 2018)

#isolate only countries in which there was an increase in maternal mortality between 2000 and 2017 and create a vector of their names
up_trend <- df_trend %>% 
  filter(Year == 2000 | Year == 2017) %>% 
  arrange(ISO, Year) %>% 
  group_by(ISO) %>% 
  mutate(diffmatmor = Maternal.Mortality - Maternal.Mortality[1L]) %>% 
  filter(diffmatmor > 0)

#Remove rows with missing data
up_trend <- up_trend[1:13,]

#make a vector with all of our countries to include in the figure and then subset our df_trend dataset for these countries
countries_trend <- c("Brunei", "Canada", "Dominican Republic", "Haiti", 
                    "Jamaica", "Kuwait"            
                    , "Lebanon", "Libya",             
                    "Saint Lucia","Mauritius"         
                    ,"Syria", "United States"     
                      ,"Venezuela")
final_trend <- df_trend %>% filter(country_name %in% countries_trend)

#Plot trend using ggplot
ggplot(final_trend, aes(x = Year, y = Maternal.Mortality, color = country_name)) +
  geom_line(size = 1) +        
  geom_point() +               
  labs(
    title = "Maternal Mortality Trends (2000-2017)",
    x = "Year",
    y = "Maternal Mortality Rate",
    color = "Country"
  ) +
  theme_minimal() +           
  scale_color_hue()
