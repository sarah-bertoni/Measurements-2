# Name: Cedric Fontaine #

# Clears the log space and saved datasets #

cat("\f")
rm(list = ls())

library(tidyverse)
library(haven)
library(readxl)

data <- read_excel("Data_Migration.xlsx")

data_colonial <- data %>%
  mutate(is_french_colony = if_else(Migrant_Country %in% c(
    "Algérie", "Maroc", "Tunisie", "Mauritanie", "Mali", "Niger", "Tchad", "Guinée", "Côte d'Ivoire", 
    "Burkina Faso", "Bénin", "Sénégal", "Djibouti", "Madagascar", "Comores", "Gabon", 
    "République centrafricaine", "Congo", "Liban", "Syrie", "Vietnam", "Laos", "Cambodge"
  ), 1, 0))

data_colonial_2 <- data_colonial %>% filter(Year %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) %>% 
  filter(!Migrant_Country %in% c(
    "Allemagne", "Autriche", "Belgique", "Bulgarie", "Chypre", "Croatie", "Danemark", "Espagne", 
    "Estonie", "Finlande", "France", "Grèce", "Hongrie", "Irlande", "Italie", "Lettonie", 
    "Lituanie", "Luxembourg", "Malte", "Pays-Bas", "Pologne", "Portugal", "République tchèque", 
    "Roumanie", "République Slovaque", "Slovénie", "Suède", "Ex-Tchécoslovaquie", "Monaco", "Islande", "Monde", "Monde non specifié",
    "Royaume-Uni"
  ))

  
  
View(data_colonial_2)


model1 <- lm(data=data_colonial_2, People~is_french_colony)
summary(model1)


complementary_data <- read_excel("complementary_data.xlsx")


library(tidyr)

data_long <- complementary_data%>%
  pivot_longer(cols = c(5:9),
               names_to = "Year", 
               values_to = "Value")
View(data_long)


data_wide <- data_long %>%
  pivot_wider(names_from = `Series Name`, values_from = Value)

View(data_wide)
new <- data_wide %>%
  select(-`Population, total`) %>%  
  filter(!is.na(`Age dependency ratio, young`)) %>% 
  mutate(Year = as.numeric(Year))


# Perform the left join
combined_data <- left_join(data_colonial_2, new, by = c("Year","Migrant_country_abb") )

new2 <- data_wide %>%
  select(-`Age dependency ratio, young`) %>%  
  filter(!is.na(`Population, total`)) %>% 
  mutate(Year = as.numeric(Year))  
  
  
# Perform the left join
combined_data2 <- left_join(combined_data, new2, by = c("Year","Migrant_country_abb") )

final <- combined_data2 %>% 
  filter(Year %in% c(2015, 2016, 2017, 2018, 2019))

library(writexl)
write_xlsx(final, "D:/A A PPD PSE Semester 1/Measurement/final.xlsx")  

model2 <- lm(data=final, log(People+1)~is_french_colony+ `Population, total` + `Age dependency ratio, young`+as.factor(Year))

summary(model2)
