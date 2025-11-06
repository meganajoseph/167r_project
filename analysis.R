# import libraries
library(tidyverse)

# import data
real_estate <- read.csv("Real_Estate_Sales_2001-2023_GL.csv")

# take columns we want
real_estate_simp <- real_estate %>% select("List.Year", "Town", "Assessed.Value", "Sale.Amount", "Sales.Ratio", "Property.Type", "Residential.Type")%>%
  filter(!is.na(List.Year) & List.Year != "" & !is.na(Town) & Town != "" & !is.na(Assessed.Value) & Assessed.Value != "" & !is.na(Sale.Amount) & Sale.Amount != "" & !is.na(Sales.Ratio) & Sales.Ratio != "" & !is.na(Property.Type) & Property.Type != "" &!is.na(Residential.Type) & Residential.Type!="" )

# clean data


# descriptive statistics over entire data set
mean_sp <- mean(real_estate_simp$Sale.Amount)
mean_av <- mean(real_estate_simp$Assessed.Value)
mean_sr <- mean(real_estate_simp$Sales.Ratio)

# min

# max

# 1st q

# median

# 3rd q