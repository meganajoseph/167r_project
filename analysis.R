# import libraries
library(tidyverse)

# import data
real_estate <- read.csv("Real_Estate_Sales_2001-2023_GL.csv")

# take columns we want
real_estate_simp <- real_estate %>% select("List.Year", "Town", "Assessed.Value", "Sale.Amount", "Sales.Ratio", "Property.Type", "Residential.Type") %>%
  filter(!is.na(List.Year) & List.Year != "" & 
           !is.na(Town) & Town != "" & 
           !is.na(Assessed.Value) & Assessed.Value != "" & 
           !is.na(Sale.Amount) & Sale.Amount != "" & 
           !is.na(Sales.Ratio) & Sales.Ratio != "" & 
           !is.na(Property.Type) & Property.Type != "" &
           !is.na(Residential.Type) & Residential.Type!="" ) %>%
  filter( Town == "Hartford" | Town == "Westport" | Town == "Cheshire" | Town == "Sprague")

# clean data
# drop duplicates
real_estate_dropped <- real_estate_simp[!duplicated(real_estate_simp), ]

# check the unique entries in Property Type column
unique(real_estate_dropped$Property.Type) 
# want to remove all of the irrelevant property types to our study (residential only)
cleaned_real_estate <- real_estate_dropped[!real_estate_dropped$Property.Type %in% c("Industrial", "Commercial", "Vacant Land", "Public Utility", ""), ]
# check that the irrelevant types are removed
unique(cleaned_real_estate$Property.Type)

# make sale.ratio numeric
cleaned_real_estate$Sales.Ratio <- as.numeric(cleaned_real_estate$Sales.Ratio)

# descriptive statistics over entire data set
mean_sp <- mean(cleaned_real_estate$Sale.Amount)
mean_av <- mean(cleaned_real_estate$Assessed.Value)
mean_sr <- mean(cleaned_real_estate$Sales.Ratio)

# min

# max
max_sp <- max(cleaned_real_estate$Sale.Amount)
max_av <- max(cleaned_real_estate$Assessed.Value)
max_sr <- max(cleaned_real_estate$Sales.Ratio)

# 1st q

# median

# 3rd q


# graphs

# visualize number of entries in each town
ggplot(data=cleaned_real_estate, aes(Town)) +
  geom_bar() + 
  labs(x = "Town",
       y = "Frequency",
       title = "Number of observations for each town of interest") 

# visualize number of entries in each residential type
ggplot(data=cleaned_real_estate, aes(Residential.Type)) +
  geom_bar() + 
  labs(x = "Residential Type",
       y = "Frequency",
       title = "Number of observations for each residential type") 

# visualize the distribution of sale price
ggplot(data=cleaned_real_estate, aes(Sale.Amount)) +
  geom_histogram(bins=20) + 
  labs(x = "Sale Price",
       y = "Frequency",
       title = "Distribution of sale price") 
