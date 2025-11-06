
############################################ (Filtering out the non-residential properties)

data <- read.csv("Real_Estate_2001-2023_GL.csv", sep = ",") #read csv file
class(data) #check that it saved as data frame

unique(data$Property.Type) #check the unique entries in Property Type column
data_clean <- data[!data$Property.Type %in% c("Industrial", "Commercial", "Vacant Land", "Public Utility", ""), ] #want to 
#remove all of the irrelevant property types to our study (residential only)
unique(data_clean$Property.Type) #check that the irrelevant types are removed

#nrow(data_clean)
#length(unique(data_clean$Property.Type))

#############################################