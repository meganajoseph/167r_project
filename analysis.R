# import libraries
library(tidyverse)
library(ggplot2)

# import data
real_estate <- read.csv("Real_Estate_Sales_2001-2023_GL.csv")

# make sale.ratio numeric
real_estate$Sales.Ratio <- as.numeric(cleaned_real_estate$Sales.Ratio)



# clean data
# take columns we want
real_estate_simp <- real_estate %>% select("List.Year", "Town", "Assessed.Value", "Sale.Amount", "Sales.Ratio", "Property.Type", "Residential.Type") %>%
  filter(!is.na(List.Year) & List.Year != "" & 
           !is.na(Town) & Town != "" & 
           !is.na(Assessed.Value) & Assessed.Value != "" &
           !is.na(Sale.Amount) & Sale.Amount != "" & Sale.Amount >= 2000 &
           !is.na(Sales.Ratio) & Sales.Ratio != "" & 
           !is.na(Property.Type) & Property.Type != "" &
           !is.na(Residential.Type) & Residential.Type!="" ) %>%
  filter( Town == "Stamford" | Town == "Westport" | Town == "Cheshire" | Town == "Sprague")

# drop duplicates
real_estate_dropped <- real_estate_simp[!duplicated(real_estate_simp), ]

# check the unique entries in Property Type column
unique(real_estate_dropped$Property.Type) 
# want to remove all of the irrelevant property types to our study (residential only)
cleaned_real_estate <- real_estate_dropped[!real_estate_dropped$Property.Type %in% c("Industrial", "Commercial", "Vacant Land", "Public Utility", ""), ]
# check that the irrelevant types are removed
unique(cleaned_real_estate$Property.Type)




# descriptive statistics over entire data set
mean_sp <- mean(cleaned_real_estate$Sale.Amount)
mean_av <- mean(cleaned_real_estate$Assessed.Value)
mean_sr <- mean(cleaned_real_estate$Sales.Ratio)

# min
min_sp <- min(cleaned_real_estate$Sale.Amount)
min_av <- min(cleaned_real_estate$Assessed.Value)
min_sr <- min(cleaned_real_estate$Sales.Ratio)

# max
max_sp <- max(cleaned_real_estate$Sale.Amount)
max_av <- max(cleaned_real_estate$Assessed.Value)
max_sr <- max(cleaned_real_estate$Sales.Ratio)

# 1st q
first_quantile_sp <- quantile(cleaned_real_estate$Sale.Amount, probs = 0.25)
first_quantile_av <- quantile(cleaned_real_estate$Assessed.Value, probs = 0.25)
first_quantile_sr <- quantile(cleaned_real_estate$Sales.Ratio, probs = 0.25)

# median
median_sp <- median(cleaned_real_estate$Sale.Amount)
median_av <- median(cleaned_real_estate$Assessed.Value)
median_sr <- median(cleaned_real_estate$Sales.Ratio)

# 3rd q
third_quantile_sp <- quantile(cleaned_real_estate$Sale.Amount, probs = 0.75)
third_quantile_av <- quantile(cleaned_real_estate$Assessed.Value, probs = 0.75)
third_quantile_sr <- quantile(cleaned_real_estate$Sales.Ratio, probs = 0.75)



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
# add caption explaining log values
ggplot(data=cleaned_real_estate, aes(log(Sale.Amount))) +
  geom_histogram(bins=20) + 
  labs(x = "Log Sale Price",
       y = "Frequency",
       title = "Distribution of log of sale price") 

# visualize the sales ratio among towns across years
ratio_df <- cleaned_real_estate %>% 
  group_by(Town, List.Year) %>%
  summarise(mean_sr_ingroup = mean(Sales.Ratio, na.rm = TRUE))

ggplot(ratio_df, aes(x = List.Year, y = mean_sr_ingroup, color = Town)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(ratio_df$List.Year))+
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Average sales ratio by town",
       x = "Year", 
       y = "Average Sales Ratio",
       color = "Town")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust = .5))

# generate biannual boxplots (with some redundant functions for now, will consolidate later)
data <- real_estate
unique(data$Property.Type) #check the unique entries in Property Type column
data_clean <- data[!data$Property.Type %in% c("Industrial", "Commercial", "Vacant Land", "Public Utility", ""), ]


#pdf hartford

# Ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Sale.Amount <- as.numeric(data_clean$Sale.Amount)

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

# prepare list of boxplot data
boxplot_data <- list()
valid_labels <- c()

for (i in seq_along(intervals)) {
  y <- intervals[i]
  years_interval <- y:min(y+1, end_year)
  
  # subset hartford data
  data_subset <- data_clean[data_clean$Town == "Hartford" & data_clean$List.Year %in% years_interval, ]
  sales <- na.omit(data_subset$Sale.Amount)
  
  if (length(sales) > 0) {
    boxplot_data[[length(boxplot_data) + 1]] <- sales
    valid_labels <- c(valid_labels, interval_labels[i])
  }
}

if (length(boxplot_data) == 0) stop("No Sale.Amount data found for Hartford.")

# set consistent y-axis limits
y_limits <- range(unlist(boxplot_data))

# open pdf
pdf("Hartford_SaleAmounts_2yr_pairs.pdf", width = 10, height = 12)

# set up 2 columns and enough rows to fit all intervals
n <- length(boxplot_data)
rows <- ceiling(n / 2)
par(mfrow = c(rows, 2), mar = c(4, 4, 3, 1))  # adjust margins

# plot each interval
for (i in seq_along(boxplot_data)) {
  boxplot(boxplot_data[[i]],
          main = valid_labels[i],
          ylab = "Sale Amount",
          col = "lightblue",   # different color for Hartford
          ylim = y_limits)
}

dev.off()

#westport

# ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Sale.Amount <- as.numeric(data_clean$Sale.Amount)

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

# prepare list of boxplot data
boxplot_data <- list()
valid_labels <- c()

for (i in seq_along(intervals)) {
  y <- intervals[i]
  years_interval <- y:min(y+1, end_year)
  
  # subset westport data
  data_subset <- data_clean[data_clean$Town == "Westport" & data_clean$List.Year %in% years_interval, ]
  sales <- na.omit(data_subset$Sale.Amount)
  
  if (length(sales) > 0) {
    boxplot_data[[length(boxplot_data) + 1]] <- sales
    valid_labels <- c(valid_labels, interval_labels[i])
  }
}

if (length(boxplot_data) == 0) stop("No Sale.Amount data found for Westport.")

# set consistent y-axis limits
y_limits <- range(unlist(boxplot_data))

# open pdf
pdf("Westport_SaleAmounts_2yr_pairs.pdf", width = 10, height = 12)

# set up 2 columns and enough rows to fit all intervals
n <- length(boxplot_data)
rows <- ceiling(n / 2)
par(mfrow = c(rows, 2), mar = c(4, 4, 3, 1))  # adjust margins

# plot each interval
for (i in seq_along(boxplot_data)) {
  boxplot(boxplot_data[[i]],
          main = valid_labels[i],
          ylab = "Sale Amount",
          col = "lightgreen",   # color for Westport
          ylim = y_limits)
}

dev.off()

#cheshire

# ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Sale.Amount <- as.numeric(data_clean$Sale.Amount)

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

# prepare list of boxplot data
boxplot_data <- list()
valid_labels <- c()

for (i in seq_along(intervals)) {
  y <- intervals[i]
  years_interval <- y:min(y+1, end_year)
  
  # subset cheshire data for this interval
  data_subset <- data_clean[data_clean$Town == "Cheshire" & data_clean$List.Year %in% years_interval, ]
  sales <- na.omit(data_subset$Sale.Amount)
  
  if (length(sales) > 0) {
    boxplot_data[[length(boxplot_data) + 1]] <- sales
    valid_labels <- c(valid_labels, interval_labels[i])
  }
}

if (length(boxplot_data) == 0) stop("No Sale.Amount data found for Cheshire.")

# set consistent y-axis limits
y_limits <- range(unlist(boxplot_data))

# open pdf
pdf("Cheshire_SaleAmounts_2yr_pairs.pdf", width = 10, height = 12)

# set up 2 columns and enough rows to fit all intervals
n <- length(boxplot_data)
rows <- ceiling(n / 2)
par(mfrow = c(rows, 2), mar = c(4, 4, 3, 1))  # adjust margins

# plot each interval
for (i in seq_along(boxplot_data)) {
  boxplot(boxplot_data[[i]],
          main = valid_labels[i],
          ylab = "Sale Amount",
          col = "pink",   # color for Cheshire
          ylim = y_limits)
}

dev.off()

###sprague

# ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Sale.Amount <- as.numeric(data_clean$Sale.Amount)

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

# prepare list of boxplot data
boxplot_data <- list()
valid_labels <- c()

for (i in seq_along(intervals)) {
  y <- intervals[i]
  years_interval <- y:min(y+1, end_year)
  
  # subset sprague data for this interval
  data_subset <- data_clean[data_clean$Town == "Sprague" & data_clean$List.Year %in% years_interval, ]
  sales <- na.omit(data_subset$Sale.Amount)
  
  if (length(sales) > 0) {
    boxplot_data[[length(boxplot_data) + 1]] <- sales
    valid_labels <- c(valid_labels, interval_labels[i])
  }
}

if (length(boxplot_data) == 0) stop("No Sale.Amount data found for Sprague.")

# set consistent y-axis limits
y_limits <- range(unlist(boxplot_data))

# open pdf
pdf("Sprague_SaleAmounts_2yr_pairs.pdf", width = 10, height = 12)

# set up 2 columns and enough rows to fit all intervals
n <- length(boxplot_data)
rows <- ceiling(n / 2)
par(mfrow = c(rows, 2), mar = c(4, 4, 3, 1))  # adjust margins

# plot each interval
for (i in seq_along(boxplot_data)) {
  boxplot(boxplot_data[[i]],
          main = valid_labels[i],
          ylab = "Sale Amount",
          col = "lightgoldenrod",   # color for Sprague
          ylim = y_limits)
}

dev.off()


#####side by side with labels
# ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Sale.Amount <- as.numeric(data_clean$Sale.Amount)

# towns and colors
towns <- c("Hartford", "Westport", "Cheshire", "Sprague")
colors <- c("lightblue", "lightgreen", "pink", "gold")

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

for (t_index in seq_along(towns)) {
  town <- towns[t_index]
  col <- colors[t_index]
  
  # prepare data: one list per interval
  boxplot_data <- list()
  valid_labels <- c()
  
  for (i in seq_along(intervals)) {
    y <- intervals[i]
    years_interval <- y:min(y+1, end_year)
    
    data_subset <- data_clean[data_clean$Town == town & data_clean$List.Year %in% years_interval, ]
    sales <- na.omit(data_subset$Sale.Amount)
    
    if (length(sales) > 0) {
      boxplot_data[[length(boxplot_data) + 1]] <- sales
      valid_labels <- c(valid_labels, interval_labels[i])
    }
  }
  
  if (length(boxplot_data) == 0) {
    warning(paste("No Sale.Amount data found for", town))
    next
  }
  
  # determine y-axis limits
  y_limits <- range(unlist(boxplot_data))
  
  # open pdf
  pdf_filename <- paste0(town, "_SaleAmounts_2yr_trend.pdf")
  pdf(pdf_filename, width = 12, height = 6)
  
  # single boxplot with all intervals side by side
  boxplot(boxplot_data,
          names = valid_labels,
          col = col,
          ylim = y_limits,
          las = 2,          # vertical x-axis labels
          ylab = "Sale Amount",
          main = paste("Sale Amount Trends -", town))
  
  dev.off()
  cat("PDF created for", town, ":", pdf_filename, "\n")
}

######## side by side assessed values

# ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Assessed.Value <- as.numeric(data_clean$Assessed.Value)

# towns and colors
towns <- c("Hartford", "Westport", "Cheshire", "Sprague")
colors <- c("lightblue", "lightgreen", "pink", "gold")

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

for (t_index in seq_along(towns)) {
  town <- towns[t_index]
  col <- colors[t_index]
  
  # prepare data: one list per interval
  boxplot_data <- list()
  valid_labels <- c()
  
  for (i in seq_along(intervals)) {
    y <- intervals[i]
    years_interval <- y:min(y+1, end_year)
    
    data_subset <- data_clean[data_clean$Town == town & data_clean$List.Year %in% years_interval, ]
    values <- na.omit(data_subset$Assessed.Value)
    
    if (length(values) > 0) {
      boxplot_data[[length(boxplot_data) + 1]] <- values
      valid_labels <- c(valid_labels, interval_labels[i])
    }
  }
  
  if (length(boxplot_data) == 0) {
    warning(paste("No Assessed.Value data found for", town))
    next
  }
  
  # determine y-axis limits
  y_limits <- range(unlist(boxplot_data))
  
  # open PDF
  pdf_filename <- paste0(town, "_AssessedValues_2yr_trend.pdf")
  pdf(pdf_filename, width = 12, height = 6)
  
  # single boxplot with all intervals side by side
  boxplot(boxplot_data,
          names = valid_labels,
          col = col,
          ylim = y_limits,
          las = 2,          # vertical x-axis labels
          ylab = "Assessed Value",
          main = paste("Assessed Value Trends -", town))
  
  dev.off()
  cat("PDF created for", town, ":", pdf_filename, "\n")
}

################ sales ratio

# ensure numeric columns
data_clean$List.Year <- as.numeric(data_clean$List.Year)
data_clean$Sales.Ratio <- as.numeric(data_clean$Sales.Ratio)

# towns and colors
towns <- c("Hartford", "Westport", "Cheshire", "Sprague")
colors <- c("lightblue", "lightgreen", "pink", "gold")

# define 2-year intervals
start_year <- 2001
end_year <- 2023
intervals <- seq(start_year, end_year, by = 2)
interval_labels <- sapply(intervals, function(y) paste0(y, "-", min(y+1, end_year)))

for (t_index in seq_along(towns)) {
  town <- towns[t_index]
  col <- colors[t_index]
  
  # prepare data: one list per interval
  boxplot_data <- list()
  valid_labels <- c()
  
  for (i in seq_along(intervals)) {
    y <- intervals[i]
    years_interval <- y:min(y+1, end_year)
    
    data_subset <- data_clean[data_clean$Town == town & data_clean$List.Year %in% years_interval, ]
    values <- na.omit(data_subset$Sales.Ratio)
    
    if (length(values) > 0) {
      boxplot_data[[length(boxplot_data) + 1]] <- values
      valid_labels <- c(valid_labels, interval_labels[i])
    }
  }
  
  if (length(boxplot_data) == 0) {
    warning(paste("No Sales.Ratio data found for", town))
    next
  }
  
  # determine y-axis limits
  y_limits <- range(unlist(boxplot_data))
  
  # open pdf
  pdf_filename <- paste0(town, "_SalesRatio_2yr_trend.pdf")
  pdf(pdf_filename, width = 12, height = 6)
  
  # single boxplot with all intervals side by side
  boxplot(boxplot_data,
          names = valid_labels,
          col = col,
          ylim = y_limits,
          las = 2,          # vertical x-axis labels
          ylab = "Sales Ratio",
          main = paste("Sales Ratio Trends -", town))
  
  dev.off()
  cat("PDF created for", town, ":", pdf_filename, "\n")
}



# geographical heatmap
locations <- real_estate %>% select("Town", "Assessed.Value", "Sale.Amount", "Location") %>%
  filter(!is.na(Town) & Town != "" &
           !is.na(Assessed.Value) & Assessed.Value != "" & 
           !is.na(Sale.Amount) & Sale.Amount != "" & 
           !is.na(Location) & Location != "")

loc_split <- within(locations, Location <- data.frame(do.call('rbind', strsplit(as.character(Location), " "))))
loc_split$Latitude <- as.numeric(gsub("\\(", "", loc_split$Location$X2))
loc_split$Longitude <- as.numeric(gsub("\\)", "", loc_split$Location$X3))
loc_split <- loc_split %>% 
  select("Town", "Assessed.Value", "Sale.Amount", "Latitude", "Longitude") %>%
  filter(Latitude < -69 & Latitude > -74 & Longitude < 42.5 & Longitude > 40)


new_dir <- "graphs/heatmaps"
if (!dir.exists(new_dir)) {
  dir.create(new_dir, recursive = TRUE)
}

pdf_path <- file.path(new_dir, "sale_price_ct.pdf")
pdf(pdf_path) 
ggplot(loc_split, aes(x=Latitude, y=Longitude, z=Sale.Amount)) +
  stat_summary_2d(fun = median, bins = 40) +
  scale_fill_gradientn(colors=terrain.colors(10), name="Sale Price") +
  labs(title="Density of Sale Price in Connecticut")
dev.off()

ggplot(loc_split, aes(x=Latitude, y=Longitude, z=Assessed.Value)) +
  stat_summary_2d(fun = median, bins = 40) +
  scale_fill_gradientn(colors=terrain.colors(10), name="Assessed Value") +
  labs(title="Density of Assessed Value in Connecticut")

# john heatmap additions
locations <- real_estate %>% select("Town", "Assessed.Value", "Sale.Amount", "Location", "Sales.Ratio") %>%
  filter(!is.na(Town) & Town != "" &
           !is.na(Assessed.Value) & Assessed.Value != "" &
           !is.na(Sale.Amount) & Sale.Amount != "" &
           !is.na(Sales.Ratio) & Sales.Ratio != "" &
           !is.na(Location) & Location != "")



loc_split <- within(locations, Location <- data.frame(do.call('rbind', strsplit(as.character(Location), " "))))
loc_split$Latitude <- as.numeric(gsub("\\(", "", loc_split$Location$X2))
loc_split$Longitude <- as.numeric(gsub("\\)", "", loc_split$Location$X3))
loc_split$Sales.Ratio <- as.numeric(loc_split$Sales.Ratio)
loc_split <- loc_split %>%
  select("Town", "Assessed.Value", "Sale.Amount", "Latitude", "Longitude", "Sales.Ratio") %>%
  filter(Latitude < -69 & Latitude > -74 & Longitude < 42.5 & Longitude > 40)

ggplot(loc_split, aes(x=Longitude, y=Latitude, z=Sale.Amount)) +
  stat_summary_2d(fun = median, bins = 40) +
  scale_fill_gradientn(colors=terrain.colors(10), name="Sale Price") +
  labs(title="Density of Sale Price in Connecticut")

ggplot(loc_split, aes(x=Longitude, y=Latitude, z=Assessed.Value)) +
  stat_summary_2d(fun = median, bins = 40) +
  scale_fill_gradientn(colors=terrain.colors(10), name="Assessed Value") +
  labs(title="Density of Assessed Value in Connecticut")

ggplot(loc_split, aes(x=Longitude, y=Latitude, z=log(Sales.Ratio))) +
  stat_summary_2d(fun = median, bins = 40) +
  scale_fill_gradientn(colors=terrain.colors(10), name="Sales Ratio") +
  labs(title="Density of Sales Ratio in Connecticut")

#swap latitude longitude
temp <- loc_split$Latitude
loc_split$Latitude <- loc_split$Longitude
loc_split$Longitude <- temp

#sales price maps

# stamford sale price heatmap
stamford <- loc_split[loc_split$Town == "Stamford",]

stamford_filtered <- loc_split[
  loc_split$Town == "Stamford" &
    loc_split$Latitude >= 41 & loc_split$Latitude <= 42 &
    loc_split$Longitude >= -73.6 & loc_split$Longitude <= -73.4,
]


ggplot(stamford_filtered, aes(x=Longitude, y=Latitude, z=log(Sale.Amount))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sale Price") +
  labs(title="Density of Sale Price in Stamford")

nrow(stamford)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Stamford',])

str(loc_split)

# cheshire sale price heatmap
cheshire <- loc_split[loc_split$Town == "Cheshire",]

cheshire_filtered <- loc_split[
  loc_split$Town == "Cheshire" &
    loc_split$Latitude >= 41.4 & loc_split$Latitude <= 41.6 &
    loc_split$Longitude >= -73 & loc_split$Longitude <= -72.75,
]


ggplot(cheshire_filtered, aes(x=Longitude, y=Latitude, z=log(Sale.Amount))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sale Price") +
  labs(title="Density of Sale Price in Cheshire")

# westport sale price heatmap
westport <- loc_split[loc_split$Town == "Westport",]

westport_filtered <- loc_split[
  loc_split$Town == "Westport" &
    loc_split$Latitude >= 41.09 & loc_split$Latitude <= 41.68 &
    loc_split$Longitude >= -73.4 & loc_split$Longitude <= -72.9,
]


ggplot(westport_filtered, aes(x=Longitude, y=Latitude, z=log(Sale.Amount))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sale Price") +
  labs(title="Density of Sale Price in Westport")

nrow(westport)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Westport',])

str(loc_split)

# sprague sale price heatmap
sprague <- loc_split[loc_split$Town == "Sprague",]

sprague_filtered <- loc_split[
  loc_split$Town == "Sprague" &
    loc_split$Latitude >= 41.55 & loc_split$Latitude <= 41.66 &
    loc_split$Longitude >= -73.23 & loc_split$Longitude <= -72.04,
]


ggplot(sprague_filtered, aes(x=Longitude, y=Latitude, z=log(Sale.Amount))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sale Price") +
  labs(title="Density of Sale Price in Sprague")

nrow(sprague)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Sprague',])

str(loc_split)


# assessed heatmaps

# stamford assessed value heatmap
stamford <- loc_split[loc_split$Town == "Stamford",]

stamford_filtered <- loc_split[
  loc_split$Town == "Stamford" &
    loc_split$Latitude >= 41 & loc_split$Latitude <= 42 &
    loc_split$Longitude >= -73.6 & loc_split$Longitude <= -73.4,
]


ggplot(stamford_filtered, aes(x=Longitude, y=Latitude, z=log(Assessed.Value))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Assessed Value") +
  labs(title="Density of Assessed Value in Stamford")

nrow(stamford)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Stamford',])

str(loc_split)

# cheshire assessed value heatmap
cheshire <- loc_split[loc_split$Town == "Cheshire",]

cheshire_filtered <- loc_split[
  loc_split$Town == "Cheshire" &
    loc_split$Latitude >= 41.4 & loc_split$Latitude <= 41.6 &
    loc_split$Longitude >= -73 & loc_split$Longitude <= -72.75,
]


ggplot(cheshire_filtered, aes(x=Longitude, y=Latitude, z=log(Assessed.Value))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Assessed Value") +
  labs(title="Density of Assessed Value in Cheshire")

# westport assessed value heatmap
westport <- loc_split[loc_split$Town == "Westport",]

westport_filtered <- loc_split[
  loc_split$Town == "Westport" &
    loc_split$Latitude >= 41.09 & loc_split$Latitude <= 41.68 &
    loc_split$Longitude >= -73.4 & loc_split$Longitude <= -72.9,
]


ggplot(westport_filtered, aes(x=Longitude, y=Latitude, z=log(Assessed.Value))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Assessed Value") +
  labs(title="Density of Assessed Value in Westport")

nrow(westport)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Westport',])

str(loc_split)

# sprague assessed value heatmap
sprague <- loc_split[loc_split$Town == "Sprague",]

sprague_filtered <- loc_split[
  loc_split$Town == "Sprague" &
    loc_split$Latitude >= 41.55 & loc_split$Latitude <= 41.66 &
    loc_split$Longitude >= -73.23 & loc_split$Longitude <= -72.04,
]


ggplot(sprague_filtered, aes(x=Longitude, y=Latitude, z=log(Assessed.Value))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Assessed Value") +
  labs(title="Density of Assessed Value in Sprague")

nrow(sprague)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Sprague',])

str(loc_split)


#ratio maps

# stamford ratio heatmap
stamford <- loc_split[loc_split$Town == "Stamford",]

stamford_filtered <- loc_split[
  loc_split$Town == "Stamford" &
    loc_split$Latitude >= 41 & loc_split$Latitude <= 42 &
    loc_split$Longitude >= -73.6 & loc_split$Longitude <= -73.4,
]


ggplot(stamford_filtered, aes(x=Longitude, y=Latitude, z=log(Sales.Ratio))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sales Ratio") +
  labs(title="Density of Sales Ratio in Stamford")

nrow(stamford)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Stamford',])

str(loc_split)

# cheshire ratio heatmap
cheshire <- loc_split[loc_split$Town == "Cheshire",]

cheshire_filtered <- loc_split[
  loc_split$Town == "Cheshire" &
    loc_split$Latitude >= 41.4 & loc_split$Latitude <= 41.6 &
    loc_split$Longitude >= -73 & loc_split$Longitude <= -72.75,
]


ggplot(cheshire_filtered, aes(x=Longitude, y=Latitude, z=log(Sales.Ratio))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sales Ratio") +
  labs(title="Density of Sales Ratio in Cheshire")

# westport ratio heatmap
westport <- loc_split[loc_split$Town == "Westport",]

westport_filtered <- loc_split[
  loc_split$Town == "Westport" &
    loc_split$Latitude >= 41.09 & loc_split$Latitude <= 41.68 &
    loc_split$Longitude >= -73.4 & loc_split$Longitude <= -72.9,
]


ggplot(westport_filtered, aes(x=Longitude, y=Latitude, z=log(Sales.Ratio))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sales Ratio") +
  labs(title="Density of Sales Ratio in Westport")

nrow(westport)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Westport',])

str(loc_split)

# sprague ratio heatmap
sprague <- loc_split[loc_split$Town == "Sprague",]

sprague_filtered <- loc_split[
  loc_split$Town == "Sprague" &
    loc_split$Latitude >= 41.55 & loc_split$Latitude <= 41.66 &
    loc_split$Longitude >= -73.23 & loc_split$Longitude <= -72.04,
]


ggplot(sprague_filtered, aes(x=Longitude, y=Latitude, z=log(Sales.Ratio))) +
  stat_summary_2d(fun = median, bins = 50) +
  scale_fill_gradientn(colors=terrain.colors(10), name="log Sales Ratio") +
  labs(title="Density of Sales Ratio in Sprague")

nrow(sprague)
unique(loc_split$Town)
head(loc_split[loc_split$Town=='Sprague',])

str(loc_split)



# advanced analysis
# regression model
SS_model <- lm(formula =  Sale.Amount ~ Assessed.Value * Town, data = cleaned_real_estate)
summary(SS_model)
SS_model_log <- lm(log(Sale.Amount) ~ log(Assessed.Value) * Town, data = (cleaned_real_estate %>% filter( Assessed.Value > 0 & Sale.Amount > 0)))
summary(SS_model_log)

ggplot(cleaned_real_estate, aes(x = log(Assessed.Value), y = log(Sale.Amount), color = Town)) +
  geom_point(size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(title = "Interaction",
       x = "Log assessed value",
       y = "Log sale amount") +
  facet_wrap(~ Town, nrow = NULL, ncol = NULL, scales = "fixed")

#assumption
mod_residuals <- residuals(SS_model_log)
mod_fitted <- fitted(SS_model_log)
ggplot(data.frame(residual = mod_residuals), aes(sample = residual)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Quantile-quantile plot of residuals",
       x = "Theoretical quantile (Normal distribution)",
       y = "Sample quantile")

#residual histogram
ggplot(data = NULL, aes(x = residuals(SS_model_log))) +
  geom_histogram(bins = 30, fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(title = "residual histogram",
       x = "residual",
       y = "frequency")


resid_dataframe <- data.frame(time = 1:length(residuals(SS_model_log)),
                            resid = residuals(SS_model_log),
                            fittedvalue = fitted(SS_model_log))
#make the redisual& Time plot
ggplot(resid_dataframe, aes(x = time,y = resid))+
  geom_point() +
  geom_hline(yintercept = 0) +
  labs( title = "Residual vs. Time",
        x = "Time",
        y = "Residual value")

#make the redisual& fitted plot

ggplot(resid_dataframe, aes(x = fittedvalue,y = resid))+
  geom_point() +
  geom_hline(yintercept = 0) +
  labs( title = "Residual vs. fitted value",
        x = "Fitted value",
        y = "Residual value")

