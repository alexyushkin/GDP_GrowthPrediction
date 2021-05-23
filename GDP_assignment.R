# Title: GDP Growth Prediction
# Author: Alexey Yushkin
# Date: May 1, 2021

# Upload libraries
library("tidyverse")
library("forecast")
library("stats")

# Deactivate warnings
options(warn=-1)

# Upload file
file <- read_csv("./data/20210424_challenge_dataset.csv")

# Choose a Country
country = "United States"

# Create tibble for the County
data <- file %>% 
  filter(countryname == country)

# Get rid of empty columns
emptycols <- sapply(data, function (x) all(is.na(x)))
data <- data[!emptycols]

# Get rid of columns that have more than 50% of missing data
n_rows = nrow(data)
halfemptycols <- sapply(data, function (x) sum(is.na(x)) > n_rows / 2)
data <- data[!halfemptycols]

# Get rid of columns without variability
data <- data[vapply(data, function(x) length(unique(x)) > 1, logical(1L))]

# Get rid of non-numeric columns
data <- data %>% 
  dplyr::select(where(is.numeric))

# Fill in missing values with previous values first, than with following values
data <- data %>%
  fill(colnames(data), .direction="downup")

# Remove year column which doesn't have useful information
data <- subset(data, select=-year)

# Create vector of column names
col_names <- colnames(data)

# Extrapolate values for the next year using forecast library
for (col in col_names) {
  data[n_rows + 1, col] <- predict(HoltWinters(as.ts(data[1:n_rows, col]), beta=FALSE, gamma=FALSE), 1)[1]
}

# Print expected GDP growth
gdp_growth <- data["WB_ny_gdp_mktp_kd_zg"]
print(paste0("Expected GDP Growth: ", round(gdp_growth[nrow(gdp_growth),], 2), "%"))

# Create formula for using in linear regression
col_names <- col_names[col_names != "WB_ny_gdp_mktp_kd_zg"]
cols <- paste(col_names, collapse = "+")
form <- paste("WB_ny_gdp_mktp_kd_zg", "~", cols, sep=" ")

# Train model
model <- lm(formula=form, data=data)

# Predict GDP growth using the linear model
gdp_gr_pred <- predict(model, data[n_rows + 1, col_names])
print(paste0("Predicted GDP Growth: ", round(gdp_gr_pred, 2), "%"))
