# Set the path to the parent directory of RR classes
setwd("Z:\\File folders\\Teaching\\Reproducible Research\\2023\\Repository\\RRcourse2023\\6. Coding and documentation")

# Load necessary libraries
library(readxl)
library(dplyr)
library(stringr)
library(Hmisc)

# Import data from the O*NET database
task_data <- read.csv("Data\\onet_tasks.csv")

# Extract the first digit of the ISCO code
task_data$isco08_1dig <- as.numeric(str_sub(task_data$isco08, 1, 1))

# Aggregate task data by 1-digit ISCO code
agg_task_data <- aggregate(task_data[, grep("^t_", names(task_data))],
                           by = list(task_data$isco08_1dig),
                           FUN = mean, na.rm = TRUE)
agg_task_data$Group.1 <- agg_task_data$Group.1 #Rename the group column to match.

# Read employment data from Eurostat
sheet_names <- paste0("ISCO", 1:9)
isco_data_list <- lapply(sheet_names, function(sheet) {
  read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = sheet) %>%
    mutate(ISCO = as.numeric(gsub("ISCO", "", sheet)))
})

# Combine all ISCO data into one data frame
all_data <- bind_rows(isco_data_list)

# Function to calculate total employment and shares for a country
calculate_shares <- function(data, country_name) {
  country_cols <- data[, grepl(country_name, names(data))]
  total <- rowSums(country_cols)
  data[[paste0("total_", country_name)]] <- rep(total, 9)
  data[[paste0("share_", country_name)]] <- data[[country_name]] / data[[paste0("total_", country_name)]]
  return(data)
}

# Countries to process
countries <- c("Belgium", "Spain", "Poland")

# Calculate employment shares for each country
for (country in countries) {
  all_data <- calculate_shares(all_data, country)
}

# Merge employment and task data
combined_data <- left_join(all_data, agg_task_data, by = c("ISCO" = "Group.1"))

# Task variables of interest
task_vars <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

# Function to standardize task variables
standardize_tasks <- function(data, country_name, task_var) {
  share_col <- paste0("share_", country_name)
  temp_mean <- wtd.mean(data[[task_var]], data[[share_col]], na.rm = TRUE)
  temp_sd <- sqrt(wtd.var(data[[task_var]], data[[share_col]], na.rm = TRUE))
  std_col_name <- paste0("std_", country_name, "_", gsub("t_", "", task_var))
  data[[std_col_name]] <- (data[[task_var]] - temp_mean) / temp_sd
  return(data)
}

# Standardize task variables for each country
for (country in countries) {
  for (task_var in task_vars) {
    combined_data <- standardize_tasks(combined_data, country, task_var)
  }
}

# Calculate NRCA (Non-routine cognitive analytical)
for (country in countries) {
  std_cols <- paste0("std_", country, "_", gsub("t_", "", task_vars))
  combined_data[[paste0(country, "_NRCA")]] <- rowSums(combined_data[, std_cols])
}

# Function to standardize NRCA
standardize_nrca <- function(data, country_name) {
  nrca_col <- paste0(country_name, "_NRCA")
  share_col <- paste0("share_", country_name)
  temp_mean <- wtd.mean(data[[nrca_col]], data[[share_col]], na.rm = TRUE)
  temp_sd <- sqrt(wtd.var(data[[nrca_col]], data[[share_col]], na.rm = TRUE))
  std_nrca_col <- paste0("std_", country_name, "_NRCA")
  data[[std_nrca_col]] <- (data[[nrca_col]] - temp_mean) / temp_sd
  return(data)
}

# Standardize NRCA for each country
for (country in countries) {
  combined_data <- standardize_nrca(combined_data, country)
}

# Calculate weighted NRCA
for (country in countries) {
  std_nrca_col <- paste0("std_", country, "_NRCA")
  share_col <- paste0("share_", country)
  multip_col <- paste0("multip_", country, "_NRCA")
  combined_data[[multip_col]] <- combined_data[[std_nrca_col]] * combined_data[[share_col]]
}

# Aggregate weighted NRCA by time
agg_nrca_list <- lapply(countries, function(country) {
  multip_col <- paste0("multip_", country, "_NRCA")
  aggregate(combined_data[[multip_col]],
            by = list(combined_data$TIME),
            FUN = sum, na.rm = TRUE) %>%
    mutate(country = country)
})

agg_nrca <- bind_rows(agg_nrca_list)

# Plot NRCA over time
for (country in countries) {
  plot_data <- agg_nrca[agg_nrca$country == country, ]
  plot(plot_data$x, main = paste("NRCA over Time -", country),
       xlab = "Time", ylab = "NRCA", xaxt = "n")
  axis(1, at = seq(1, nrow(plot_data), by = 3), labels = plot_data$Group.1[seq(1, nrow(plot_data), by = 3)])
}

# Add more tasks:
# task_vars <- c(task_vars, "t_4A3a3", "t_4C2d1i", "t_4C3d3")
