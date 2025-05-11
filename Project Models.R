#===================================
# Change file paths for your machine
#===================================
rm(list =ls()) # Clear environment
directory <- "C:/Users/coule/OneDrive - Bentley University/Spring 2025"
project <- "/EC382/Final Project/data"
mainpath <- (file.path(directory, project))

setwd(file.path(mainpath)) # Set working directory


#===================================
# Install and load required packages
#===================================
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("vars")
#install.packages("urca")
#install.packages("zoo")
library(readxl) # For loading excel data
library(dplyr) # For data filtering
library(lubridate) # For date differencing
library(vars) # For vector autoregressions
library(tseries) # For setting ts vars
library(urca) # For Zivot-Andrews test
library(zoo) # For adf_diff test


#=======================
# Load and prepare data
#=======================
df <- read_excel("monetary_fiscal.xlsx", sheet = "Quarterly") # Load data
df$date <- as.Date(df$date) # Set dates to date type

df$date <- as.yearqtr(df$date, format = "%Y:0%q") # format the date column

df <- df %>%
  mutate(
    d_bs = c(NA, diff(bs)),
    d_gov = c(NA, diff(gov)),
    d_unrate = c(NA, diff(unrate)),
    d_indpro = c(NA, diff(indpro)),
    d_rgdppc = c(NA, diff(rgdppc)),
    d_pce = c(NA, diff(pce)),
    d_transfers = c(NA, diff(transfers)),
    
    # Second differences for gov, unrate, and transfers
    d2_gov = c(NA, diff(d_gov)),
    d2_unrate = c(NA, diff(d_unrate)),
    d2_transfers = c(NA, diff(d_transfers)),
    d2_pce = c(NA, diff(d_pce))
  )


#===================================
# Plot variables of interest
#===================================
# Define recession start dates
recession_1990_start <- as.yearqtr("1990 Q3") # Not used
recession_2001_start <- as.yearqtr("2001 Q1") # Not used
recession_2007_start <- as.yearqtr("2007 Q4")
recession_2020_start <- as.yearqtr("2019 Q4")

# Extract 5 years before and 5 years after for each recession
df_recession_1990 <- df%>% # Not used
  filter(date >= (recession_1990_start - 5) & date <= (recession_1990_start + 5))
df_recession_2001 <- df%>% # Not used
  filter(date >= (recession_2001_start - 5) & date <= (recession_2001_start + 5))
df_recession_2007 <- df%>%
  filter(date >= (recession_2007_start - 5) & date <= (recession_2007_start + 5))
df_recession_2020 <- df%>%
  filter(date >= (recession_2020_start - 5) & date <= (recession_2020_start + 5))


#===================================
# Plot dependent variables
#===================================
df_plot <- df[df$date >= as.yearqtr("2002 Q4") & df$date <= as.yearqtr("2024 Q4"), ]

# Plot all variables on the same chart
matplot(x = df_plot$date,
        y = df_plot[, c("bs", "transfers", "gov")],
        type = "l", lty = 1, lwd = 2, col = 1:6,
        xlab = "Date", ylab = "Percent Change",
        main = "Key Dependent Macroeconomic Variables (2002 Q4 – 2024 Q4)")

# Add legend with proper labels
legend("topleft",
       legend = c("Balance Sheet", "Government Transfers", "Government Spending"),
       col = 1:6, lty = 1, lwd = 2, bty = "n")


#===================================
# Plot independent variables
#===================================
# Filter data
df_plot <- df[df$date >= as.yearqtr("2002 Q4") & df$date <= as.yearqtr("2024 Q4"), ]

# Plot all variables on the same chart
matplot(x = df_plot$date,
        y = df_plot[, c("unrate", "indpro", "rgdppc", "pce")],
        type = "l", lty = 1, lwd = 2, col = 1:6,
        xlab = "Date", ylab = "Percent Change (UR Raw %)",
        main = "Key Independent Macroeconomic Variables (2002 Q4 – 2024 Q4)")

# Add legend with proper labels
legend("bottom",
       legend = c("Unemployment Rate", "Industrial Production", "Real GDP Per Capita", "Inflation (PCE)"),
       col = 1:6, lty = 1, lwd = 2, bty = "n")


#===================================
# Test for stationary over 4 periods
#===================================
# Define concise function to run Adf_diff tests per window
run_adf_block <- function(df) {
  print(adf.test(df$bs))
  print(adf.test(df$d_bs))
  print(adf.test(df$gov))
  print(adf.test(df$d_gov))
  print(adf.test(df$d2_gov))
  print(adf.test(df$unrate))
  print(adf.test(df$d_unrate))
  print(adf.test(df$d2_unrate))
  print(adf.test(df$indpro))
  print(adf.test(df$d_indpro))
  print(adf.test(df$rgdppc))
  print(adf.test(df$d_rgdppc))
  print(adf.test(df$pce))
  print(adf.test(df$d_pce))
  print(adf.test(df$d2_pce))
  print(adf.test(df$transfers))
  print(adf.test(df$d_transfers))
  print(adf.test(df$d2_transfers))
}

# Run for each recession window
run_adf_block(df_recession_1990) # Not used
run_adf_block(df_recession_2001) # Not used
run_adf_block(df_recession_2007)
run_adf_block(df_recession_2020)


#=============================================
# Summary Statistics in 2007 and 2020 Periods
#=============================================
summary(df_recession_2007)
summary(df_recession_2020)


# ==================================
# Select Optimal Lag for 9 Models
# ==================================
# Prepare data for d_bs during the 2007 recession M1
VAR_data_2007_bs <- na.omit(df_recession_2007[, c("d_bs", "d_indpro", "d_rgdppc", "d_pce")])
lag_selection_2007_bs <- VARselect(VAR_data_2007_bs, lag.max = 6, type = "const")
print(lag_selection_2007_bs$selection)

# Prepare data for d2_gov during the 2007 recession M2
VAR_data_2007_gov <- na.omit(df_recession_2007[, c("d2_gov", "d_indpro", "d_rgdppc", "d_pce")])
lag_selection_2007_gov <- VARselect(VAR_data_2007_gov, lag.max = 6)
print(lag_selection_2007_gov$selection)

# Prepare data for d2_transfers during the 2007 recession M3
VAR_data_2007_transfers <- na.omit(df_recession_2007[, c("d2_transfers", "d_indpro", "d_rgdppc", "d_pce")])
lag_selection_2007_transfers <- VARselect(VAR_data_2007_transfers, lag.max = 6)
print(lag_selection_2007_transfers$selection)

# Prepare data for d_bs during the 2020 recession M4
VAR_data_2020_bs <- na.omit(df_recession_2020[, c("d_bs", "d_indpro", "d_rgdppc", "d_pce")])
lag_selection_2020_bs <- VARselect(VAR_data_2020_bs, lag.max = 6)
print(lag_selection_2020_bs$selection)

# Prepare data for d2_gov during the 2020 recession M5
VAR_data_2020_gov <- na.omit(df_recession_2020[, c("d2_gov", "d_indpro", "d_rgdppc", "d_pce")])
lag_selection_2020_gov <- VARselect(VAR_data_2020_gov, lag.max = 6)
print(lag_selection_2020_gov$selection)

# Prepare data for d2_transfers during the 2020 recession M6
VAR_data_2020_transfers <- na.omit(df_recession_2020[, c("d2_transfers", "d_indpro", "d_rgdppc", "d_pce")])
lag_selection_2020_transfers <- VARselect(VAR_data_2020_transfers, lag.max = 6)
print(lag_selection_2020_transfers$selection)

# Prepare data for d2_gov during the 2020 recession with unrate M7
VAR_data_2020_gov_unrate <- na.omit(df_recession_2020[, c("d2_gov", "d_indpro", "d_rgdppc", "d_pce", "d2_unrate")])
lag_selection_2020_gov_unrate <- VARselect(VAR_data_2020_gov_unrate, lag.max = 6)
print(lag_selection_2020_gov_unrate$selection)

# Prepare data for d_bs during the 2020 recession with unrate M8
VAR_data_2020_bs_unrate <- na.omit(df_recession_2020[, c("d_bs", "d_indpro", "d_rgdppc", "d_pce", "d2_unrate")])
lag_selection_2020_bs_unrate <- VARselect(VAR_data_2020_bs_unrate, lag.max = 6)
print(lag_selection_2020_bs_unrate$selection)

# Prepare data for d2_transfers during the 2020 recession with unrate M9
VAR_data_2020_transfers_unrate <- na.omit(df_recession_2020[, c("d2_transfers", "d_indpro", "d_rgdppc", "d_pce", "d2_unrate")])
lag_selection_2020_transfers_unrate <- VARselect(VAR_data_2020_transfers_unrate, lag.max = 6)
print(lag_selection_2020_transfers_unrate$selection)


# ================================
# Estimate VAR Models with L4
# ================================
# 2007 models
VAR_2007_bs <- VAR(VAR_data_2007_bs, p = 4)
VAR_2007_gov <- VAR(VAR_data_2007_gov, p = 4)
VAR_2007_transfers <- VAR(VAR_data_2007_transfers, p = 4)

# 2020 models without unrate
VAR_2020_bs <- VAR(VAR_data_2020_bs, p = 4)
VAR_2020_gov <- VAR(VAR_data_2020_gov, p = 4)
VAR_2020_transfers <- VAR(VAR_data_2020_transfers, p = 4)

# 2020 models with unrate
VAR_2020_bs_unrate <- VAR(VAR_data_2020_bs_unrate, p = 4)
VAR_2020_gov_unrate <- VAR(VAR_data_2020_gov_unrate, p = 4)
VAR_2020_transfers_unrate <- VAR(VAR_data_2020_transfers_unrate, p = 4)

# View model summaries
summary(VAR_2007_bs) # Model 1
summary(VAR_2007_gov) # Model 2
summary(VAR_2007_transfers) # Model 3
summary(VAR_2020_bs) # Model 4
summary(VAR_2020_gov) # Model 5
summary(VAR_2020_transfers) # Model 6
summary(VAR_2020_bs_unrate) # Model 7
summary(VAR_2020_gov_unrate) # Model 8
summary(VAR_2020_transfers_unrate) # Model 9


# ================================
# Impulse Response Functions
# ================================
run_irf <- function(var_model, impulse_var, n_ahead = 12) {
  # Identify all variables in the model except the impulse
  response_vars <- setdiff(colnames(var_model$y), impulse_var)
  
  # Run IRF
  irf_result <- irf(var_model,
                    impulse = impulse_var,
                    response = response_vars,
                    n.ahead = n_ahead,
                    ortho = FALSE,
                    runs = 1000)
  
  return(irf_result)
}


# 2007 Recession Period IRFs
irf_2007_bs <- run_irf(VAR_2007_bs, impulse_var = "d_bs")
plot(irf_2007_bs)
irf_2007_bs # Model 1

irf_2007_gov <- run_irf(VAR_2007_gov, impulse_var = "d2_gov")
plot(irf_2007_gov)
irf_2007_gov # Model 2

irf_2007_transfers <- run_irf(VAR_2007_transfers, impulse_var = "d2_transfers")
plot(irf_2007_transfers)
irf_2007_transfers # Model 3

# 2020 Recession Period IRFs
irf_2020_bs <- run_irf(VAR_2020_bs, impulse_var = "d_bs")
plot(irf_2020_bs)
irf_2020_bs # Model 4

irf_2020_gov <- run_irf(VAR_2020_gov, impulse_var = "d2_gov")
plot(irf_2020_gov)
irf_2020_gov # Model 5

irf_2020_transfers <- run_irf(VAR_2020_transfers, impulse_var = "d2_transfers")
plot(irf_2020_transfers)
irf_2020_transfers # Model 6

# 2020 Recession Period with Unrate IRFs
irf_2020_bs_unrate <- run_irf(VAR_2020_bs_unrate, impulse_var = "d_bs")
plot(irf_2020_bs_unrate)
irf_2020_bs_unrate # Model 7

irf_2020_gov_unrate <- run_irf(VAR_2020_gov_unrate, impulse_var = "d2_gov")
plot(irf_2020_gov_unrate)
irf_2020_gov_unrate # Model 8

irf_2020_transfers_unrate <- run_irf(VAR_2020_transfers_unrate, impulse_var = "d2_transfers")
plot(irf_2020_transfers_unrate)
irf_2020_transfers_unrate # Model 9


# ================================
# Granger Causality Tests
# ================================
run_granger_tests <- function(var_model) {
  var_names <- colnames(var_model$y)
  granger_results <- data.frame(Cause = character(), P_Value = numeric(), stringsAsFactors = FALSE)
  
  for (cause_var in var_names) {
    test <- causality(var_model, cause = cause_var)$Granger
    p_val <- test$p.value
    granger_results <- rbind(granger_results, data.frame(Cause = cause_var, P_Value = p_val))
  }
  
  return(granger_results)
}

# Run Granger causality for each model
granger_2007_bs <- run_granger_tests(VAR_2007_bs)
granger_2007_bs # Model 1
granger_2007_gov <- run_granger_tests(VAR_2007_gov)
granger_2007_gov # Model 2
granger_2007_transfers <- run_granger_tests(VAR_2007_transfers)
granger_2007_transfers # Model 3

granger_2020_bs <- run_granger_tests(VAR_2020_bs)
granger_2020_bs # Model 4
granger_2020_gov <- run_granger_tests(VAR_2020_gov)
granger_2020_gov # Model 5
granger_2020_transfers <- run_granger_tests(VAR_2020_transfers)
granger_2020_transfers # Model 6

granger_2020_bs_unrate <- run_granger_tests(VAR_2020_bs_unrate)
granger_2020_bs_unrate # Model 7
granger_2020_gov_unrate <- run_granger_tests(VAR_2020_gov_unrate)
granger_2020_gov_unrate # Model 8
granger_2020_transfers_unrate <- run_granger_tests(VAR_2020_transfers_unrate)
granger_2020_transfers_unrate # Model 9

