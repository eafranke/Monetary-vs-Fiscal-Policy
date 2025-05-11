# Which Type of Government Stimulus is Better for Recessions? Measuring the Effects of Monetary and Fiscal Policy Shocks on Economic Conditions

## About

This project evaluates the effectiveness of different macroeconomic stimulus tools—specifically balance sheet expansions (monetary policy), government transfers, and government spending (fiscal policy)—during the 2007–2009 Great Recession and the 2020 COVID-19 recession. Using quarterly macroeconomic data from 2002 Q4 to 2024 Q4, the analysis applies vector autoregression (VAR), impulse response functions (IRFs), and Granger causality testing to assess how these policies impacted output, employment, and inflation. All analysis is conducted in **R**.

## Repository Contents

### 1. `monetary_fiscal.xlsx`
Primary data file containing quarterly U.S. macroeconomic indicators from the Federal Reserve Economic Database (FRED). All variables are expressed as **year-over-year percent changes**, **except for the unemployment rate**, which is reported in raw percentage terms. Key variables include:

- `bs` – Federal Reserve Balance Sheet (total assets)
- `transfers` – Government transfer payments to persons
- `gov` – Real government spending and investment
- `indpro` – Industrial production
- `rgdppc` – Real GDP per capita
- `pce` – Personal Consumption Expenditures (inflation)
- `unrate` – Unemployment rate (raw percent)
- `date` – Time variable (in quarterly format)

### 2. `Project Models.R`
Comprehensive R script containing the full analysis workflow:

- **Data loading and preparation** – Cleaning, differencing, and filtering variables
- **Exploratory plots** – Visual comparisons of policy and indicator variables across time  
- **ADF stationarity testing** – Validates differencing requirements for VAR  
- **VAR modeling** – Estimated across two recession periods using 9 model specifications  
- **Impulse Response Functions (IRFs)** – Plots dynamic effects of policy shocks on macro outcomes  
- **Granger causality tests** – Assesses predictive power of policy variables on economic indicators  

### 3. Project Paper  
**`Which Type of Government Stimulus is Better for Recessions.pdf`**  
Full research write-up by Erik Franke, including:

- Introduction & Literature Review  
- Data and Methodology
- Granger Causality Insights
- Empirical Results and IRF Interpretation    
- Policy Implications and Conclusion

## Usage Instructions

1. Open the `Project Models.R` file in R or RStudio.
2. Adjust file paths as needed for your machine.
3. Run the entire script to reproduce data preparation, modeling, IRF plotting, and Granger testing.
4. Refer to the PDF for theoretical background and interpretation of results.
