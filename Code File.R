#Govt Bond Yield  
#Importing the dataset
df1<-read.csv(file.choose())
head(df1)

#Converting the Date columnn to Date format
df1$DATE <- as.Date(df1$DATE, format = "%d-%m-%Y")
str(df1)

#Plotting the RAW Data
plot(df1)

#Converting the dataset to Time-Series
gby_ts <- ts(df1$Price,
             start = c(2000,1),
             frequency = 12)

#Decomposing the dataset
decomposed_df1 <- decompose(gby_ts)
#Plotting the decomposed data
plot(decomposed_df1)

#ADF Test to check for Stationarity
library(tseries)
adf_gby <- adf.test(df1$Price)
print(adf_gby)
#The test shows that the data is not stationary as p-value is greater than 0.05

#Taking The first Difference of the data to make it Stationary
df1$gby_diff <- c(NA, diff(df1$Price, differences = 1))
df1 <- df1[-1, ]

#ADF Test to check for stationarity of First Difference Data
adf_gby2 <- adf.test(df1$gby_diff)
print(adf_gby2)
#The test shows that the Differenced data is stationary as p-value is less than 0.05

#NIFTY50 Prices
#Importing the datset
df2<-read.csv(file.choose())
head(df2)

#Converting the Date columnn to Date format
df2$DATE <- as.Date(df2$DATE, format = "%d-%m-%Y")
str(df2)

#Plotting the RAW Data
plot(df2)

#Converting the dataset to Time-Series
stock_ts <- ts(df2$Price, 
               start = c(2000,1), 
               frequency = 12)

#Decomposing the dataset
decomposed_df2 <- decompose(stock_ts)
#Plotting the decomposed data
plot(decomposed_df2)

#ADF Test to check for Stationarity
library(tseries)
adf_stock <- adf.test(df2$Price)
print(adf_stock)
#The test shows that the data is not stationary

#Taking The first Difference of the data to make it Stationary
df2$Price_diff <- c(NA, diff(df2$Price, differences = 1))
df2 <- df2[-1, ]

#ADF Test to check for stationarity of First Difference Data
adf_stock2 <- adf.test(df2$Price_diff)
print(adf_stock2)
#The test shows that the Differenced data is stationary 

#WPI
#Importing the datset
df3<-read.csv(file.choose())
head(df3)

#Converting the Date columnn to Date format
df3$DATE <- as.Date(df3$DATE, format = "%d-%m-%Y")
str(df3)

#Plotting the RAW Data
plot(df3)

#Converting the dataset to Time-Series
WPI_ts <- ts(df3$WPI,
             start = c(2000,1),
             frequency = 12)

#Decomposing the dataset
decomposed_df3 <- decompose(WPI_ts)
#Plotting the decomposed data
plot(decomposed_df3)

#ADF Test to check for Stationarity
library(tseries)
adf_wpi <- adf.test(df3$WPI)
print(adf_wpi)
#The test shows that the data is not stationary.

#Taking The first Difference of the data to make it Stationary
df3$wpi_diff <- c(NA, diff(df3$WPI, differences = 1))
df3 <- df3[-1, ]

#The test shows that the Differenced data is stationary
adf_wpi2 <- adf.test(df3$wpi_diff)
print(adf_wpi2)

#Exp-IMP Ratio
#Importing the dataset
df4<-read.csv(file.choose())
head(df4)
df4 <- df4[1:228, ]

#Converting the date column into Date format.
df4$DATE <- as.Date(df4$DATE, format = "%Y-%m-%d")
str(df4)

#Plotting the Raw Data
plot(df4)

#Converting the dataset to Time-Series
ex_imp_ts <- ts(df4$EX.IMP,
                start = c(2000,1),
                frequency = 12)

#Decomposing the dataset
decomposed_df4 <- decompose(ex_imp_ts)
#Plotting the decomposed data
plot(decomposed_df4)

#ADF Test to check for Stationarity
library(tseries)
adf_ex_imp <- adf.test(df4$EX.IMP)
print(adf_ex_imp)
#The test shows that the data is not stationary

#Taking The first Difference of the data to make it Stationary
df4$ex_imp_diff <- c(NA, diff(df4$EX.IMP, differences = 1))
df4 <- df4[-1, ]

#ADF Test foe stationarity for First Difference Data
adf_ex_imp2 <- adf.test(df4$ex_imp_diff)
print(adf_ex_imp2)
#The test shows that the differenced data is stationary

#All the variables are I(1)

#Merging the data
merged_df <- merge(df1, df2, by = "DATE")
merged_df <- merge(merged_df, df3, by = "DATE")
merged_df <- merge(merged_df, df4, by = "DATE")

#Working with the level data
final <- subset(merged_df, select = -c(Price_diff, gby_diff,wpi_diff, ex_imp_diff))
#Converting the merged data into Time series
final_ts <- ts(final[, -1], start = c(2000, 03), frequency = 12)

#Checking for multicollinearity
cor_matrix <- cor(final_ts)
print(cor_matrix)
#It can be seen that there is no significant multicollinearity 

#Lag selection
install.packages("vars")
library(vars)
lag_selection <- VARselect(final_ts, lag.max = 12, type = "trend")
print(lag_selection)
print(lag_selection$selection)
#The lag value comes to be 2 according to all the criterieas

#JJ test for cointegration
library(urca)
cointegration_test <- ca.jo(final_ts, type = "trace", ecdet = "trend", K = 2)
summary(cointegration_test)
#The test shows that there are no cointegrating relationships in the model 

#As there are no cointegrating relationships, we proceed with VAR Model

#Creating new dataset with differenced data
final_diff <- subset(merged_df, select = -c(Price.x, Price.y ,WPI, EX.IMP))
#Converting this new data into time series
final_diff_ts <- ts(final_diff[, -1], start = c(2000, 03), frequency = 12)

#Lag Selection for VAR model
lag_select <- VARselect(final_diff_ts, lag.max = 15, type = "trend")
print(lag_select)
print(lag_select$selection)
#The Lag value selected on the basis of HQ criteria is 1

#VAR Model
var_model <- VAR(final_diff_ts, p = 1, type = "trend")
summary(var_model)

#Granger Causality
# List of variables in the model
variables <- c("gby_diff", "Price_diff", "wpi_diff", "ex_imp_diff")

#Loop through each variable and testing Granger causality on others
for (var in variables) {
  cat("\nGranger Causality Test for:", var, "\n")
  print(causality(var_model, cause = var))
}
# Defining a data frame to store results
results <- data.frame(
  Cause = character(),
  Affected_Variable = character(),
  F_Statistic = numeric(),
  P_Value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

# Populating the data frame with test results
for (var in variables) {
  test <- causality(var_model, cause = var)
  for (affected in variables) {
    if (affected != var) {
      # Extracting F-statistic and p-value
      f_stat <- test$Granger$statistic
      p_val <- test$Granger$p.value
      sig <- ifelse(p_val < 0.05, "Yes", "No")
      
      # Appending results to data frame
      results <- rbind(results, data.frame(
        Cause = var,
        Affected_Variable = affected,
        F_Statistic = f_stat,
        P_Value = p_val,
        Significant = sig
      ))
    }
  }
}

# View summarized results
print(results)
# It can be seen that WPI and EXP-IMP ratio have significant impact on other variables but Government Bond Yield and NIFTY 50 prices do not have any significant effect.

#IRF
#WPI EFFECTS:
irf_results1 <- irf(var_model, impulse = "wpi_diff", response = c("gby_diff", "Price_diff", "ex_imp_diff"),
                    n.ahead = 10, boot = TRUE, ci = 0.95)
plot(irf_results1)
# WPI has a significant impact on NIFTY 50 prices and some impact on EXP-IMP Ratio

#GBY EFFECTS:
irf_results2 <- irf(var_model, impulse = "gby_diff", response = c("wpi_diff", "Price_diff", "ex_imp_diff"),
                    n.ahead = 10, boot = TRUE, ci = 0.95)
plot(irf_results2)
#GBY has significant impact on NIFTY 50 Prices and some effect on EXP-IMP ratio

#NIFTY 50
irf_results3 <- irf(var_model, impulse = "Price_diff", response = c("wpi_diff", "gby_diff", "ex_imp_diff"),
                    n.ahead = 10, boot = TRUE, ci = 0.95)
plot(irf_results3)
#NIFTY 50 prices have significant impact on EXP-IMP ratio and some effect on both GBY and WPI

#EX-IMP
irf_results4 <- irf(var_model, impulse = "ex_imp_diff", response = c("wpi_diff", "gby_diff", "Price_diff"),
                    n.ahead = 10, boot = TRUE, ci = 0.95)
plot(irf_results4)
#EXP-IMP ratio has a significant impact only on NIFTY 50 prices

#VD
# Calculate the variance decomposition
fevd_results <- fevd(var_model, n.ahead = 10)

# Print the variance decomposition results
print(fevd_results)
#Every variable is well explained by their own shocks.


#DIAGNOSTIC CHECKS
# Extracting residuals from the VAR model
residuals_var <- residuals(var_model)

# Ljung-Box test for autocorrelation
for (i in 1:ncol(residuals_var)) {
  print(paste("Ljung-Box test for equation", colnames(residuals_var)[i]))
  print(Box.test(residuals_var[, i], lag = 1, type = "Ljung-Box"))  # lag=10 for testing autocorrelation up to 10 lags
}
#The test shows that there is no significant auto correlation in the data

#ARCH TEST for Heteroscedasticity
install.packages("FinTS")
library(FinTS)
# Apply ARCH test
arch_test <- ArchTest(residuals_var)

# Print results
print(arch_test)
#The test reveals some heteroskedasticity (p < 0.05), this only suggests variability in volatility and does not compromise model stability

#Stability check
#CUSUM TEST
# Perform CUSUM test on the residuals
cusum_test <- efp(residuals_var[, 1] ~ 1, type = "Rec-CUSUM")

# Plot the CUSUM test results
plot(cusum_test, main = "CUSUM Test for Stability")
#The test reveals that the Model is stable

#Normality
#Shapiro-Wilk test for normality on the residuals of the VAR model
shapiro_test <- apply(residuals(var_model), 2, shapiro.test)

# Print results
print(shapiro_test)
#The test reveals some non-normality in the residuals but does not affect the stability of the model



