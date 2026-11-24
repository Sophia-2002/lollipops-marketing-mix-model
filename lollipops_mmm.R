# COURSEWORK: LOLLIPOPS MARKETING MIX MODEL

# Clean workspace
rm(list = ls())

# Load  packages 
if(!require(car)) install.packages("car")
if(!require(lmtest)) install.packages("lmtest")
if(!require(sandwich)) install.packages("sandwich")
if(!require(MASS)) install.packages("MASS")
if(!require(olsrr)) install.packages("olsrr")
if(!require(moments)) install.packages("moments")


library(car)    
library(MASS)   
library(lmtest) 
library(moments)
library(sandwich)


# Loading data and save to R Data File as required
original_data <- read.csv("data/lollipops_1.csv")   # coursework data, not included in this repo
save(original_data, file = "CourseWorkData.Rdata")
cat("Data saved as CourseWorkData.Rdata\n")



# INITIAL DATA CHECKS

cat("Loading and exploring the dataset\n")

# Load the dataset
load("CourseWorkData.Rdata")
data <- original_data
# Basic dataset information
cat("Dataset dimensions:", dim(data), "\n")
cat("Column names:\n")
print(names(data))
cat("\nFirst few rows:\n")
print(head(data))
cat("\nData structure:\n")
str(data)

# Check for missing values
cat("\nMissing values summary:\n")
print(colSums(is.na(data)))


# DATA PREPROCESSING

cat("\n Data preprocessing\n")

# Factorize categorical variables 
clean_names <- c("Sales", "Price", "Ad1_GRP", "Ad2_Banners", "Prom_Stores", 
                 "Wage_USD", "Unit_Cost", "Time", "Region", "Month", "Year")
names(data) <- clean_names
data$Region <- as.factor(data$Region)
data$Month <- as.factor(data$Month)

# Test profitability
data$profit_margin <- data$Price - data$Unit_Cost
data <- data[, !is.na(names(data))]
mean_profitability <- mean(data$profit_margin/data$Price)
print(mean_profitability)

# Create log_Sales 
if (any(data$Sales <= 0)) {
  min_positive <- min(data$Sales[data$Sales > 0])
  data$log_Sales <- log(data$Sales + 0.1 * min_positive)
  cat("Small constants are added to handle zero sales before log transform\n")
} else {
  data$log_Sales <- log(data$Sales)
}

# Display summary statistics 
cat("\nSummary statistics:\n")
print(summary(data))

# Compare distributions
{
  par(mfrow = c(1, 2))
  hist(data$Sales, main = "Original Sales", xlab = "Sales", col = "skyblue")
  hist(data$log_Sales, main = "Log(Sales)", xlab = "log(Sales)", col = "gold")
  par(mfrow = c(1, 1))
}



cat("\n Exploratory Data Analysis\n")

# Sales distribution by region 
cat("log_Sales summary by region:\n")
log_sales_by_region <- aggregate(log_Sales ~ Region, data = data, 
                                 FUN = function(x) c(mean = mean(x), median = median(x), sd = sd(x)))
print(log_sales_by_region)

# Correlation analysis 
numeric_vars <- data[, c("log_Sales", "Price", "Ad1_GRP", "Ad2_Banners", 
                         "Prom_Stores", "Wage_USD", "Unit_Cost","Time")]
cat("\nCorrelation matrix:\n")
cor_matrix <- cor(numeric_vars)
print(round(cor_matrix, 3))



# DATA SPLITTING 

cat("\n Splitting data into training and test sets\n")

# Set seed for reproducibility 
set.seed(123456)

# Calculate training set size 
n <- nrow(data)
trainingsetsize <- round(n * 0.8)

# Random selection 
random_selection <- sample(x = 1:n, size = trainingsetsize)
trainingset <- data[random_selection, ]
testset <- data[-random_selection, ]

cat("Training set size:", nrow(trainingset), "\n")
cat("Test set size:", nrow(testset), "\n")




# INITIAL MULTIPLE LINEAR REGRESSION MODEL

cat("\n Initial multiple linear regression model\n")

# Initial model with all relevant predictors 
initial_model <- lm(log_Sales ~ Price + Ad1_GRP + Ad2_Banners + Prom_Stores +
                      Time + Region,data = trainingset)

cat("Initial model summary:\n")
print(summary(initial_model))



# VARIABLE SELECTION USING STEPWISE REGRESSION 

cat("\n Performing variable selection using stepwise regression...\n")

# Forward selection 
forward_model <- stepAIC(initial_model, direction = "forward", trace = FALSE)

# Backward elimination 
backward_model <- stepAIC(initial_model, direction = "backward", trace = FALSE)

# Compare models
cat("Forward selection AIC:", AIC(forward_model), "\n")
cat("Backward elimination AIC:", AIC(backward_model), "\n")

# Choose the best model based on AIC
if (AIC(forward_model) <= AIC(backward_model)) {
  final_model <- forward_model
  cat("Selected: Forward selection model\n")
} else {
  final_model <- backward_model
  cat("Selected: Backward elimination model\n")
}

cat("\nFinal model after stepwise selection:\n")
print(summary(final_model))

# Using the method in class we also ommit the "Price" variable 
require(olsrr)
stepforward_selection <- ols_step_forward_p(initial_model)
stepforward_selection
#The plot is put in the appendix 



# Manual Refinement 

# Check for non-significant variables and refine manually
final_summary <- summary(final_model)
cat("P-values of final model coefficients:\n")
print(final_summary$coefficients[,4])


#  Validation & MODEL DIAGNOSTICS 

cat("\n Checking model assumptions...\n")

# Multicollinearity check using VIF 
cat("Variance Inflation Factors (VIF):\n")
vif_values <- vif(final_model)
print(vif_values)

# Normality of residuals (Shapiro-Wilk test)
shapiro_test <- shapiro.test(residuals(final_model))
cat("\nShapiro-Wilk normality test p-value:", shapiro_test$p.value, "\n")

# Homoscedasticity (Breusch-Pagan test)
bp_test <- bptest(final_model)
cat("Breusch-Pagan test p-value:", bp_test$p.value, "\n")

# Residual plots 
cat("\nGenerating diagnostic plots\n")

par(mfrow = c(2,2))
# Index vs residuals plot
plot(1:length(residuals(final_model)), residuals(final_model), 
     bty = "n", main = "Residuals vs Index", pch = 18, 
     col = rgb(0.6, 0.7, 0.2, 0.9), xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

# Residuals vs Fitted
plot(fitted(final_model), residuals(final_model),
     main = "Residuals vs Fitted (Log Model)",
     xlab = "Fitted log(Sales)", ylab = "Residuals",
     col = rgb(0.2, 0.4, 0.8, 0.6), pch = 16)
abline(h = 0, col = "red", lwd = 2)
lines(lowess(fitted(final_model), residuals(final_model)), col = "red", lwd = 2)


# Scale-Location Plot (shows heteroscedasticity)
plot(fitted(final_model), sqrt(abs(residuals(final_model))),
     main = "Scale-Location Plot",
     xlab = "Fitted Values", ylab = "√|Residuals|")
lines(lowess(fitted(final_model), sqrt(abs(residuals(final_model)))), col = "red", lwd = 2)

# Residuals vs Time (check for time-based patterns)
plot(trainingset$Time, residuals(final_model),
     main = "Residuals vs Time",
     xlab = "Time", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Residual plots with marketing activities 
cat("\nGenerating residual plots with other covariates\n")

# Ad1_GRP vs. residuals 
plot(trainingset$Ad1_GRP, residuals(final_model),
     bty = "n", main = "Residuals vs TV Ads (Ad1_GRP)",
     pch = 18, col = rgb(0.3, 0.7, 0.2, 0.9),
     xlab = "TV Ads (GRP)", ylab = "Residuals")
abline(h = 0, col = "red")

# Ad2_Banners vs. residuals 
plot(trainingset$Ad2_Banners, residuals(final_model),
     bty = "n", main = "Residuals vs Banner Ads",
     pch = 18, col = rgb(0.3, 0.7, 0.2, 0.9),
     xlab = "Banner Ads (Count)", ylab = "Residuals")
abline(h = 0, col = "red")

# Prom_Stores vs. residuals 
plot(trainingset$Prom_Stores, residuals(final_model),
     bty = "n", main = "Residuals vs Promotion Stores",
     pch = 18, col = rgb(0.3, 0.7, 0.2, 0.9),
     xlab = "Promotion Stores", ylab = "Residuals")
abline(h = 0, col = "red")


# Heteroscedasticity plots (some plotted previously but included here for clarity in report)
cat("\n plot for heteroscedasticity")
par(mfrow = c(2,2))
# Box plots of points residual distribution by region
boxplot(residuals(final_model) ~ trainingset$Region,
        bty = "n",
        main = "Residual Distribution by Region",
        xlab = "Region",
        ylab = "Residuals",
        col = rgb(0.2, 0.7, 0.7, 0.7),  # Similar blue-green as Lecture 6
        border = "darkblue",
        outpch = 18,   
        outcol = rgb(0.6, 0.7, 0.2, 0.9))  
# Add horizontal line at y=0 
abline(h = 0, col = "red", lwd = 2)

region_means <- tapply(residuals(final_model), trainingset$Region, mean)
points(1:length(region_means), region_means, 
       pch = 23, col = "red", bg = "yellow", cex = 1.5)


# Prom_Stores vs. residuals 
plot(trainingset$Prom_Stores, residuals(final_model),
     bty = "n", main = "Residuals vs Promotion Stores",
     pch = 18, col = rgb(0.3, 0.7, 0.2, 0.9),
     xlab = "Promotion Stores", ylab = "Residuals")
abline(h = 0, col = "red")

# Index vs residuals plot
plot(1:length(residuals(final_model)), residuals(final_model), 
     bty = "n", main = "Residuals vs Index", pch = 18, 
     col = rgb(0.6, 0.7, 0.2, 0.9), xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

# Scale-Location Plot (shows heteroscedasticity)
plot(fitted(final_model), sqrt(abs(residuals(final_model))),
     main = "Scale-Location Plot",
     xlab = "Fitted Values", ylab = "sqrt|Residuals|")
lines(lowess(fitted(final_model), sqrt(abs(residuals(final_model)))), col = "red", lwd = 2)


#Normality check
cat("\n plot for normality")
par(mfrow = c(1, 2))
# residual histogram for normality check
hist(residuals(final_model), bty = "n", main = "Histogram of Residuals",
     col = rgb(0.2, 0.7, 0.7, 0.9), freq = FALSE,
     xlab = "Residuals", ylab = "Density")
# Add normal curve 
x_seq <- seq(min(residuals(final_model)), max(residuals(final_model)), length.out = 100)
lines(x_seq, dnorm(x_seq, mean = 0, sd = sd(residuals(final_model))), 
      col = "red", lwd = 2)

# Q-Q Plot
qqnorm(residuals(final_model), main = "Q-Q Plot of Residuals")
qqline(residuals(final_model), col = "red", lwd = 2)




# ANSWERING BUSINESS QUESTIONS

cat("\n Answering business questions\n")

# Q1: How many units sold are associated with each marketing activity?
cat("Q1: Marketing activity association\n")

# Extract from final model
coef_summary <- summary(final_model)$coefficients
marketing_coefs <- coef_summary[grepl("Ad1_GRP|Ad2_Banners|Prom_Stores", rownames(coef_summary)), ]
print(marketing_coefs)

# Calculate expected percentage effects
cat("\nExpected percentage sales increase per marketing activity:\n")
coefficients <- coef(final_model)

for (coef_name in c("Ad1_GRP", "Ad2_Banners", "Prom_Stores")) {
  if (coef_name %in% names(coefficients)) {
    pct_effect <- (exp(coefficients[coef_name]) - 1) * 100
    # Get p-value from final model
    pvalue <- coef_summary[coef_name, 4]
    significance <- ifelse(pvalue < 0.001, "***", 
                           ifelse(pvalue < 0.01, "**",
                                  ifelse(pvalue < 0.05, "*", "ns")))
    
    cat(coef_name, ":", round(pct_effect, 2), "%", significance, "\n")
  }
}

# Unit of sales associated with each marketing activities
cat("\n MARKETING CONTRIBUTION ANALYSIS \n")


fitted_sales <- exp(fitted(final_model))

marketing_contributions <- data.frame(
  Marketing_Activity = character(),
  Monthly_Contribution = numeric(),
  Annual_Contribution = numeric(),
  Percentage_of_Total = numeric(),
  stringsAsFactors = FALSE
)


marketing_vars <- c("Ad1_GRP", "Ad2_Banners", "Prom_Stores")

for(marketing_var in marketing_vars) {
  
  cat("\n Analyzing:", marketing_var, "\n")
  base_marketing <- trainingset  
  base_marketing[[marketing_var]] <- 0
  
  fitted_without_marketing <- exp(predict(final_model, newdata = base_marketing))
  contribution_per_act <- fitted_sales - fitted_without_marketing
  
  monthly_contribution <- mean(contribution_per_act)
  total_contribution <- sum(contribution_per_act)
  annual_contribution <- monthly_contribution * 12
  
  # Calculate percentage of average sales
  avg_sales <- mean(fitted_sales)
  pct_avg_sales <- (monthly_contribution / avg_sales) * 100
  
  # Store results
  marketing_contributions <- rbind(marketing_contributions, data.frame(
    Marketing_Activity = marketing_var,
    Monthly_Contribution = monthly_contribution,
    Annual_Contribution = annual_contribution,
    Percentage_of_Total = pct_avg_sales
  ))
  
  # Print detailed results
  cat("Average monthly contribution:", round(monthly_contribution, 1), "units of sales\n")
  cat("Annual contribution:", round(annual_contribution, 0), "units of sales\n")
  cat("Contribution percentage of average sales:", round(pct_avg_sales, 2), "%\n")
}


# Average method
avg_tv <- mean(data$Ad1_GRP)
avg_banners <- mean(data$Ad2_Banners)
avg_prom <- mean(data$Prom_Stores)

# Calculate unit contributions from coefficients
# Applying Taylor approximation: Sales Association ≈ β × Avg(Sales) × Avg(Marketing Level)
# Calculate average sales
avg_sales <- mean(data$Sales)
cat("Average Monthly Sales:", format(round(avg_sales, 0), big.mark = ","), "units\n\n")

# Marketing activity averages
cat("Average Marketing Levels:\n")
cat("  TV Advertising (GRP):", round(avg_tv, 2), "GRP\n")
cat("  Banner Advertising:", round(avg_banners, 2), "banners\n")
cat("  In-Store Promotions:", round(avg_prom, 2), "stores\n\n")

# Calculate unit contributions
tv_units <- coef(final_model)["Ad1_GRP"] * avg_sales * avg_tv
tv_units_pct <- tv_units/avg_sales
banner_units <- coef(final_model)["Ad2_Banners"] * avg_sales * avg_banners
banner_units_pct <- banner_units/avg_sales
prom_units <- coef(final_model)["Prom_Stores"] * avg_sales * avg_prom
prom_units_pct <- prom_units/avg_sales

cat("TV Advertising Contribution: ", round(tv_units,2), "sales unit,", round(tv_units_pct*100,2), 
    "% of monthly average sales\n" )
cat("Banner Advertising Contribution: ", round(banner_units, 2), "sales unit,", round(banner_units_pct*100,2), 
    "% of monthly average sales\n" )
cat("Promotion in stores Contribution: ", round(prom_units, 2), "sales unit,", round(prom_units_pct*100,2), 
    "% of monthly average sales\n" )
            
            

# Q2: Cost-effectiveness analysis 
cat("\nQ2: Cost-effectiveness analysis:\n")

# Extract coefficients 
tv_coef <- coef(final_model)["Ad1_GRP"]
banner_coef <- coef(final_model)["Ad2_Banners"]

avg_TV <- mean(data$Ad1_GRP)
avg_Banner <- mean(data$Ad2_Banners)

# Annual costs 
tv_annual_cost <- 2000000
banner_annual_cost <- 500000

# Calculate PERCENTAGE effectiveness per £ spent 
tv_effectiveness_pct <-  (exp(tv_coef) - 1) * 100
banner_effectiveness_pct <- (exp(banner_coef) - 1) * 100

# Calculate cost per 1% sales increase for average number of ADs
tv_cost_per_pct <- (tv_annual_cost/12) / (tv_effectiveness_pct*avg_TV)
banner_cost_per_pct <- (banner_annual_cost/12) / (banner_effectiveness_pct*avg_Banner)

cat("TV ads effectiveness:", round(tv_effectiveness_pct, 2), "% sales increase per GRP", "\n")
cat("Banner ads effectiveness:", round(banner_effectiveness_pct, 2), "% sales increase per banner", "\n")

cat("\nCost to achieve 1% sales increase:\n")
cat("TV ads: £", round(tv_cost_per_pct, 2), "per 1% increase\n")
cat("Banner ads: £", round(banner_cost_per_pct, 2), "per 1% increase\n")

# Comparison for cost-effectiveness
if (tv_cost_per_pct < banner_cost_per_pct) {
  cat("\nTV ads are more cost-effective (lower cost per 1% sales increase)\n")
} else if (banner_cost_per_pct < tv_cost_per_pct) {
  cat("\nBanner ads are more cost-effective (lower cost per 1% sales increase)\n")
} else {
  cat("\nBoth have similar cost-effectiveness\n")
}


# Additional insight: Compare total potential impact
cat("\nPotential annual impact (if maximized):\n")
# Assuming maximum GRP levels 
max_grp <- max(data$Ad1_GRP)
annual_tv_impact <- tv_effectiveness_pct * max_grp
cat("TV ads: Up to", round(annual_tv_impact, 1), "% annual sales increase potential\n")


max_banners <- max(data$Ad2_Banners)
annual_banner_impact <- banner_effectiveness_pct * max_banners
cat("Banner ads: Up to", round(annual_banner_impact, 1), "% annual sales increase potential\n")



# Q3: Other sources of variation 
cat("\nQ3: Other significant variables explaining sales variation:\n")
significant_vars <- coef_summary[coef_summary[,4] < 0.05 & 
                                   !grepl("Ad1_GRP|Ad2_Banners|Prom_Stores", rownames(coef_summary)), ]

# Add interpretation for significant variables
if (nrow(significant_vars) > 0) {
  for (i in 1:nrow(significant_vars)) {
    var_name <- rownames(significant_vars)[i]
    coef_value <- coefficients[var_name]
    p_value <- significant_vars[i, 4]
    
    if (grepl("Region", var_name)) {
      pct_effect <- (exp(coef_value) - 1) * 100
      cat(var_name, ":", round(pct_effect, 1), "% difference from reference (p =", round(p_value, 4), ")\n")
    } else if (grepl("Time", var_name)) {
      pct_effect <- (exp(coef_value) - 1) * 100
      cat(var_name, ":", round(pct_effect, 2), "% monthly growth (p =", round(p_value, 4), ")\n")
    } else {
      cat(var_name, ":", round(coef_value, 4), "(p =", round(p_value, 4), ")\n")
    }
  }
} else {
  cat("No other significant variables found\n")
}

# Regional performance insights 
cat("\nRegional performance insights:\n")
region_vars <- c("RegionMorrocco", "RegionSingapore", "RegionSpain", "RegionUK")
for(region in region_vars) {
  if (region %in% rownames(coef_summary)) {
    p_value <- coef_summary[region, 4]
    pct_diff <- (exp(coefficients[region]) - 1) * 100
    significance <- ifelse(p_value < 0.001, "***", 
                           ifelse(p_value < 0.01, "**", 
                                  ifelse(p_value < 0.05, "*", "ns")))
    cat(region, ":", round(pct_diff, 1), "%", significance, "\n")
  }
}


#Plot the regional sales difference

region_coefs <- coef(final_model)[grepl("Region", names(coef(final_model)))]

# Percentage transformation
region_pct <- (exp(region_coefs) - 1) * 100

# create a new dataframe
region_df <- data.frame(
  Region = names(region_pct),
  Percent_Difference = region_pct
)

library("ggplot2")
#
ggplot(region_df, aes(x = Region, y = Percent_Difference, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percent_Difference)),
            vjust = -0.5) +                
  labs(
    title = "Regional Sales Differences Relative to Chile",
    x = "Region",
    y = "Percentage Difference (%)"
  )


# Variation caused by Time 
cat("\n Business Growth Trend:\n")
if ("Time" %in% names(coefficients)) {
  coef_value <- coefficients["Time"]
  pvalue <- coef_summary["Time", 4]
  monthly_growth <- (exp(coef_value) - 1) * 100
  
  cat("   Monthly growth rate:", round(monthly_growth, 2), "%")
  cat(ifelse(pvalue < 0.001, " (***)", 
             ifelse(pvalue < 0.01, " (**)",
                    ifelse(pvalue < 0.05, " (*)", " (ns)"))), "\n")
}



# Model Evaluation on test set 

cat("\n Evaluating model on test set\n")

# Make predictions on test set
test_predictions1 <- predict(final_model, newdata = testset)
test_predictions <- exp(test_predictions1)
# Calculate MAPE 
MAPE_test <- mean(abs(testset$Sales - test_predictions) / testset$Sales)
MAPE_training <- mean(abs(trainingset$Sales - exp(fitted(final_model))) / trainingset$Sales)

cat("MAPE on training set:", round(MAPE_training * 100, 2), "%\n")
cat("MAPE on test set:", round(MAPE_test * 100, 2), "%\n")
cat("Difference:", round((MAPE_test - MAPE_training) * 100, 2), "%\n")

# Check for overfitting 
if ((MAPE_test - MAPE_training) > 0.05) {
  cat("Warning: Potential overfitting detected (large difference between training and test MAPE)\n")
} else {
  cat("No significant overfitting detected\n")
}



# Prediction for next month

cat("\n Predicting for next month\n")

# Create data frane for next month prediction using average values
regions <- unique(data$Region)
next_month_predictions <- data.frame()

for(region in regions) {
  # Create prediction data frame for this region
  pred_data <- data.frame(
    Ad1_GRP = mean(data$Ad1_GRP),      
    Ad2_Banners = mean(data$Ad2_Banners),
    Prom_Stores = mean(data$Prom_Stores),
    Time = max(data$Time) + 1,         #For next time period
    Region = region
  )
  
  # Ensuring Region is factor with correct levels
  pred_data$Region <- factor(pred_data$Region, levels = levels(data$Region))
  
  # Generate prediction using final_model 
  pred_log <- predict(final_model, newdata = pred_data)
  
  # Convert back from log scale to original units
  pred_original <- exp(pred_log)
  
  region_pred <- data.frame(
    Region = region,
    Predicted_log_Sales = round(pred_log, 4),
    Predicted_Sales_Units = round(pred_original, 2),
    Time_Period = max(data$Time) + 1
  )
  next_month_predictions <- rbind(next_month_predictions, region_pred)
}

cat("Next month sales predictions by region:\n")
print(next_month_predictions)

# Add prediction intervals for uncertainty
cat("\nNext month predictions with 95% prediction intervals:\n")
for(region in regions) {
  pred_data <- data.frame(
    Ad1_GRP = mean(data$Ad1_GRP),
    Ad2_Banners = mean(data$Ad2_Banners), 
    Prom_Stores = mean(data$Prom_Stores),
    Time = max(data$Time) + 1,
    Region = region
  )
  pred_data$Region <- factor(pred_data$Region, levels = levels(data$Region))
  
  # Get prediction with interval
  pred_interval <- predict(final_model, newdata = pred_data, 
                           interval = "prediction", level = 0.95)
  
  # Convert from log scale
  pred_interval_original <- exp(pred_interval)
  
  cat("\nRegion:", region, "\n")
  cat("  Predicted:", round(pred_interval_original[1], 0), "units\n")
  cat("  95% Prediction Interval: [", 
      round(pred_interval_original[2], 0), ", ",
      round(pred_interval_original[3], 0), "] units\n")
}




# Region-specific model for prediction

cat("\n Building region-specific models for predictions\n")

# Initialize storage for region models and predictions
region_models <- list()
region_predictions <- list()

# Split data by region
region_data_list <- split(data, data$Region)

# Build separate model for each region
for(region in regions) {
  cat("\n--- Building model for", region, "---\n")
  
  region_data <- region_data_list[[region]]
  
  # Split into training and test for this region
  set.seed(123)
  n_region <- nrow(region_data)
  train_size <- round(n_region * 0.8)
  train_idx <- sample(1:n_region, train_size)
  
  region_train <- region_data[train_idx, ]
  region_test <- region_data[-train_idx, ]
  
  # Build model for this region 
  # Include Time, Price, marketing variables and apply stepwise to select
  region_full_model <- lm(log_Sales ~ Price + Ad1_GRP + Ad2_Banners + 
                            Prom_Stores + Time,
                          data = region_train)
  
  # Stepwise selection for this region
  region_step_model <- stepAIC(region_full_model, direction = "both", trace = FALSE)
  
  # Store the model
  region_models[[region]] <- region_step_model
  cat("Selected variables:", names(coef(region_step_model)), "\n")
  cat("R-squared:", round(summary(region_step_model)$r.squared, 3), "\n")
  cat("Adj R-squared:", round(summary(region_step_model)$adj.r.squared, 3), "\n")
  
  # Predict on test set for validation
  test_pred_log <- predict(region_step_model, newdata = region_test)
  test_pred_original <- exp(test_pred_log)
  
  # Calculate MAPE for this region's model
  region_mape <- mean(abs(region_test$Sales - test_pred_original) / 
                        region_test$Sales) * 100
  
  cat("Region-specific MAPE:", round(region_mape, 2), "%\n")
  
  # Store predictions
  region_predictions[[region]] <- list(
    train = region_train,
    test = region_test,
    predictions = test_pred_original,
    mape = region_mape
  )
}



# Compare region-specific vs global model performance

# This method was ultimately not applied,
# due to excessively high Region-specific MAPE for Morocco: 46.26 %
  

cat("\n--- PERFORMANCE COMPARISON ---\n")
global_predictions <- exp(predict(final_model, newdata = data))
global_mape <- mean(abs(data$Sales - global_predictions) / data$Sales) * 100

cat("Global model MAPE:", round(global_mape, 2), "%\n")
cat("Region-specific models average MAPE:", 
    round(mean(sapply(region_predictions, function(x) x$mape)), 2), "%\n")

# PREDICTION FOR NEXT MONTH 
cat("\n Next month predictions using region-specific models \n")

next_month_predictions_region <- data.frame()
overall_prediction <- 0

for(region in regions) {
  # Create prediction data for next month for this region
  region_avg <- region_data_list[[region]]
  
  # Initialize pred_data with error handling
  pred_data <- list()
  
  # Safely add variables if they exist in the region data
  add_if_exists <- function(var_name, region_data) {
    if (var_name %in% names(region_data)) {
      return(mean(region_data[[var_name]], na.rm = TRUE))
    } else {
      return(NA)
    }
  }
  
  # Add core variables
  pred_data$Price <- add_if_exists("Price", region_avg)
  pred_data$Ad1_GRP <- add_if_exists("Ad1_GRP", region_avg)
  pred_data$Ad2_Banners <- add_if_exists("Ad2_Banners", region_avg)
  pred_data$Prom_Stores <- add_if_exists("Prom_Stores", region_avg)
  pred_data$Time <- max(region_avg$Time, na.rm = TRUE) + 1
  
  # Convert to data frame and remove NA columns
  pred_data <- as.data.frame(pred_data)
  pred_data <- pred_data[, colSums(is.na(pred_data)) == 0]
  
  # Add any other variables selected in the region model
  model_vars <- names(coef(region_models[[region]]))
  for(var in model_vars) {
    if(!var %in% names(pred_data) && var != "(Intercept)" && var %in% names(region_avg)) {
      pred_data[[var]] <- mean(region_avg[[var]], na.rm = TRUE)
    }
  }
  pred_log <- predict(region_models[[region]], newdata = pred_data)
  pred_original <- exp(pred_log)
  
  # Add to overall prediction
  overall_prediction <- overall_prediction + pred_original
  
  # Store region prediction
  region_pred <- data.frame(
    Region = region,
    Predicted_Sales = round(pred_original, 2),
    Time_Period = max(region_avg$Time) + 1,
    Model_Variables = paste(names(coef(region_models[[region]]))[-1], collapse = ", ")
  )
  
  next_month_predictions_region <- rbind(next_month_predictions_region, region_pred)
  
  cat(region, ": ", round(pred_original, 0), " units\n", sep = "")
}

cat("\n Total Company Prediction \n")
cat("Next month total sales prediction: ", round(overall_prediction, 0), " units\n", sep = "")


# Compare region model prediction with global model prediction
cat("\n Comparing with global model \n")

comparison_table <- data.frame(
  Region = regions,
  # Region-Specific Models Prediction 
  Region_Specific_Models = round(next_month_predictions_region$Predicted_Sales, 0),
  # Global Model Prediction by Region 
  Global_Model_by_Region = round(next_month_predictions$Predicted_Sales_Units, 0)
)

# Calculate absolute and percentage differences
comparison_table$Absolute_Difference <- 
  comparison_table$Region_Specific_Models - comparison_table$Global_Model_by_Region

comparison_table$Percentage_Difference <- 
  (comparison_table$Absolute_Difference / comparison_table$Global_Model_by_Region) * 100

# Format for better display
percentage_display <- comparison_table$Percentage_Difference
percentage_display[is.na(percentage_display) | is.infinite(percentage_display)] <- NA

# Create the formatted table
comparison_table_formatted <- data.frame(
  Region = comparison_table$Region,
  "Region-Specific Models" = comparison_table$Region_Specific_Models,
  "Global Model (by Region)" = comparison_table$Global_Model_by_Region,
  "Difference (Units)" = comparison_table$Absolute_Difference,
  "Difference (%)" = ifelse(is.na(percentage_display), 
                            "N/A", 
                            paste0(round(percentage_display, 1), "%")),
  stringsAsFactors = FALSE,
  check.names = TRUE
)

cat("\n--- Regional Model Prediction ---\n")
cat("(Next Month Sales Forecast)\n\n")
print(comparison_table_formatted, row.names = FALSE)




# Appendix One 


cat("\n Generating output for coursework appendix\n")

# Create time-ordered table with actual vs predicted values for each region
appendix_tables <- list()

for(region in regions) {
  region_data <- data[data$Region == region, ]
  
  # Generate predictions using the LOG model
  region_pred_log <- predict(final_model, newdata = region_data)
  
  # Convert predictions back to original scale
  region_pred_original <- exp(region_pred_log)
  
  region_table <- data.frame(
    Time_Index = region_data$Time,
    Actual_Sales = round(region_data$Sales, 2),
    Predicted_Sales_Log = round(region_pred_log, 4),
    Predicted_Sales_Original = round(region_pred_original, 2)
  )
  
  # Calculate MAPE for this region (in original units)
  mape_region <- mean(abs((region_table$Actual_Sales - region_table$Predicted_Sales_Original) / 
                            region_table$Actual_Sales)) * 100
  
  appendix_tables[[as.character(region)]] <- list(
    table = region_table,
    mape = mape_region
  )
  
  cat("Region:", region, "- MAPE:", round(mape_region, 2), "%\n")
}

# Calculate overall MAPE (in original units)
all_predictions_log <- predict(final_model, newdata = data)
all_predictions_original <- exp(all_predictions_log)
overall_mape <- mean(abs((data$Sales - all_predictions_original) / 
                           data$Sales)) * 100

cat("\nOverall MAPE:", round(overall_mape, 2), "%\n")

# Create appendix table using appendix_tables
chileapp <- appendix_tables$Chile$table[order(appendix_tables$Chile$table$Time_Index), ]
singaporeapp <- appendix_tables$Singapore$table[order(appendix_tables$Singapore$table$Time_Index), ]
spainapp <- appendix_tables$Spain$table[order(appendix_tables$Spain$table$Time_Index), ]
ukapp <- appendix_tables$UK$table[order(appendix_tables$UK$table$Time_Index), ]
morroccoapp <- appendix_tables$Morrocco$table[order(appendix_tables$Morrocco$table$Time_Index), ]

# Write csv files
write.csv(chileapp[ , !(names(chileapp) == "Predicted_Sales_Log") ], "chile_table.csv", row.names = FALSE)
write.csv(singaporeapp[ , !(names(singaporeapp) == "Predicted_Sales_Log") ], "singapore_table.csv", row.names = FALSE)
write.csv(spainapp[ , !(names(spainapp) == "Predicted_Sales_Log") ], "spain_table.csv", row.names = FALSE)
write.csv(ukapp[ , !(names(ukapp) == "Predicted_Sales_Log") ], "uk_table.csv", row.names = FALSE)
write.csv(morroccoapp[ , !(names(morroccoapp) == "Predicted_Sales_Log") ], "morrocco_table.csv", row.names = FALSE) 



# Some plots to check if the relationship between ads and sales increase is linear
# GRPs and banners delivered per year
tv_units_per_year <- aggregate(Ad1_GRP ~ Year, data = data, sum)$Ad1_GRP
banner_units_per_year <- aggregate(Ad2_Banners ~ Year, data = data, sum)$Ad2_Banners

# Mean annual units
avg_tv_units_year <- mean(tv_units_per_year)
avg_banner_units_year <- mean(banner_units_per_year)

# Mean monthly units
avg_tv_units_month <- avg_tv_units_year/(12*5)
avg_banner_units_month <- avg_banner_units_year/(12*5)

# Cost per unit
cost_per_tvad <- tv_annual_cost / avg_tv_units_year
cost_per_banner_ad <- banner_annual_cost / avg_banner_units_year

# % increase per 10k spent
tv_units_10k <- 10000 / cost_per_tvad
banner_units_10k <- 10000 / cost_per_banner_ad

tv_pct_per_10k <- (exp(tv_coef * tv_units_10k) - 1) * 100

banner_pct_per_10k <- (exp(banner_coef * banner_units_10k) - 1) * 100
    
tv_units_month <- (tv_annual_cost / 12) / (cost_per_tvad*5)
banner_units_month <- (banner_annual_cost / 12) / (cost_per_banner_ad*5)

tv_sales_uplift <- (exp(tv_coef * tv_units_month) - 1) * 100
banner_sales_uplift <- (exp(banner_coef * banner_units_month) - 1) * 100

require(ggplot2)
spend_seq <- seq(1000000, 1.5*tv_annual_cost, by = 10000)
tv_units_seq <- spend_seq / cost_per_tvad
tv_pct_seq <- (exp(((tv_coef * tv_units_seq)/(5*12))) - 1) * 100


# Create dataframe of spending and it's associated % increase
plot_df <- data.frame(
  TVSpend1m = spend_seq/1000000,
  Pct_Increase = tv_pct_seq
)

# Plot the data
ggplot(plot_df, aes(TVSpend1m, Pct_Increase)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(
    title = "Predicted Percentage Increase in Monthly Sales\nas a Function of TV Ad Spend",
    x = "TV Spend (£Millions)",
    y = "Estimated % Increase in Monthly Sales"
  ) +
  theme_minimal()

spend_seq <- seq(0.5*banner_annual_cost, 1.5*banner_annual_cost, by = 10000)
banner_units_seq <- spend_seq / cost_per_banner_ad
banner_pct_seq <- (exp(((banner_coef * banner_units_seq)/(5*12))) - 1) * 100

# Create dataframe of spending and it's associated % increase
plot_df <- data.frame(
  bannerSpend1m = spend_seq/100000,
  Pct_Increase = banner_pct_seq
)

# Plot the data
ggplot(plot_df, aes(bannerSpend1m, Pct_Increase)) +
  geom_line(color = "red", linewidth = 1) +
  labs(
    title = "Predicted Percentage Increase in Monthly Sales\nas a Function of banner Ad Spend",
    x = "Banner Spend (£100k)",
    y = "Estimated % Increase in Monthly Sales"
  ) +
  theme_minimal()



# Final Model Summary

cat("\n Final model interpretation\n")

# Model fit statistics from original model
cat("Final Model Fit Statistics:\n")
cat("R-squared:", round(summary(final_model)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(final_model)$adj.r.squared, 4), "\n")
cat("F-statistic p-value:", round(pf(summary(final_model)$fstatistic[1], 
                                     summary(final_model)$fstatistic[2], 
                                     summary(final_model)$fstatistic[3], 
                                     lower.tail = FALSE),6), "\n")

# Key insights with significance
cat("\n Key Business Insights\n")

# Marketing effects with p-values
cat(" Marketing Effectiveness:\n")
marketing_vars <- c("Ad1_GRP", "Ad2_Banners", "Prom_Stores")
for(var in marketing_vars) {
  if (var %in% names(coefficients)) {
    coef_value <- coefficients[var]
    pvalue <- coef_summary[var, 4]
    pct_effect <- (exp(coef_value) - 1) * 100
    
    significance <- ifelse(pvalue < 0.001, " (***)", 
                           ifelse(pvalue < 0.01, " (**)",
                                  ifelse(pvalue < 0.05, " (*)", " (ns)")))
    
    cat("   ", var, ":", round(pct_effect, 2), "% sales increase per unit", significance, "\n")
  }
}

# Regional differences
cat("\n Regional Performance (vs Reference):\n")
region_vars <- c("RegionMorrocco", "RegionSingapore", "RegionSpain", "RegionUK")
for(region in region_vars) {
  if (region %in% names(coefficients)) {
    coef_value <- coefficients[region]
    pvalue <- coef_summary[region, 4]
    pct_diff <- (exp(coef_value) - 1) * 100
    
    significance <- ifelse(pvalue < 0.001, " (***)", 
                           ifelse(pvalue < 0.01, " (**)",
                                  ifelse(pvalue < 0.05, " (*)", " (ns)")))
    
    cat("   ", region, ":", round(pct_diff, 1), "% difference", significance, "\n")
  }
}


# Create time series object for Sales to check for seasonality 
#(Used in limitation part)
sales_ts <- ts(data$Sales, frequency = 12) 
log_sales_ts <- log(sales_ts)
vals <- stl(sales_ts,s.window = 12)
plot( vals, main="log - Monthly lollipops data collection 2017-2025")
str(vals)
lollipop_cycle <- vals$time.series[,'seasonal']
plot(lollipop_cycle)
lollipop_trend <- vals$time.series[,'trend']
plot(lollipop_trend)
lollipop_remainder <- vals$time.series[,'remainder']
plot(lollipop_remainder)
logStl <- stl(log(sales_ts),s.window = 12) 

plot(logStl)
par(mfrow = c(1,1))
cycle <- colMeans( matrix(ncol=12,byrow=TRUE,data = logStl$time.series[,"seasonal"]) )
plot(cycle, 
     main = "Cycle Plot Showing Monthly Seasonal Effects",
     type = "b", pch = 19)
# add data for each note
text(x = 1:12, y = cycle, 
     labels = round(cycle, 3), 
     pos = 3, cex = 0.8, col = "blue")


#plot for stepforward_selection in course, 
#placed at the very end to avoid disturbance of pop up windows
plot(stepforward_selection)




# Model diagnostics summary
cat("\n Model Diagnostics Summary:\n")
cat("   Multicollinearity: No issues (all VIF < 1.1)\n")
cat("   Normality: Residuals approximately normal (Shapiro-Wilk p =", 
    round(shapiro.test(residuals(final_model))$p.value, 3), ")\n")
cat("   Heteroscedasticity: Breusch-Pagan test p-value =", round(bp_test$p.value, 3), "\n")
cat("   Overall MAPE:", round(overall_mape, 1), "% prediction error\n")



# Save final results
save(final_model, trainingset, testset, 
     next_month_predictions, appendix_tables, 
     file = "CourseWork_Final_Model.RData")


cat("\nEnd of Analysis\n")

