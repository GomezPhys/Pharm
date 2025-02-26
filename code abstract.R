# Load necessary libraries
library(readxl)   # If using Excel
library(dplyr)
library(ggplot2)

# Load data (replace 'your_file.xlsx' and 'Sheet1' with actual file name and sheet name)
df <- read_excel("~/Pharma.xlsx")
df

# Convert SF_Total_Outcomes to factor with labels
df$SF_Total_Outcomes <- factor(df$SF_Total_Outcomes, 
                               levels = c(1, 2), 
                               labels = c("One Factor", "Multiple Factors"))

# Convert I_Interest_Level to factor with labels
df$I_Interest_Level <- factor(df$I_Interest_Level, 
                              levels = c(1, 2), 
                              labels = c("Not Interested", "Interested"))

# Check structure
str(df)

# View first few rows
head(df)


# Create contingency table
contingency_table <- table(df$SF_Total_Outcomes, df$I_Interest_Level)

# Print table
print(contingency_table)

# Perform Chi-Square Test
chi_result <- chisq.test(contingency_table)

# Print result
print(chi_result)

# Check if expected counts are < 5, use Fisher’s Exact Test
if(any(chi_result$expected < 5)){
  fisher_result <- fisher.test(contingency_table)
  print(fisher_result)
  
  logistic_model <- glm(I_Interest_Level ~ SF_Total_Outcomes, 
                        data = df, 
                        family = binomial)
  
  # Model summary
  summary(logistic_model)
  
  exp(cbind(Odds_Ratio = coef(logistic_model), confint(logistic_model)))
  
  
  # Convert categorical variables to factors
  df$SF_Total_Outcomes <- as.factor(df$SF_Total_Outcomes)
  df$I_Interest_Level <- as.factor(df$I_Interest_Level)
  
  # Run logistic regression
  logistic_model <- glm(I_Interest_Level ~ SF_Total_Outcomes, data = df, family = binomial)
  
  # Extract odds ratios and confidence intervals
  odds_ratios <- exp(coef(logistic_model))
  conf_int <- exp(confint(logistic_model))
  
  # Convert results to a data frame for plotting
  plot_data <- data.frame(
    Variable = c("Intercept (One Factor)", "Multiple Factors"),
    Odds_Ratio = odds_ratios,
    Lower_CI = conf_int[,1],
    Upper_CI = conf_int[,2]
  )
  
  ggplot(plot_data, aes(x = Odds_Ratio, y = Variable)) +
    geom_point(size = 4, color = "blue") +  # Points for odds ratios
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2, color = "blue") +  # CI bars
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Reference line at OR = 1
    xlab("Odds Ratio") +
    ylab("") +
    ggtitle("Odds Ratios with 95% Confidence Intervals") +
    theme_minimal()
  
  # Load necessary libraries
  library(ggplot2)
  
  # Ensure categorical variables are factors
  df$SF_Total_Outcomes <- as.factor(df$SF_Total_Outcomes)
  df$I_Interest_Level <- as.factor(df$I_Interest_Level)
  
  # Run logistic regression
  logistic_model <- glm(I_Interest_Level ~ SF_Total_Outcomes, data = df, family = binomial)
  
  # Extract odds ratios and confidence intervals
  odds_ratios <- exp(coef(logistic_model))
  conf_int <- exp(confint(logistic_model))
  
  # Convert results to a data frame for plotting
  plot_data <- data.frame(
    Variable = c("Intercept (One Factor)", "Multiple Factors"),
    Odds_Ratio = odds_ratios,
    Lower_CI = conf_int[,1],
    Upper_CI = conf_int[,2]
  )
  
  
  # Create the plot with log scale
  ggplot(plot_data, aes(x = Odds_Ratio, y = Variable)) +
    geom_point(size = 5, color = "blue") +  # Larger points
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.3, color = "blue", linewidth = 1.2) +  # Thicker CI bars
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +  # Reference line at OR = 1
    scale_x_log10() +  # Log scale for better visualization
    xlab("Odds Ratio (Log Scale)") +
    ylab("") +
    ggtitle("Improved Odds Ratios with 95% Confidence Intervals") +
    theme_minimal(base_size = 14)  # Larger text for readability
  
  