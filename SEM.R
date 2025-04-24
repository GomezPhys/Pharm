library(readxl)   # If using Excel
library(dplyr)
library(ggplot2)
install.packages("lavaan")
library(lavaan)
install.packages("semPlot")
library(semPlot)
install.packages("lavaanPlot")
library(lavaanPlot)

df <- read_excel("~/pharmaflakes.xlsx", sheet = "Raw Data")
df
View(df)

df <- df %>%
  mutate(
    df$Financial_Support <- factor(df$Financial_Support, 
                                   levels = c("One", "Two", "ThreePlus"),
                                   ordered = TRUE),
    
    Interest = factor(I_Interest_in_P_PH_RC, 
                      levels = c(1, 2), 
                      labels = c("NoInterest", "Interested")),
    
    Appeal = factor(I_Appeal_Future_Student,
                    levels = c(1, 2),
                    labels = c("Appealing", "NotAppealing"))
  )

df$Financial_Support <- factor(df$Financial_Support,
                               levels = c("One", "Two", "ThreePlus"),
                               ordered = TRUE)

fit <- sem(sem_model, 
           data = df, 
           ordered = c("Interest"))

summary(fit, standardized = TRUE, fit.measures = TRUE)


######byyear####

df$Pharmacy_Year <- factor(df$Pyear, levels = c("1", "2", "3", "4"))

sem_model <- '
  Interest ~ Financial_Support + Appeal
'

fit_grouped <- sem(sem_model, 
                   data = df, 
                   group = "Pharmacy_Year",
                   ordered = c("Interest"))

summary(fit_grouped, standardized = TRUE, fit.measures = TRUE)


df$Pharmacy_Year <- factor(df$Pharmacy_Year,
                           levels = c("1", "2", "3", "4"),
                           ordered = TRUE)


sem_model_year <- '
  Interest ~ Financial_Support + Appeal + Pharmacy_Year
'


fit_year <- sem(sem_model_year, 
                data = df, 
                ordered = c("Interest"))

summary(fit_year, standardized = TRUE, fit.measures = TRUE)

df_clean <- df %>%
  rename(
    FS = Financial_Support,
    PR = Pharmacy_Year,
    AP = Appeal,
    IN = Interest
  )

model_clean <- '
  IN ~ FS + AP + PR
'

fit_clean <- sem(model_clean, 
                 data = df_clean, 
                 ordered = c("IN"))



df_clean$FS <- factor(df_clean$FS, ordered = FALSE)
df_clean$PR <- factor(df_clean$PR, ordered = FALSE)

fit_clean <- sem(model_clean, 
                 data = df_clean, 
                 ordered = c("IN"))
