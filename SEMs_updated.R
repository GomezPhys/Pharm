# Load libraries
library(readxl)   # If using Excel
library(dplyr)
library(ggplot2)
library(lavaan)
library(semPlot)
library(lavaanPlot)
# Load data
df <- read_excel("~/pharmaflakes.xlsx", sheet = "Raw Data")

# Recode and label variables
df <- df %>%
  mutate(
    Financial_Support = factor(SF_FT_Outcomes, 
                               levels = c(1, 2, 3, 4),
                               labels = c("One", "Two", "Three", "Four")),
    
    Interest = factor(I_Interest_in_P_PH_RC, 
                      levels = c(1, 2), 
                      labels = c("NoInterest", "Interested")),
    
    Appeal = factor(I_Appeal_Future_Student,
                    levels = c(1, 2),
                    labels = c("Appealing", "NotAppealing"))
  )
df <- df %>%
  mutate(
    FS_Two = ifelse(Financial_Support == "Two", 1, 0),
    FS_ThreePlus = ifelse(Financial_Support == "ThreePlus", 1, 0)
  )



# Define SEM model
sem_model <- '
  Interest ~ FS_Two + FS_ThreePlus + Appeal
'

# Refit the model
fit <- sem(sem_model, 
           data = df, 
           ordered = c("Interest"))

# Check the output
summary(fit, standardized = TRUE, fit.measures = TRUE)

semPaths(fit,
         whatLabels = "std",
         style = "lisrel",
         layout = "tree",
         edge.label.cex = 1.2,
         sizeMan = 6,
         sizeLat = 7,
         color = list(lat = "lightblue", man = "gray80"),
         # Optional renaming of nodes:
         labels = c(
           "Appeal",
           "Interest",
           "FS: Two Sources",
           "FS: Three+ Sources"
         ))


labels = c("Appeal", "Interest", "FS: Two Sources", "FS: Three+ Sources")

lavNames(fit, type = "ov")

semPaths(fit,
         whatLabels = "std",
         style = "lisrel",
         layout = "tree",
         edge.label.cex = 1.2,
         sizeMan = 6,
         sizeLat = 7,
         color = list(lat = "lightblue", man = "gray80"),
         labels = c(
           "FS: Two Sources",        # FS_Two
           "FS: Three+ Sources",     # FS_ThreePlus
           "Program Appeal",         # Appeal
           "Interest in MPH"         # Interest
         ))

semPlot::semPaths(fit, what = "paths", whatLabels = "name")


semPaths(fit,
         what = "std",                  # show standardized estimates
         whatLabels = "std",           
         style = "lisrel",             # classic SEM style
         layout = "tree",              # clear directional flow
         edge.label.cex = 1.2,
         sizeMan = 6,
         sizeLat = 7,
         nCharNodes = 0,               # don't truncate names
         residScale = 8,               # manage spacing for residuals
         nodeLabels = c(               # final, correct number of labels
           "FS: Two Sources",
           "FS: Three+ Sources",
           "Program Appeal",
           "Interest in MPH"
         ))

semPlot::semPaths(fit, whatLabels = "name", nCharNodes = 0)

semPlotModel(fit)$Vars$name
.

lavaanPlot(model = fit,
           coefs = TRUE,
           stand = TRUE,
           stars = "regress",
           edge_options = list(color = "black", label.cex = 1.2),
           node_options = list(shape = "box", fontname = "Arial", fontsize = 12),
           node_labels = list(
             FS_Two = "FS: Two Sources",
             FS_ThreePlus = "FS: Three+ Sources",
             Appeal = "Program Appeal",
             Interest = "Interest in MPH"
           ))


summary(fit, standardized = TRUE, fit.measures = TRUE)

install.packages("remotes")
remotes::install_github("jesseleung/lavaanPlot")
library(DiagrammeR)

grViz("
digraph SEM {

  # Node definitions with shape and color
  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12, color = lightgray]
  
  FS_Two [label = 'FS: Two Sources']
  FS_Three [label = 'FS: Three+ Sources']
  AP [label = 'Program Appeal']
  IN [label = 'Interest in MPH', fillcolor = lightblue]

  # Edges with path estimates (replace with actual values if needed)
  FS_Two -> IN [label = '0.010']
  FS_Three -> IN [label = '0.025']
  AP -> IN [label = '-1.196***']
}
")


grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12, color = lightgray]

  FS_Two [label = 'FS: Two Sources']
  FS_Three [label = 'FS: Three+ Sources']
  Interest [label = 'Interest in MPH', fillcolor = lightblue]

  FS_Two -> Interest [label = '-0.373']
  FS_Three -> Interest [label = '0.147']
}
")

#####InterestvsFinancialOutocmes#####
df <- df %>%
  mutate(
    Financial_Support = factor(SF_FT_Outcomes, 
                               levels = c(1, 2, 3, 4),
                               labels = c("One", "Two", "Three", "Four")),
    
    Interest = factor(I_Interest_in_P_PH_RC, 
                      levels = c(1, 2), 
                      labels = c("NoInterest", "Interested")),
    
    FS_Two = ifelse(Financial_Support == "Two", 1, 0),
    FS_Three = ifelse(Financial_Support == "Three", 1, 0),
    FS_Four = ifelse(Financial_Support == "Four", 1, 0)
    
    
    sem_model <- '
  Interest ~ FS_Two + FS_Three + FS_Four
'
    
    fit <- sem(sem_model, 
               data = df, 
               ordered = c("Interest"))
    
    summary(fit, standardized = TRUE, fit.measures = TRUE)

    library(DiagrammeR)

    library(DiagrammeR)
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12, color = lightgray]

  FS_Two   [label = 'FS: Two Sources']
  FS_Three [label = 'FS: Three Sources']
  FS_Four  [label = 'FS: Four Sources']
  Interest [label = 'Interest in MPH Program', fillcolor = lightblue]

  FS_Two   -> Interest [label = '-0.393']
  FS_Three -> Interest [label = '-0.029']
  FS_Four  -> Interest [label = '0.054']
}
")
    
######InterestvsPHcourse######
    
    df <- df %>%
      mutate(
        PH_Course = factor(`A_PH Course Hx`, 
                           levels = c(1, 2, 3),
                           labels = c("Yes", "No", "Unsure")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    
    df <- df %>%
      mutate(
        PH_No = ifelse(PH_Course == "No", 1, 0),
        PH_Unsure = ifelse(PH_Course == "Unsure", 1, 0)
      )
    
    
    sem_model_PH <- '
  Interest ~ PH_No + PH_Unsure
'
    
    sem_model_PH <- '
  Interest ~ PH_No + PH_Unsure
'
    
    fit_PH <- sem(sem_model_PH,
                  data = df,
                  ordered = c("Interest"))
    
    summary(fit_PH, standardized = TRUE, fit.measures = TRUE)
    
    library(DiagrammeR)
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12, color = lightgray]

  PH_No     [label = 'No PH Course']
  PH_Unsure [label = 'Unsure if PH Course']
  Interest  [label = 'Interest in MPH', fillcolor = lightblue]

  PH_No     -> Interest [label = '-0.209']
  PH_Unsure -> Interest [label = '0.416']
}
")
    
    
    
########InterestvsPreviousPH_Recoded#####
    
    
    df <- read_excel("~/pharmaflakes.xlsx", sheet = "Raw Data")
    
    df <- df %>%
      mutate(
        PH_Course_RC = factor(`A_PH Course Hx`, 
                           levels = c(1, 2),
                           labels = c("Yes", "No")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    df <- df %>%
      mutate(
        PH_No = ifelse(PH_Course_RC == "No", 1, 0)
      )
    
    table(df$PH_Course_RC, df$PH_No)
    
    
    library(lavaan)
    
    sem_model_bin <- '
  Interest ~ PH_No
'
    
    fit_bin <- sem(sem_model_bin,
                   data = df,
                   ordered = c("Interest"))
    
    summary(fit_bin, standardized = TRUE, fit.measures = TRUE)
    
    library(DiagrammeR)
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  PH_Yes [label = 'PH Course: Yes\\n(reference)', style = dashed, color = gray]
  PH_No  [label = 'PH Course: No', style = filled, fillcolor = lightgray]
  Interest [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  PH_No -> Interest [label = '-0.209']
}
")
    
    
  
    
########interestvsfamiliar#####
    
    df <- df %>%
      mutate(
        Familiar = factor(`A_Career Op`, 
                              levels = c(1, 2, 3),
                              labels = c("Not_at_all", "Slightly_Familiar", "Very_Familiar")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    df <- df %>%
      mutate(
        Fam_Slightly = ifelse(Familiar == "Slightly_Familiar", 1, 0),
        Fam_Very = ifelse(Familiar == "Very_Familiar", 1, 0),
      )
    
    
    sem_model_fam <- '
  Interest ~ Fam_Slightly + Fam_Very 
'
    
    fit_fam <- sem(sem_model_fam,
                   data = df,
                   ordered = c("Interest"))
    
    summary(fit_fam, standardized = TRUE, fit.measures = TRUE)
    
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  Fam_None     [label = 'Familiarity: Not at all\\n(reference)', style = dashed, color = gray]
  Fam_Slightly [label = 'Familiarity: Slightly', style = filled, fillcolor = lightgray]
  Fam_Very     [label = 'Familiarity: Very', style = filled, fillcolor = lightgray]
  Interest     [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  Fam_Slightly -> Interest [label = '0.434']
  Fam_Very     -> Interest [label = '0.149']
}
")
    

######interestvsaligmnet#####
    
    df <- df %>%
      mutate(
        Aligment = factor(`I_PH Aligment_(Goals)`, 
                          levels = c(1, 2, 3, 4),
                          labels = c("Not_at_all", "Slightly", "Very", "Extremely")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    df <- df %>%
      mutate(
        Align_Slightly = ifelse(Aligment == "Slightly", 1, 0),
        Align_Very     = ifelse(Aligment == "Very", 1, 0),
        Align_Extremely = ifelse(Aligment == "Extremely", 1, 0)
      )
    
    
    sem_model_align <- '
  Interest ~ Align_Slightly + Align_Very + Align_Extremely
'
    
    fit_align <- sem(sem_model_align,
                     data = df,
                     ordered = c("Interest"))
    
    table(df$Aligment)
    
    
    summary(fit_align, standardized = TRUE, fit.measures = TRUE)
    
    df <- df %>%
      mutate(
        Align3 = case_when(
          Aligment == "Not_at_all" ~ "Low",
          Aligment == "Slightly" ~ "Moderate",
          Aligment %in% c("Very", "Extremely") ~ "High"
        ),
        Align3 = factor(Align3, levels = c("Low", "Moderate", "High"))  # Set Low as reference
      )
    
    
    df <- df %>%
      mutate(
        Align_Moderate = ifelse(Align3 == "Moderate", 1, 0),
        Align_High     = ifelse(Align3 == "High", 1, 0)
      )
    
    
    
    sem_model_align3 <- '
  Interest ~ Align_Moderate + Align_High
'
    
    fit_align3 <- sem(sem_model_align3,
                      data = df,
                      ordered = c("Interest"))
    
    summary(fit_align3, standardized = TRUE, fit.measures = TRUE)
    
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  Align_Low      [label = 'Low Alignment\\n(reference)', style = dashed, color = gray]
  Align_Moderate [label = 'Moderate Alignment', style = filled, fillcolor = lightgray]
  Align_High     [label = 'High Alignment', style = filled, fillcolor = lightgray]
  Interest       [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  Align_Moderate -> Interest [label = '0.917**']
  Align_High     -> Interest [label = '1.668***']
}
")

    
    
#####AppealvsInterest######
    df <- df %>%
      mutate(
        Appeal = factor(`I_Appeal_Future_Student`, 
                          levels = c(1, 2),
                          labels = c("Not Appealing", "Appealing")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    df <- df %>%
      mutate(
        Appeal_Appealing = ifelse(Appeal == "Appealing", 1, 0)
      )
    
    sem_model_appeal <- '
  Interest ~ Appeal_Appealing
'
    
    fit_appeal <- sem(sem_model_appeal,
                      data = df,
                      ordered = c("Interest"))
    
    summary(fit_appeal, standardized = TRUE, fit.measures = TRUE)
    
    
    library(DiagrammeR)
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  Appeal_Not [label = 'Not Appealing\\n(reference)', style = dashed, color = gray]
  Appeal_Yes [label = 'Appealing', style = filled, fillcolor = lightgray]
  Interest   [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  Appeal_Yes -> Interest [label = '-1.140***']
}
")
    
    
#####FactorsvsInterest#####
    
    df <- df %>%
      mutate(
        Factors = factor(`Factors_RC`, 
                        levels = c(1, 2, 3),
                        labels = c("One Factor", "Two Factors", "Three or More Factors")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    
    df <- df %>%
      mutate(
        Factors_Two = ifelse(Factors == "Two Factors", 1, 0),
        Factors_ThreePlus = ifelse(Factors == "Three or More Factors", 1, 0)
      )
    
    
    sem_model_factors <- '
  Interest ~ Factors_Two + Factors_ThreePlus
'
    
    fit_factors <- sem(sem_model_factors,
                       data = df,
                       ordered = c("Interest"))
    
    summary(fit_factors, standardized = TRUE, fit.measures = TRUE)
    
    library(DiagrammeR)
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  Factors_One    [label = 'One Factor\\n(reference)', style = dashed, color = gray]
  Factors_Two    [label = 'Two Factors', style = filled, fillcolor = lightgray]
  Factors_Three  [label = 'Three+ Factors', style = filled, fillcolor = lightgray]
  Interest       [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  Factors_Two   -> Interest [label = '0.505']
  Factors_Three -> Interest [label = '1.310***']
}
")
    
    
    
#######MotivationvsInterest#####
    
    df <- df %>%
      mutate(
        Motivation = factor(`Motivation_RC`, 
                         levels = c(1, 2, 3),
                         labels = c("One Factor", "Two Factors", "Three or More Factors")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    df <- df %>%
      mutate(
        Mot_Two = ifelse(Motivation == "Two Factors", 1, 0),
        Mot_ThreePlus = ifelse(Motivation == "Three or More Factors", 1, 0)
      )
    
    
    sem_model_motivation <- '
  Interest ~ Mot_Two + Mot_ThreePlus
'
    
    fit_motivation <- sem(sem_model_motivation,
                          data = df,
                          ordered = c("Interest"))
    
    summary(fit_motivation, standardized = TRUE, fit.measures = TRUE)
    
    library(DiagrammeR)
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  Mot_One    [label = 'One Motivation\\n(reference)', style = dashed, color = gray]
  Mot_Two    [label = 'Two Motivations', style = filled, fillcolor = lightgray]
  Mot_Three  [label = 'Three+ Motivations', style = filled, fillcolor = lightgray]
  Interest   [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  Mot_Two   -> Interest [label = '0.840']
  Mot_Three -> Interest [label = '0.888']
}
")
    
    
#####CostvsInterest#####
    
    df <- df %>%
      mutate(
        Cost = factor(`C_Additional Cost`, 
                            levels = c(2, 3, 4),
                            labels = c("Slightly", "Very", "Extremely")),
        
        Interest = factor(I_Interest_in_P_PH_RC, 
                          levels = c(1, 2), 
                          labels = c("NoInterest", "Interested"))
      )
    
    df <- df %>%
      mutate(
        Cost_Very = ifelse(Cost == "Very", 1, 0),
        Cost_Extremely = ifelse(Cost == "Extremely", 1, 0)
      )
    
    
    sem_model_cost <- '
  Interest ~ Cost_Very + Cost_Extremely
'
    
    fit_cost <- sem(sem_model_cost,
                    data = df,
                    ordered = c("Interest"))
    
    summary(fit_cost, standardized = TRUE, fit.measures = TRUE)
    
    
    grViz("
digraph SEM {

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12]

  Cost_Slightly   [label = 'Slightly Concerned\\n(reference)', style = dashed, color = gray]
  Cost_Very       [label = 'Very Concerned', style = filled, fillcolor = lightgray]
  Cost_Extremely  [label = 'Extremely Concerned', style = filled, fillcolor = lightgray]
  Interest        [label = 'Interest in MPH', style = filled, fillcolor = lightblue]

  Cost_Very      -> Interest [label = '0.270']
  Cost_Extremely -> Interest [label = '-0.533']
}
")
    
    
  
    
    
#######POsterFigures#####
    
    df <- df %>%
      mutate(
        Cost_Level = factor(`C_Additional Cost`,
                            levels = c(1, 2, 3, 4),
                            labels = c("Not at all", "Slightly", "Very", "Extremely")),
        
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("No Interest", "Interested"))
      )
    
    library(ggplot2)
    
    ggplot(df, aes(x = Cost_Level, fill = Interest)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Interest in Dual Degree by Level of Cost Concern",
        x = "Level of Concern About Additional Cost",
        y = "Proportion of Students",
        fill = "Interest"
      ) + theme_minimal()
    
    df_filtered <- df %>%
      filter(!is.na(`C_Additional Cost`)) %>%
      mutate(
        Cost_Level = factor(`C_Additional Cost`,
                            levels = c(1, 2, 3, 4),
                            labels = c("Not at all", "Slightly", "Very", "Extremely")),
        
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("No Interest", "Interested"))
      )
    
    ggplot(df_filtered, aes(x = Cost_Level, fill = Interest)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Interest in Dual Degree by Level of Cost Concern",
        x = "Level of Concern About Additional Cost",
        y = "Proportion of Students",
        fill = "Interest"
      ) +
      theme_minimal()
    
    
    
    df_test <- df %>%
      filter(!is.na(`C_Additional Cost`)) %>%
      mutate(
        Cost_Level = factor(`C_Additional Cost`, 
                            levels = c(1, 2, 3, 4),
                            labels = c("Not at all", "Slightly", "Very", "Extremely")),
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("No Interest", "Interested"))
      )
    
    table_cost_interest <- table(df_test$Cost_Level, df_test$Interest)
    
    # Run chi-square test
    chisq.test(table_cost_interest)
    
    fisher.test(table_cost_interest)
    
    library(ggplot2)
    library(dplyr)
    
    
    
    df_clean <- df %>%
      filter(!is.na(`C_Additional Cost`)) %>%
      mutate(C_Additional_Cost = factor(`C_Additional Cost`,
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Not at all", "Slightly", "Very", "Extremely")))
    
    # Create table and proportions
    df_plot <- df_clean %>%
      group_by(`C_Additional Cost`, Interest) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(`C_Additional Cost`) %>%
      mutate(prop = count / sum(count),
             label = paste0(round(prop * 100), "%"))
    
    # Plot
    ggplot(df_plot, aes(x = `C_Additional Cost`, y = prop, fill = Interest)) +
      geom_bar(stat = "identity", position = "stack", color = "white", width = 0.7) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4, color = "white") +
      scale_fill_manual(values = c("NoInterest" = "#F8766D", "Interested" = "#00BFC4")) +
      labs(
        title = "Interest in Dual Degree by Level of Cost Concern",
        x = "Level of Concern About Additional Cost",
        y = "Proportion of Students",
        fill = "Interest"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right"
      )
    
    
    library(ggplot2)
    library(dplyr)
    
    # Clean and format data
    df_clean <- df %>%
      filter(!is.na(`C_Additional Cost`)) %>%
      mutate(
        Cost_Concern = factor(`C_Additional Cost`,
                              levels = c(1, 2, 3, 4),
                              labels = c("Not at all", "Slightly", "Very", "Extremely"))
      )
    
    df <- df %>%
      mutate(
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("No Interest", "Interested"))
      )
    
    # Create frequency table with proportions
    df_plot <- df_clean %>%
      group_by(Cost_Concern, Interest) %>%
      summarise(n = n(), .groups = "drop") %>%
      tidyr::complete(Cost_Concern, Interest, fill = list(n = 0)) %>%  # Ensures all combos appear
      group_by(Cost_Concern) %>%
      mutate(prop = n / sum(n), label = paste0(round(prop * 100), "%"))
    
    # Final plot
    ggplot(df_plot, aes(x = Cost_Concern, y = prop, fill = Interest)) +
      geom_bar(stat = "identity", position = "stack", width = 0.75, color = "white") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                color = "white", size = 5, fontface = "bold") +
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
      scale_fill_manual(values = c("Interested" = "#e69f00", "NoInterest" = "#d55e00")) +
      labs(
        title = "Interest in Dual Degree by Level of Cost Concern\n(Fisher's Exact Test, p = 0.026)",
        x = "Level of Concern About Additional Cost",
        y = "Proportion of Students (%)",
        fill = "Interest"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right"
      )
    
    library(tidyr)
    
    # Step 1: Clean and recode variables
    df_clean <- df %>%
      filter(!is.na(`C_Additional Cost`), !is.na(I_Interest_in_P_PH_RC)) %>%
      mutate(
        Cost_Concern = factor(`C_Additional Cost`,
                              levels = c(1, 2, 3, 4),
                              labels = c("Not at all", "Slightly", "Very", "Extremely")),
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("No Interest", "Interested"))
      )
    
    # Step 2: Contingency table and Fisher's Exact Test
    table_cost_interest <- table(df_clean$Cost_Concern, df_clean$Interest)
    fisher_p <- fisher.test(table_cost_interest)$p.value
    fisher_p_text <- paste0("Fisher's Exact Test, p = ", formatC(fisher_p, format = "f", digits = 3))
    
    # Step 3: Prep data for plotting
    df_plot <- df_clean %>%
      count(Cost_Concern, Interest) %>%
      complete(Cost_Concern, Interest, fill = list(n = 0)) %>%
      group_by(Cost_Concern) %>%
      mutate(
        prop = n / sum(n),
        label = paste0(round(prop * 100), "%")
      )
    
    
    library(scales)
    
    # Step 4: Plot
    ggplot(df_plot, aes(x = Cost_Concern, y = prop, fill = Interest)) +
      geom_bar(stat = "identity", position = "stack", color = "white") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 4, color = "white", fontface = "bold") +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("gray40", "orange")) +
      labs(
        title = paste("Interest in Dual Degree by Level of Cost Concern\n(", fisher_p_text, ")"),
        x = "Level of Concern About Additional Cost",
        y = "Proportion of Students (%)",
        fill = "Interest"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.position = "right"
      )
    
    
    
    
    ######second thing
    
    
    df <- df %>%
      mutate(
        Financial_Support = factor(SF_Total_Outcomes,
                                   levels = c(1, 2),
                                   labels = c("One", "Multiple")),
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("NoInterest", "Interested"))
      )
    
    
    # Create a contingency table
    table_financial_interest <- table(df$Financial_Support, df$Interest)
    
    # View the table
    print(table_financial_interest)
    
    # Fisher's exact test (since sample size may be small)
    fisher.test(table_financial_interest)
    
    # Prepare data for visualization
    df_plot <- df %>%
      count(Financial_Support, Interest) %>%
      group_by(Financial_Support) %>%
      mutate(percent = n / sum(n))
    
    # Plot
    ggplot(df_plot, aes(x = Financial_Support, y = percent, fill = Interest)) +
      geom_bar(stat = "identity", position = "fill", color = "black") +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = "Interest in Dual Degree by Financial Support",
        x = "Number of Financial Support Sources",
        y = "Proportion",
        fill = "Interest"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    
    # Recode variables
    df <- df %>%
      mutate(
        Financial_Support = factor(SF_Total_Outcomes,
                                   levels = c(1, 2),
                                   labels = c("One", "Multiple")),
        Interest = factor(I_Interest_in_P_PH_RC,
                          levels = c(1, 2),
                          labels = c("No Interest", "Interested"))
      )
    
    # Remove NA values
    df_clean <- df %>%
      filter(!is.na(Financial_Support), !is.na(Interest))
    
    # Create proportion table
    plot_data <- df_clean %>%
      count(Financial_Support, Interest) %>%
      group_by(Financial_Support) %>%
      mutate(prop = n / sum(n),
             pct_label = paste0(round(prop * 100), "%"))
    
    # Run Fisher's Exact Test
    table_support_interest <- table(df_clean$Financial_Support, df_clean$Interest)
    fisher_result <- fisher.test(table_support_interest)
    p_value <- round(fisher_result$p.value, 3)
    
    # Plot
    ggplot(plot_data, aes(x = Financial_Support, y = prop, fill = Interest)) +
      geom_col(position = "stack", color = "white") +
      geom_text(aes(label = pct_label),
                position = position_stack(vjust = 0.5),
                color = "white", size = 4.5, fontface = "bold") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("gray40", "#E69F00")) +
      labs(
        title = paste("Interest in Dual Degree by Financial Support\n(Fisher's Exact Test, p =", p_value, ")"),
        x = "Number of Financial Support Sources",
        y = "Proportion of Students (%)",
        fill = "Interest"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    