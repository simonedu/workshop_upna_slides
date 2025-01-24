
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
## ------------------- ARTIFICIAL LANGUAGE: ALIEN PARADIGM-----------------------
##                                                                            --
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             Summary statistics                           ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Performance across the training blocks  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calculate_descriptives_train_blocks <- function(data, group = c(
    "Implicit", 
    "Explicit rule-search",
    "Explicit rule-presentation",
)) {
    data %>%
        filter(Group == group) %>%
        group_by(Group, Running, Subject) %>%
        summarise(
            group = Group,
            Mean = round(mean(EnableInput.ACC) * 100, 2)
        ) %>%
        distinct() %>%
        ungroup() %>%
        group_by(Running) %>%
        mutate(SD = sd(Mean)) %>%
        mutate(n = n()) %>%
        mutate(SE = (SD / sqrt(n))) %>%
        rename(Training_block = Running) %>%
        mutate(Training_block = as.factor(str_extract_all(Training_block, "(?<=Block:?)\\d+"))) %>%
        mutate(across(where(is_character), as_factor)) %>%
        group_by(n, Training_block, Group, SD, SE) %>%
        summarise(Mean = mean(Mean)) %>%
        relocate(Mean, .after = Group) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 2)) %>%
    mutate(
      Lower_ci = Mean - qt(1 - (0.05 / 2), n - 1) * SE,
      Upper_ci = Mean + qt(1 - (0.05 / 2), n - 1) * SE,
      Lower_ci = round(Lower_ci, 2),
      Upper_ci = round(Upper_ci, 2)
    ) %>%
    glimpse()

}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~  Performance on the testing blocks: Vocabulary  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calculate_descriptives_test_blocks <- function(data, group = c(
    "Implicit", 
    "Explicit rule-search",
    "Explicit rule-presentation",
)) {
    data %>%
        filter(Group == group) %>%
        group_by(Group, Running, Subject, condition) %>%
        summarise(
            Mean = round(mean(EnableInput.ACC) * 100, 2)
        ) %>%
        distinct() %>%
        ungroup() %>%
        group_by(condition, Running) %>%
        mutate(SD = sd(Mean)) %>%
        mutate(n = n()) %>%
        mutate(SE = (SD / sqrt(n))) %>%
        rename(Testing_block = Running) %>%
        mutate(Testing_block  = as.factor(str_extract_all(Testing_block,
            "(?<=Block:?)\\d+"))) %>%
        mutate(across(where(is_character), as_factor)) %>%
        group_by(n, Group,condition, Testing_block, SD, SE) %>%
        summarise(Mean = mean(Mean)) %>%
        relocate(Mean, .before = SD) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 2))  %>%
  mutate(
    Lower_ci = Mean - qt(1 - (0.05 / 2), n - 1) * SE,
    Upper_ci = Mean + qt(1 - (0.05 / 2), n - 1) * SE,
    Lower_ci = round(Lower_ci, 2),
    Upper_ci = round(Upper_ci, 2)
  ) %>%
    glimpse()
    
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Performance on the testing blocks: Syntax  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calculate_descriptives_test_blocks_syntax <- function(data, group = c(
    "Implicit", 
    "Explicit rule-search",
    "Explicit rule-presentation",
)) {
    data %>%
        filter(Group == group) %>%
        group_by(Group, Subject) %>%
        summarise(
            Mean = round(mean(EnableInputSyntax.ACC) * 100, 2)
        ) %>%
        distinct() %>%
        ungroup() %>%
        mutate(SD = sd(Mean)) %>%
        mutate(n = n()) %>%
        mutate(SE = (SD / sqrt(n))) %>%
        mutate(across(where(is_character), as_factor)) %>%
        group_by(n, Group, SD, SE) %>%
        summarise(Mean = mean(Mean)) %>%
        relocate(Mean, .before = SD) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 2)) %>%
        mutate(test_type = "non-dprime" ) %>%
    mutate(
      Lower_ci = Mean - qt(1 - (0.05 / 2), n - 1) * SE,
      Upper_ci = Mean + qt(1 - (0.05 / 2), n - 1) * SE,
      Lower_ci = round(Lower_ci, 2),
      Upper_ci = round(Upper_ci, 2)
    ) %>%
    glimpse()
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Performance on the testing blocks: Syntax dprime  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

calculate_descriptives_dprime <- function(data, group = c(
    "Implicit", 
    "Explicit rule-search",
    "Explicit rule-presentation",
)) {
    data %>%
        filter(Group == group) %>%
        group_by(Group, Subject) %>%
        summarise(
            Mean = round(mean(dprime),2)
        ) %>%
        distinct() %>%
        ungroup() %>%
        mutate(SD = sd(Mean)) %>%
        mutate(n = n()) %>%
        mutate(SE = (SD / sqrt(n))) %>%
        mutate(across(where(is_character), as_factor)) %>%
        group_by(n, Group, SD, SE) %>%
        summarise(Mean = mean(Mean)) %>%
        relocate(Mean, .before = SD) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 2)) %>%
        mutate(test_type = "d'" ) %>%
    mutate(
      Lower_ci = Mean - qt(1 - (0.05 / 2), n - 1) * SE,
      Upper_ci = Mean + qt(1 - (0.05 / 2), n - 1) * SE,
      Lower_ci = round(Lower_ci, 2),
      Upper_ci = round(Upper_ci, 2)
    ) %>%
    glimpse()
}



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Visualization                             ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Custom custom theme 

# Adapted from Hehman, E., & Xie, S. Y. (2021). Doing better data visualization.
# Advances in Methods and Practices in Psychological Science, 4(4).
# https://journals.sagepub.com/doi/pdf/10.1177/25152459211045334

theme_minimalism <- function() {
  theme_minimal() + # ggplot's minimal theme hides many unnecessary features of plot
    theme( # make modifications to the theme
      panel.grid.major.y = element_blank(), # hide major grid for y axis
      panel.grid.minor.y = element_blank(), # hide minor grid for y axis
      panel.grid.major.x = element_blank(), # hide major grid for x axis
      panel.grid.minor.x = element_blank(), # hide minor grid for x axis
      plot.background = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(), # no legend title
      legend.text = element_text(size = 12, color = "#333333"),
      axis.line = element_line(),
      text = element_text(size = 12, family = "Times New Roman"), 
      # font aesthetics
      axis.text = element_text(size = 12, color = "#333333"),
      # axis.title = element_text(size = 1, color = "#333333")#,
      legend.position = "right",
      # legend.justification = "left",
      plot.title.position = "plot",
      #   plot.caption = element_text(hjust=0),
      plot.caption.position = "plot"
    )
}


theme_minimalism_presentation <- function() {
  theme_minimal() + # ggplot's minimal theme hides many unnecessary features of plot
    theme( # make modifications to the theme
      panel.grid.major.y = element_blank(), # hide major grid for y axis
      panel.grid.minor.y = element_blank(), # hide minor grid for y axis
      panel.grid.major.x = element_blank(), # hide major grid for x axis
      panel.grid.minor.x = element_blank(), # hide minor grid for x axis
      plot.background = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(), # no legend title
      legend.text = element_text(size = 18, color = "#333333"),
      text = element_text(size = 18, family = "Nunito"), 
      # font aesthetics
      axis.text = element_text(size = 18, color = "#333333"),
      # axis.title = element_text(size = 1, color = "#333333")#,
      axis.line = element_line(),
      legend.position = "right",
      # legend.justification = "left",
      plot.title.position = "plot",
      #   plot.caption = element_text(hjust=0),
      plot.caption.position = "plot"
    )
}




theme_minimalism_legend <- function() {
  theme_minimal() + # ggplot's minimal theme hides many unnecessary features of plot
    theme( # make modifications to the theme
      panel.grid.major.y = element_blank(), # hide major grid for y axis
      panel.grid.minor.y = element_blank(), # hide minor grid for y axis
      panel.grid.major.x = element_blank(), # hide major grid for x axis
      panel.grid.minor.x = element_blank(), # hide minor grid for x axis
      plot.background = element_blank(),
      panel.border = element_blank(),
   #   legend.title = element_blank(), # no legend title
      legend.text = element_text(size = 8, color = "#333333"),
      text = element_text(size = 8, family = "Times New Roman"), 
      # font aesthetics
      axis.text = element_text(size = 8, color = "#333333"),
      # axis.title = element_text(size = 1, color = "#333333")#,
      legend.position = "top",
      # legend.justification = "left",
      #legend.title=element_blank(),
      plot.title.position = "plot",
      #   plot.caption = element_text(hjust=0),
      plot.caption.position = "plot"
    )
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             Graph with ribbons                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Custom custom theme
theme_minimalism_ribbons <- function() {
  theme_minimal() + # ggplot's minimal theme hides many unnecessary features of plot
    theme( # make modifications to the theme
      panel.grid.major.y = element_blank(), # hide major grid for y axis
      panel.grid.minor.y = element_blank(), # hide minor grid for y axis
      panel.grid.major.x = element_blank(), # hide major grid for x axis
      panel.grid.minor.x = element_blank(), # hide minor grid for x axis
      plot.background = element_blank(),
      panel.border = element_blank(),
      legend.title = element_blank(), # no legen2d title
      legend.text = element_text(size = 18, color = "#333333"),
      text = element_text(size = 18, family = "Times New Roman"), # font aesthetics
      axis.text = element_text(size = 18, color = "#333333"),
      # axis.title = element_text(size = 1, color = "#333333")#,
      legend.position = "top",
      # legend.justification = "left",
      plot.title.position = "plot",
      #   plot.caption = element_text(hjust=0),
      plot.caption.position = "plot"
    )
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Check database names                          ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#- Check if databases have the same column names
check_col_names <- function(x, y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print("Warning: Names are not the same")
      break
    } else if (i == tail(names(y), n = 1)) {
      print("Names are identical")
    }
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
## ----------------------------------- ASRT--------------------------------------
##                                                                            --
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Reading raw data files                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Explicit group  ----
##~~~~~~~~~~~~~~~~~~~~~~~~

read_raw_data_files_exp_rule_search <- function() {
    list.files(
        path = "data/raw_data/ASRT_Explicit_rule-search/",
        pattern = "*.xlsx",
        full.names = TRUE
    ) %>%
        lapply(read_excel)
}

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Implicit group  ----
##~~~~~~~~~~~~~~~~~~~~~~~~

read_raw_data_files_imp <- function() {
    list.files(
        path = "data/raw_data/ASRT_Implicit/",
        pattern = "*.xlsx",
        full.names = TRUE
    ) %>%
        lapply(read_excel)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Pattern (P) trials                          ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To find response times for Blocks 1 and 10 for patterned trials ~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - Filter to look at FirstAcc = 1 only [exclude 0 and blanks]
# - Filter to look only at P of TrialTypes
# - Look at average values of firstRT

calculate_RT_B_CorrectP <- function(data, block_number = 1:10) {
    
    df <- data %>%
        filter(firstACC == 1 & TrialType == "P") %>%
        group_by(Subject, Group, Block) %>%
        filter(Block == block_number) %>%
        summarise(RT_Correct = round(mean(firstRT), 2))
    
    block_number <- str_extract(df$Block[1], "(\\w+)")
    
    df %>%
        rename_with(
            ~ case_when(
                . == "RT_Correct" ~ glue("RT_B", block_number, "_CorrectP"),
                TRUE ~ .
            )
        ) %>%
        
        select(-Block) %>%
        glimpse()
}

# .........Acc first response for P trials, block 1 & 10 .........
#   - Filter to look at FirstAcc = 1 and 0
#   - Look at average of firstAcc values

calculate_ACC_Overall_P <- function(data) {
  data %>%
    filter(TrialType == "P") %>%
    group_by(Subject) %>%
    summarise(Acc_Overall_P = round(mean(firstACC), 2)) %>%
    glimpse()
}

# Acc first response for P trials, block 1
calculate_Acc_B1_P <- function(data) {
  data %>%
    filter(TrialType == "P" & Block == 1) %>%
    group_by(Subject) %>%
    summarise(Acc_B1_P = round(mean(firstACC), 2)) %>%
    glimpse()
}

# Acc first response for P trials, block 10
calculate_Acc_B10_P <- function(data) {
  data %>%
    filter(TrialType == "P" & Block == 10) %>%
    group_by(Subject) %>%
    summarise(Acc_B10_P = round(mean(firstACC), 2)) %>%
    glimpse()
}

# ................................................................

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Procedural learning score 1                        ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Measure used in Kate Brill-Schuetz MA thesis & Faretta-Stutenberg & Morgan-Short 2018)

#  The difference in reaction time for correct, pattern trials versus
#     correct, random trials

# Overall RT for all correct first response random trials
calculate_RT_Overall_CorrectR <- function(data) {
  data %>%
    filter(firstACC == 1 & TrialType == "R") %>%
    group_by(Subject) %>%
    summarise(RT_Overall_CorrectR = round(mean(firstRT), 2)) %>%
    glimpse()
}


# Overall RT for all correct first response pattern trials
calculate_RT_Overall_CorrectP <- function(data) {
  data %>%
    filter(firstACC == 1 & TrialType == "P") %>%
    group_by(Subject) %>%
    summarise(RT_Overall_CorrectP = round(mean(firstRT), 2)) %>%
    glimpse()
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             Random (P) trials                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#................................................................

# ..To find first response overall accuracy for random trials..

# Acc first response for R trials, overall
calculate_ACC_Overall_R <- function(data) {
  data %>%
    filter(TrialType == "R") %>%
    group_by(Subject) %>%
    summarise(Acc_Overall_R = round(mean(firstACC), 2)) %>%
    glimpse()
}


# To find response times for Blocks 1 and 10 for random trials ~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - Filter to look at FirstAcc = 1 only [exclude 0 and blanks]
# - Filter to look only at R of TrialTypes
# - Look at average values of firstRT

calculate_RT_B_CorrectR <- function(data, block_number = 1:10) {
    
    df <- data %>%
        filter(firstACC == 1 & TrialType == "R") %>%
        group_by(Subject, Group, Block) %>%
        filter(Block == block_number) %>%
        summarise(RT_Correct = round(mean(firstRT), 2))
    
    block_number <- str_extract(df$Block[1], "(\\w+)")
    
    df %>%
        rename_with(
            ~ case_when(
                . == "RT_Correct" ~ glue("RT_B", block_number, "_CorrectR"),
                TRUE ~ .
            )
        ) %>%
        
        select(-Block) %>%
        glimpse()
}

# Acc first response for R trials, block 1
calculate_Acc_B1_R <- function(data) {
    data %>%
        filter(TrialType == "R" & Block == 1) %>%
        group_by(Subject) %>%
        summarise(Acc_B1_R = round(mean(firstACC), 2)) %>%
        glimpse()
}

# Acc first response for R trials, block 10
calculate_Acc_B10_R <- function(data) {
    data %>%
        filter(TrialType == "R" & Block == 10) %>%
        group_by(Subject) %>%
        summarise(Acc_B10_R = round(mean(firstACC), 2)) %>%
        glimpse()
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   Tables                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Modify function for n in tabulator 
fmt_header_n_small <- function (n) 
{
  z1 <- character(length(n))
  z1[!is.na(n)] <- sprintf("\n(n = %.0f)", n[!is.na(n)])
  z1
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------- SUPPLEMENTARY ANALYSES-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            Performance across the training blocks by awareness           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


calculate_descriptives_train_blocks_aware <- function(data, group = c(
  "Implicit", 
  "Explicit rule-search",
  "Explicit rule-presentation",
)) {
  data %>%
    filter(Group == group) %>%
    group_by(Group, 
      Awareness, 
      Running) %>%
    summarise(
      Mean = round(mean(EnableInput.ACC) * 100, 2)
    ) %>%
    distinct() %>%
    ungroup() %>%
    group_by(Running) %>%
    mutate(SD = sd(Mean)) %>%
    mutate(n = n()) %>%
    mutate(SE = (SD / sqrt(n))) %>%
    rename(Training_block = Running) %>%
    mutate(Training_block = as.factor(str_extract_all(Training_block, "(?<=Block:?)\\d+"))) %>%
    mutate(across(where(is_character), as_factor)) %>%
    group_by(n, Awareness, Training_block, Group, SD, SE) %>%
    summarise(Mean = mean(Mean)) %>%
    relocate(Mean, .after = Group) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    mutate(
      Lower_ci = Mean - qt(1 - (0.05 / 2), n - 1) * SE,
      Upper_ci = Mean + qt(1 - (0.05 / 2), n - 1) * SE,
      Lower_ci = round(Lower_ci, 2),
      Upper_ci = round(Upper_ci, 2)
    ) %>%
    glimpse()
  
}
