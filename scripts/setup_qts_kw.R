# setup for all QTS explorations

# load packages
library(readxl)
library(tidyverse)
library(rms)
library(knitr)
library(kableExtra)

# make custom functions
round2 <- function(x) {format(round(x, 2), digits = 2)}
convert_truefalse <- function(x) {recode_factor(x,
                                                "0" = "False",
                                                "1" = "True",
                                                "0.5" = "Other")}
convert_yesno <- function(x) {recode_factor(x,
                                            "0" = "No",
                                            "1" = "Yes",
                                            "0.5" = "Other",
                                            .default = "Other",
                                            "NA" = "NA")}
convert_yesmaybeno <- function(x) {recode_factor(x,
                                                 "0" = "No",
                                                 "0.5" = "Maybe",
                                                 "1" = "Yes",
                                                 .default = "Other",
                                                 "NA" = "NA")}
convert_often <- function(x) {recode_factor(x,
                                            "1" = "Yearly",
                                            "2" = "Monthly",
                                            "3" = "Weekly",
                                            "4" = "Daily",
                                            .default = "Other",
                                            "NA" = "NA")}
convert_often2 <- function(x) {recode_factor(x,
                                             "0" = "Never",
                                             "1" = "Rarely",
                                             "2" = "Often",
                                             "3" = "Very Often",
                                             .default = "Other",
                                             "NA" = "NA")}
convert_often3 <- function(x) {recode_factor(x,
                                             "0" = "Never",
                                             "1" = "Rarely",
                                             "2" = "Sometimes",
                                             "3" = "Often",
                                             "4" = "Always",
                                             .default = "Other",
                                             "NA" = "NA")}
convert_consistfreq <- function(x) {recode_factor(x,
                                                  "1" = "Single incident",
                                                  "2" = "Inconsist., infreq.",
                                                  "3" = "Consist., infreq.",
                                                  "4" = "Consist., freq.",
                                                  .default = "Other",
                                                  "NA" = "NA")}
convert_consistfreq_exact <- function(x) {
  recode_factor(x,
                "1" = "Once [single incident]",
                "2" = "Several times [inconsistent and infrequent]",
                "3" = "Once per year [consistent but infrequent]",
                "4" = "More than once per year [frequent and consistent]",
                .default = "Other",
                "NA" = "NA")
}
convert_important <- function(x) {recode_factor(x,
                                                "0" = "Not at all important",
                                                "1" = "Somewhat important",
                                                "2" = "Important",
                                                "3" = "Very important",
                                                .default = "Other",
                                                "NA" = "NA")}
convert_present <- function(x) {recode_factor(x,
                                              "0" = "Not present",
                                              "1" = "Present but uncertain",
                                              "2" = "Persuaded of issue",
                                              .default = "Other",
                                              "NA" = "NA")}
convert_encourage <- function(x) {recode_factor(x,
                                                "0" = "Strongly discourage",
                                                "1" = "Discourage",
                                                "2" = "Encourage",
                                                "3" = "Strongly encourage",
                                                .default = "Other",
                                                "NA" = "NA")}
# make function to create pretty histograms
custom_histo <- function(x) {
  
  q_text <- data.frame(key %>% 
                         filter(!is.na(question_text), 
                                question == x))$question_text
  q_text <- gsub("\\*\\*", "", q_text)
  q_text <- gsub("&rsquo;", "'", q_text)
  q_text <- paste("Question text", q_text, sep = ": ")
  q_text <- gsub('(.{1,80})(\\s|$)', '\\1\n', q_text)
  
  df_plot <- d %>%
    count(country, researcher, urban_rural, charismatic_local, 
          !!as.symbol(x)) %>%
    group_by(country, researcher, urban_rural, charismatic_local) %>%
    mutate(prop = n/sum(n, na.rm = T),
           label = paste0(as.numeric(round2(prop)) * 100, "%")) %>%
    rename(var = !!as.symbol(x)) %>%
    mutate(is_value = !is.na(var))
  
  plot <- df_plot %>%
    ggplot(aes(x = var, y = n, 
               fill = researcher, alpha = is_value,
               label = label)) +
    facet_grid(urban_rural ~ charismatic_local ~ country) +
    geom_bar(stat = "identity") +
    geom_text(size = 2, nudge_y = 2) +
    scale_fill_manual(values = custom_pal) +
    scale_alpha_discrete(guide = "none", range = c(0.5, 1)) +
    labs(title = paste("Distribution of responses:", x),
         subtitle = q_text,
         x = "Response",
         y = "Count",
         fill = "Researcher") +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    guides(fill = guide_legend(ncol = 6, byrow = TRUE))
  
  return(plot)
}

# make custom palette for 6 fieldworkers
custom_pal <- c("#1b9e77", "#e6ab02", "#d95f02", 
                "#7570b3", "#e7298a", "#66a61e")

# load question key for SC
key_sc <- read_excel("/Users/kweisman/Documents/Research (Stanford)/Projects/Templeton Grant/DATA WRANGLING/templeton_qts/_from Nikki/GuidetoQTS.xlsx", sheet = 1) %>%
  rename(question_text = `Original Question`,
         question = `New Variable`,
         description = `Coding/description`,
         response_options = `Likert Options`) %>%
  mutate_all(funs(trimws))

# load question key for TAT
key_tat <- read_excel("/Users/kweisman/Documents/Research (Stanford)/Projects/Templeton Grant/DATA WRANGLING/templeton_qts/_from Nikki/GuidetoQTS.xlsx", sheet = 2) %>%
  rename(question_text = `Original Question`,
         question = `New Variable`,
         description = Description,
         coding = Coding,
         response_options = `Likert Options`) %>%
  mutate_all(funs(trimws))