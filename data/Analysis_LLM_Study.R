library(ggplot2)
library(dplyr)    
library(tidyr)
library(brms)
library(lmerTest)
library(tidyverse)
library(ordinal)
library(MASS)
library(olsrr)
library(lme4)
library(emmeans)
library(reshape2)


# This file includes the following sections:
# - Data Cleaning & Preparing
# - Analysis for...
#   - Frequency of LLM Usage
#.  - Risk of LLM Usage
#.  - Benefits of LLM Usage
#.  - Ethics of LLM Usage
#.  - Comfort Level of Disclosing LLM Usage to Peers/Reviewers
# - Emmeans
# - Chi-squared Tests

#==========================Data Cleaning & Preparing============================
survey_df <- LLM_Survey_Filtered_Final

# Add Demographic Groups as IVs
group_gender <- function(gender) {
  if (is.na(gender)) {
    return(NA)
  }
  if (tolower(gender) == 'man') {
    return('Man')
  } else if (tolower(gender) == 'woman' || tolower(gender) == 'non-binary') {
    return('Non-Man')
  } else {
    return(NA)
  }
}

group_language <- function(L2) {
  if (is.na(L2)) {
    return(NA)
  }
  if (tolower(L2) == 'yes') {
    return('Native')
  } else if (tolower(L2) == 'no') {
    return('Non-Native')
  } else {
    return(NA)
  }
}

group_race <- function(input) {
  if (is.na(input)) {
    return(NA)
  }
  if (input == 'White / Caucasian') {
    return('White')
  } else if (input %in% c('Asian', 'Black or African American', 'Hispanic and Latino', 'Middle Eastern')) {
    return('Non-White')
  } else {
    return(NA)
  }
}

group_years <- function(input) {
  input <- tolower(input)
  if (is.na(input)) {
    return(NA)
  }
  if (input == '0' || input == '1-3') {
    return('0-3')
  } else if (input == '4-6' || input == '7-10') {
    return('4-10')
  } else if (input == '11+') {
    return('11+')
  } else {
    return(NA)
  }
}

# change exclude into Exclude-> N/A, run another model just with them
group_publication <- function(input) {
  input <- tolower(input)
  if (is.na(input)) {
    return(NA)
  }
  if (input == '0 and plans to publish soon' || input == '1-5') {
    return('0-5')
  } else if (input == '6-10') {
    return('6-10')
  } else if (input == '11+') {
    return('11+')
  } else {
    return(NA)
  }
}


# Apply the function to create new columns
survey_df$Gender_Group <- factor(sapply(survey_df$Q93, group_gender))
survey_df$Race_Group <- factor(sapply(survey_df$Q95, group_race))
survey_df$Language_Group <- factor(sapply(survey_df$Q77, group_language))
survey_df$Publication_Group <- factor(sapply(survey_df$Q75, group_publication))
survey_df$Year_Group <- factor(sapply(survey_df$Q76, group_years))
survey_df$ResponseId = as.factor(survey_df$ResponseId)

# Dependent Variables -- usage types
survey_df$Q26f <- factor(survey_df$Q26, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
survey_df$Q78f <- factor(survey_df$Q78, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
survey_df$Q80f <- factor(survey_df$Q80, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
survey_df$Q82f <- factor(survey_df$Q82, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
survey_df$Q86f <- factor(survey_df$Q86, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
survey_df$Q84f <- factor(survey_df$Q84, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)

survey_df$Q88_1f <- factor(survey_df$Q88_1, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
survey_df$Q88_2f <- factor(survey_df$Q88_2, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
survey_df$Q88_3f <- factor(survey_df$Q88_3, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
survey_df$Q88_4f <- factor(survey_df$Q88_4, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
survey_df$Q88_5f <- factor(survey_df$Q88_5, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
survey_df$Q88_6f <- factor(survey_df$Q88_6, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)

survey_df$Q89_1f <- factor(survey_df$Q89_1, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
survey_df$Q89_2f <- factor(survey_df$Q89_2, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
survey_df$Q89_3f <- factor(survey_df$Q89_3, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
survey_df$Q89_4f <- factor(survey_df$Q89_4, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
survey_df$Q89_5f <- factor(survey_df$Q89_5, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
survey_df$Q89_6f <- factor(survey_df$Q89_6, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)

survey_df$Q90_1f <- factor(survey_df$Q90_1, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
survey_df$Q90_2f <- factor(survey_df$Q90_2, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
survey_df$Q90_3f <- factor(survey_df$Q90_3, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
survey_df$Q90_4f <- factor(survey_df$Q90_4, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
survey_df$Q90_5f <- factor(survey_df$Q90_5, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
survey_df$Q90_6f <- factor(survey_df$Q90_6, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)

survey_df$Q91_1f <- factor(survey_df$Q91_1, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q91_2f <- factor(survey_df$Q91_2, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q91_3f <- factor(survey_df$Q91_3, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q91_4f <- factor(survey_df$Q91_4, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q91_5f <- factor(survey_df$Q91_5, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q91_6f <- factor(survey_df$Q91_6, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)

survey_df$Q92_1f <- factor(survey_df$Q92_1, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q92_2f <- factor(survey_df$Q92_2, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q92_3f <- factor(survey_df$Q92_3, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q92_4f <- factor(survey_df$Q92_4, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q92_5f <- factor(survey_df$Q92_5, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
survey_df$Q92_6f <- factor(survey_df$Q92_6, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)

comfort_map <- c("Uncomfortable" = 1, "Somewhat uncomfortable" = 2, "Neutral" = 3, "Somewhat comfortable" = 4, "Comfortable" = 5)
frequency_map <- c("Never" = 1, "Very Rarely" = 2, "Rarely" = 3, "Occasionally" = 4, "Frequently" = 5, "Very Frequently" = 6)
risky_map <- c('Not at all Risky' = 1, 'Slightly Risky' = 2, 'Moderately Risky' = 3, 'Very Risky' = 4, 'Extremely Risky' = 5)
useful_map <- c('Not at all useful' = 1, 'Slightly useful' = 2, 'Moderately useful' = 3, 'Very useful' = 4, 'Extremely useful' = 5)
ethics_map <- c("Unacceptable" = 1, "Somewhat Unacceptable" = 2, "Neutral" = 3, "Somewhat Acceptable" = 4, "Acceptable" = 5)

survey_df$Q91_1numeric <- comfort_map[survey_df$Q91_1f]
survey_df$Q91_2numeric <- comfort_map[survey_df$Q91_2f]
survey_df$Q91_3numeric <- comfort_map[survey_df$Q91_3f]
survey_df$Q91_4numeric <- comfort_map[survey_df$Q91_4f]
survey_df$Q91_5numeric <- comfort_map[survey_df$Q91_5f]
survey_df$Q91_6numeric <- comfort_map[survey_df$Q91_6f]
survey_df$Q92_1numeric <- comfort_map[survey_df$Q92_1f]
survey_df$Q92_2numeric <- comfort_map[survey_df$Q92_2f]
survey_df$Q92_3numeric <- comfort_map[survey_df$Q92_3f]
survey_df$Q92_4numeric <- comfort_map[survey_df$Q92_4f]
survey_df$Q92_5numeric <- comfort_map[survey_df$Q92_5f]
survey_df$Q92_6numeric <- comfort_map[survey_df$Q92_6f]

# ===============Frequency of LLM Usage==============
frequency_df <- survey_df %>% dplyr::select(Q26f, Q78f, Q80f, Q82f, Q86f, Q84f, Gender_Group, Race_Group, Language_Group, Year_Group, ResponseId)

questions_to_domains_freq <- c('Q26f' = 'Information seeking',
                               'Q78f' = 'Editing',
                               'Q82f' = 'Ideation & Framing',
                               'Q80f' = 'Direct Writing',
                               'Q84f' = 'Data Cleaning & Analysis',
                               'Q86f' = 'Data Generation')

mutated_df_freq <- frequency_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_freq[Question]))  # Use the mapping to get the type for each question

mutated_df_freq$value_numeric <- frequency_map[mutated_df_freq$value]
mutated_filter_gender = mutated_df_freq[!is.na(mutated_df_freq$Gender_Group) & !is.na(mutated_df_freq$Type),]
mutated_filter_race = mutated_df_freq[!is.na(mutated_df_freq$Race_Group) & !is.na(mutated_df_freq$Type),]
mutated_filter_language = mutated_df_freq[!is.na(mutated_df_freq$Language_Group) & !is.na(mutated_df_freq$Type),]
mutated_filter_year = mutated_df_freq[!is.na(mutated_df_freq$Year_Group) & !is.na(mutated_df_freq$Type),]
View(mutated_df_freq)

freq_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_filter_language)
freq_gender <- lmer(value_numeric ~ Gender_Group + Type + (1|ResponseId), data = mutated_filter_gender)
freq_gender_interaction <- lmer(value_numeric ~ Gender_Group * Type + (1|ResponseId), data = mutated_filter_gender)

freq_race <- lmer(value_numeric ~ Race_Group + Type + (1|ResponseId), data = mutated_filter_race)
freq_race_interaction <- lmer(value_numeric ~ Race_Group * Type + (1|ResponseId), data = mutated_filter_race)

freq_language <- lmer(value_numeric ~ Language_Group + Type + (1|ResponseId), data = mutated_filter_language)
freq_language_interaction <- lmer(value_numeric ~ Language_Group * Type + (1|ResponseId), data = mutated_filter_language)

freq_year <- lmer(value_numeric ~ Year_Group + Type + (1|ResponseId), data = mutated_filter_year)
freq_year_interaction <- lmer(value_numeric ~ Year_Group * Type + (1|ResponseId), data = mutated_filter_year)


# ===============Risks of LLM Usage==============
risk_df <- survey_df %>% dplyr::select(Q89_1f, Q89_2f, Q89_3f, Q89_4f, Q89_5f, Q89_6f, Gender_Group, Race_Group, Language_Group, Year_Group, ResponseId)

questions_to_domains_risk <- c('Q89_1f' = 'Information seeking',
                               'Q89_2f' = 'Editing',
                               'Q89_3f' = 'Direct Writing',
                               'Q89_4f' = 'Ideation & Framing',
                               'Q89_5f' = 'Data Generation',
                               'Q89_6f' = 'Data Cleaning & Analysis')
mutated_df_risk <- risk_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_risk[Question]))

mutated_df_risk$value_numeric <- risky_map[mutated_df_risk$value]
mutated_risk_gender = mutated_df_risk[!is.na(mutated_df_risk$Gender_Group) & !is.na(mutated_df_risk$Type),]
mutated_risk_race = mutated_df_risk[!is.na(mutated_df_risk$Race_Group) & !is.na(mutated_df_risk$Type),]
mutated_risk_language = mutated_df_risk[!is.na(mutated_df_risk$Language_Group) & !is.na(mutated_df_risk$Type),]
mutated_risk_year = mutated_df_risk[!is.na(mutated_df_risk$Year_Group) & !is.na(mutated_df_risk$Type),]
View(mutated_df_risk)

risk_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_risk_race)
risk_gender <- lmer(value_numeric ~ Gender_Group + Type + (1|ResponseId), data = mutated_risk_gender)
risk_gender_interaction <- lmer(value_numeric ~ Gender_Group * Type + (1|ResponseId), data = mutated_risk_gender)

risk_race <- lmer(value_numeric ~ Race_Group + Type + (1|ResponseId), data = mutated_risk_race)
risk_race_interaction <- lmer(value_numeric ~ Race_Group * Type + (1|ResponseId), data = mutated_risk_race)

risk_language <- lmer(value_numeric ~ Language_Group + Type + (1|ResponseId), data = mutated_risk_language)
risk_language_interaction <- lmer(value_numeric ~ Language_Group * Type + (1|ResponseId), data = mutated_risk_language)

risk_year <- lmer(value_numeric ~ Year_Group + Type + (1|ResponseId), data = mutated_risk_year)
risk_year_interaction <- lmer(value_numeric ~ Year_Group * Type + (1|ResponseId), data = mutated_risk_year)


# ===============Benefits of LLM Usage==============
useful_df <- survey_df %>% dplyr::select(Q88_1f, Q88_2f, Q88_3f, Q88_4f, Q88_5f, Q88_6f, Gender_Group, Race_Group, Language_Group, Year_Group, ResponseId)
questions_to_domains_useful <- c('Q88_1f' = 'Information seeking',
                                 'Q88_2f' = 'Editing',
                                 'Q88_3f' = 'Direct Writing',
                                 'Q88_4f' = 'Ideation & Framing',
                                 'Q88_5f' = 'Data Generation',
                                 'Q88_6f' = 'Data Cleaning & Analysis')

mutated_df_useful <- useful_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_useful[Question]))

mutated_df_useful$value_numeric <- useful_map[mutated_df_useful$value]
mutated_useful_gender = mutated_df_useful[!is.na(mutated_df_useful$Gender_Group) & !is.na(mutated_df_useful$Type),]
mutated_useful_race = mutated_df_useful[!is.na(mutated_df_useful$Race_Group) & !is.na(mutated_df_useful$Type),]
mutated_useful_language = mutated_df_useful[!is.na(mutated_df_useful$Language_Group) & !is.na(mutated_df_useful$Type),]
mutated_useful_year = mutated_df_useful[!is.na(mutated_df_useful$Year_Group) & !is.na(mutated_df_useful$Type),]

useful_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_useful_race)
useful_gender <- lmer(value_numeric ~ Gender_Group + Type + (1|ResponseId), data = mutated_useful_gender)
useful_gender_interaction <- lmer(value_numeric ~ Gender_Group * Type + (1|ResponseId), data = mutated_useful_gender)

useful_race <- lmer(value_numeric ~ Race_Group + Type + (1|ResponseId), data = mutated_useful_race)
useful_race_interaction <- lmer(value_numeric ~ Race_Group * Type + (1|ResponseId), data = mutated_useful_race)

useful_language <- lmer(value_numeric ~ Language_Group + Type + (1|ResponseId), data = mutated_useful_language)
useful_language_interaction <- lmer(value_numeric ~ Language_Group * Type + (1|ResponseId), data = mutated_useful_language)

useful_year <- lmer(value_numeric ~ Year_Group + Type + (1|ResponseId), data = mutated_useful_year)
useful_year_interaction <- lmer(value_numeric ~ Year_Group * Type + (1|ResponseId), data = mutated_useful_year)

# ===============Ethics of LLM Usage==============
ethics_df <- survey_df %>% dplyr::select(Q90_1f, Q90_2f, Q90_3f, Q90_4f, Q90_5f, Q90_6f, Gender_Group, Race_Group, Language_Group, Year_Group, ResponseId)
questions_to_domains_ethics <- c('Q90_1f' = 'Information seeking',
                                 'Q90_2f' = 'Editing',
                                 'Q90_3f' = 'Direct Writing',
                                 'Q90_4f' = 'Ideation & Framing',
                                 'Q90_5f' = 'Data Generation',
                                 'Q90_6f' = 'Data Cleaning & Analysis')

mutated_df_ethics <- ethics_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_ethics[Question]),  # Use the mapping to get the type for each question
         Answers = value)

mutated_df_ethics$value_numeric <- ethics_map[mutated_df_ethics$value]
mutated_ethics_gender = mutated_df_ethics[!is.na(mutated_df_ethics$Gender_Group) & !is.na(mutated_df_ethics$Type),]
mutated_ethics_race = mutated_df_ethics[!is.na(mutated_df_ethics$Race_Group) & !is.na(mutated_df_ethics$Type),]
mutated_ethics_language = mutated_df_ethics[!is.na(mutated_df_ethics$Language_Group) & !is.na(mutated_df_ethics$Type),]
mutated_ethics_year = mutated_df_ethics[!is.na(mutated_df_ethics$Year_Group) & !is.na(mutated_df_ethics$Type),]

ethics_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_ethics_race)
ethics_gender <- lmer(value_numeric ~ Gender_Group + Type + (1|ResponseId), data = mutated_ethics_gender)
ethics_gender_interaction <- lmer(value_numeric ~ Gender_Group * Type + (1|ResponseId), data = mutated_ethics_gender)

ethics_race <- lmer(value_numeric ~ Race_Group + Type + (1|ResponseId), data = mutated_ethics_race)
ethics_race_interaction <- lmer(value_numeric ~ Race_Group * Type + (1|ResponseId), data = mutated_ethics_race)

ethics_language <- lmer(value_numeric ~ Language_Group + Type + (1|ResponseId), data = mutated_ethics_language)
ethics_language_interaction <- lmer(value_numeric ~ Language_Group * Type + (1|ResponseId), data = mutated_ethics_language)

ethics_year <- lmer(value_numeric ~ Year_Group + Type + (1|ResponseId), data = mutated_ethics_year)
ethics_year_interaction <- lmer(value_numeric ~ Year_Group * Type + (1|ResponseId), data = mutated_ethics_year)


# ===============Comfort Level of Disclosing to Peers==============
comfort_peers_df <- survey_df %>% dplyr::select(Q91_1numeric, Q91_2numeric, Q91_3numeric, Q91_4numeric, Q91_5numeric, Q91_6numeric, Gender_Group, Race_Group, Language_Group, Year_Group, ResponseId)

questions_to_domains_peers <- c('Q91_1numeric' = 'Information seeking',
                                'Q91_2numeric' = 'Editing',
                                'Q91_3numeric' = 'Direct Writing',
                                'Q91_4numeric' = 'Ideation & Framing',
                                'Q91_5numeric' = 'Data Generation',
                                'Q91_6numeric' = 'Data Cleaning & Analysis')

mutated_df_peers <- comfort_peers_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_peers[Question]))

mutated_df_peers = mutated_df_peers[!is.na(mutated_df_peers$value),]
mutated_peers_gender = mutated_df_peers[!is.na(mutated_df_peers$Gender_Group) & !is.na(mutated_df_peers$Type),]
mutated_peers_race = mutated_df_peers[!is.na(mutated_df_peers$Race_Group) & !is.na(mutated_df_peers$Type),]
mutated_peers_language = mutated_df_peers[!is.na(mutated_df_peers$Language_Group) & !is.na(mutated_df_peers$Type),]
mutated_peers_year = mutated_df_peers[!is.na(mutated_df_peers$Year_Group) & !is.na(mutated_df_peers$Type),]

peers_null <- lmer(value ~ Type + (1|ResponseId), data = mutated_peers_year)
peers_gender <- lmer(value ~ Gender_Group + Type + (1|ResponseId), data = mutated_peers_gender)
peers_gender_interaction <- lmer(value ~ Gender_Group * Type + (1|ResponseId), data = mutated_peers_gender)

peers_race <- lmer(value ~ Race_Group + Type + (1|ResponseId), data = mutated_peers_race)
peers_race_interaction <- lmer(value ~ Race_Group * Type + (1|ResponseId), data = mutated_peers_race)

peers_language <- lmer(value ~ Language_Group + Type + (1|ResponseId), data = mutated_peers_language)
peers_language_interaction <- lmer(value ~ Language_Group * Type + (1|ResponseId), data = mutated_peers_language)

peers_year <- lmer(value ~ Year_Group + Type + (1|ResponseId), data = mutated_peers_year)
peers_year_interaction <- lmer(value ~ Year_Group * Type + (1|ResponseId), data = mutated_peers_year)

# ===============Comfort Level of Disclosing to Reviewers==============

comfort_reviewers_df <- survey_df %>% dplyr::select(Q92_1numeric, Q92_2numeric, Q92_3numeric, Q92_4numeric, Q92_5numeric, Q92_6numeric, Gender_Group, Race_Group, Language_Group, Year_Group, ResponseId)

questions_to_domains_reviewers <- c('Q92_1numeric' = 'Information seeking',
                                    'Q92_2numeric' = 'Editing',
                                    'Q92_3numeric' = 'Direct Writing',
                                    'Q92_4numeric' = 'Ideation & Framing',
                                    'Q92_5numeric' = 'Data Generation',
                                    'Q92_6numeric' = 'Data Cleaning & Analysis')

mutated_df_reviewers <- comfort_reviewers_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_reviewers[Question]))

mutated_df_reviewers = mutated_df_reviewers[!is.na(mutated_df_reviewers$value),]
mutated_reviewers_gender = mutated_df_reviewers[!is.na(mutated_df_reviewers$Gender_Group) & !is.na(mutated_df_reviewers$Type),]
mutated_reviewers_race = mutated_df_reviewers[!is.na(mutated_df_reviewers$Race_Group) & !is.na(mutated_df_reviewers$Type),]
mutated_reviewers_language = mutated_df_reviewers[!is.na(mutated_df_reviewers$Language_Group) & !is.na(mutated_df_reviewers$Type),]
mutated_reviewers_year = mutated_df_reviewers[!is.na(mutated_df_reviewers$Year_Group) & !is.na(mutated_df_reviewers$Type),]

reviewers_null <- lmer(value ~ Type + (1|ResponseId), data = mutated_reviewers_year)
reviewers_gender <- lmer(value ~ Gender_Group + Type + (1|ResponseId), data = mutated_reviewers_gender)
reviewers_gender_interaction <- lmer(value ~ Gender_Group * Type + (1|ResponseId), data = mutated_reviewers_gender)

reviewers_race <- lmer(value ~ Race_Group + Type + (1|ResponseId), data = mutated_reviewers_race)
reviewers_race_interaction <- lmer(value ~ Race_Group * Type + (1|ResponseId), data = mutated_reviewers_race)

reviewers_language <- lmer(value ~ Language_Group + Type + (1|ResponseId), data = mutated_reviewers_language)
reviewers_language_interaction <- lmer(value ~ Language_Group * Type + (1|ResponseId), data = mutated_reviewers_language)

reviewers_year <- lmer(value ~ Year_Group + Type + (1|ResponseId), data = mutated_reviewers_year)
reviewers_year_interaction <- lmer(value ~ Year_Group * Type + (1|ResponseId), data = mutated_reviewers_year)

#=======================Analysis for Fields============================
# Note: A lot of the code below is repetitive to what above because field is in
# a different dataframe

df <- LLM_Survey_Fields
colnames(df)[colnames(df) == 'Field Category'] <- 'Field'

df$Field = as.factor(df$Field)
df$ResponseId = as.factor(df$ResponseId)

# Dependent Variables -- usage types
df$Q26f <- factor(df$Q26, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
df$Q78f <- factor(df$Q78, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
df$Q80f <- factor(df$Q80, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
df$Q82f <- factor(df$Q82, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
df$Q86f <- factor(df$Q86, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)
df$Q84f <- factor(df$Q84, levels = c("Never" , "Very Rarely", "Rarely", "Occasionally", "Frequently", "Very Frequently"), ordered = TRUE)


df$Q88_1f <- factor(df$Q88_1, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
df$Q88_2f <- factor(df$Q88_2, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
df$Q88_3f <- factor(df$Q88_3, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
df$Q88_4f <- factor(df$Q88_4, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
df$Q88_5f <- factor(df$Q88_5, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)
df$Q88_6f <- factor(df$Q88_6, levels = c('Not at all useful', 'Slightly useful', 'Moderately useful', 'Very useful', 'Extremely useful'), ordered = TRUE)

df$Q89_1f <- factor(df$Q89_1, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
df$Q89_2f <- factor(df$Q89_2, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
df$Q89_3f <- factor(df$Q89_3, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
df$Q89_4f <- factor(df$Q89_4, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
df$Q89_5f <- factor(df$Q89_5, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)
df$Q89_6f <- factor(df$Q89_6, levels = c('Not at all Risky', 'Slightly Risky', 'Moderately Risky', 'Very Risky', 'Extremely Risky'), ordered = TRUE)

df$Q90_1f <- factor(df$Q90_1, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
df$Q90_2f <- factor(df$Q90_2, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
df$Q90_3f <- factor(df$Q90_3, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
df$Q90_4f <- factor(df$Q90_4, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
df$Q90_5f <- factor(df$Q90_5, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)
df$Q90_6f <- factor(df$Q90_6, levels = c("Unacceptable", "Somewhat Unacceptable", "Neutral", "Somewhat Acceptable", "Acceptable"), ordered = TRUE)

df$Q91_1f <- factor(df$Q91_1, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q91_2f <- factor(df$Q91_2, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q91_3f <- factor(df$Q91_3, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q91_4f <- factor(df$Q91_4, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q91_5f <- factor(df$Q91_5, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q91_6f <- factor(df$Q91_6, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)

df$Q92_1f <- factor(df$Q92_1, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q92_2f <- factor(df$Q92_2, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q92_3f <- factor(df$Q92_3, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q92_4f <- factor(df$Q92_4, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q92_5f <- factor(df$Q92_5, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)
df$Q92_6f <- factor(df$Q92_6, levels = c("Uncomfortable", "Somewhat uncomfortable", "Neutral", "Somewhat comfortable", "Comfortable"), ordered = TRUE)


comfort_map <- c("Uncomfortable" = 1, "Somewhat uncomfortable" = 2, "Neutral" = 3, "Somewhat comfortable" = 4, "Comfortable" = 5)
frequency_map <- c("Never" = 1, "Very Rarely" = 2, "Rarely" = 3, "Occasionally" = 4, "Frequently" = 5, "Very Frequently" = 6)
risky_map <- c('Not at all Risky' = 1, 'Slightly Risky' = 2, 'Moderately Risky' = 3, 'Very Risky' = 4, 'Extremely Risky' = 5)
useful_map <- c('Not at all useful' = 1, 'Slightly useful' = 2, 'Moderately useful' = 3, 'Very useful' = 4, 'Extremely useful' = 5)
ethics_map <- c("Unacceptable" = 1, "Somewhat Unacceptable" = 2, "Neutral" = 3, "Somewhat Acceptable" = 4, "Acceptable" = 5)


# ===============Frequency of LLM Usage==============
frequency_df <- survey_df %>% dplyr::select(Q26f, Q78f, Q80f, Q82f, Q86f, Q84f, Field, ResponseId)

questions_to_domains_freq <- c('Q26f' = 'Information seeking',
                               'Q78f' = 'Editing',
                               'Q82f' = 'Ideation & Framing',
                               'Q80f' = 'Direct Writing',
                               'Q84f' = 'Data Cleaning & Analysis',
                               'Q86f' = 'Data Generation')

mutated_df_freq <- frequency_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_freq[Question]))  # Use the mapping to get the type for each question

mutated_df_freq$value_numeric <- frequency_map[mutated_df_freq$value]
mutated_fields = mutated_df_freq[!is.na(mutated_df_freq$Field) & !is.na(mutated_df_freq$Type),]
mutated_fields = filter(mutated_fields, Field != "Other")

freq_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_fields)
freq_field <- lmer(value_numeric ~ Field + Type + (1|ResponseId), data = mutated_fields)
freq_field_interaction <- lmer(value_numeric ~ Field * Type + (1|ResponseId), data = mutated_fields)

# ===============Risks of LLM Usage==============
risk_df <- survey_df %>% dplyr::select(Q89_1f, Q89_2f, Q89_3f, Q89_4f, Q89_5f, Q89_6f, Field, ResponseId)

questions_to_domains_risk <- c('Q89_1f' = 'Information seeking',
                               'Q89_2f' = 'Editing',
                               'Q89_3f' = 'Direct Writing',
                               'Q89_4f' = 'Ideation & Framing',
                               'Q89_5f' = 'Data Generation',
                               'Q89_6f' = 'Data Cleaning & Analysis')
mutated_df_risk <- risk_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_risk[Question]))

mutated_df_risk$value_numeric <- risky_map[mutated_df_risk$value]
mutated_fields_risk = mutated_df_risk[!is.na(mutated_df_risk$Field) & !is.na(mutated_df_risk$Type),]
mutated_fields_risk = filter(mutated_fields_risk, Field != "Other")

risk_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_fields_risk)
risk_field <- lmer(value_numeric ~ Field + Type + (1|ResponseId), data = mutated_fields_risk)
risk_field_interaction <- lmer(value_numeric ~ Field * Type + (1|ResponseId), data = mutated_fields_risk)

# ===============Benefits of LLM Usage==============
useful_df <- survey_df %>% dplyr::select(Q88_1f, Q88_2f, Q88_3f, Q88_4f, Q88_5f, Q88_6f, Field, ResponseId)
questions_to_domains_useful <- c('Q88_1f' = 'Information seeking',
                                 'Q88_2f' = 'Editing',
                                 'Q88_3f' = 'Direct Writing',
                                 'Q88_4f' = 'Ideation & Framing',
                                 'Q88_5f' = 'Data Generation',
                                 'Q88_6f' = 'Data Cleaning & Analysis')

mutated_df_useful <- useful_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_useful[Question]))

mutated_df_useful$value_numeric <- useful_map[mutated_df_useful$value]
mutated_fields_useful = mutated_df_useful[!is.na(mutated_df_useful$Field) & !is.na(mutated_df_useful$Type),]
mutated_fields_useful = filter(mutated_fields_useful, Field != "Other")

useful_null <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_fields_useful)
useful_field <- lmer(value_numeric ~ Field + Type + (1|ResponseId), data = mutated_fields_useful)
useful_field_interaction <- lmer(value_numeric ~ Field * Type + (1|ResponseId), data = mutated_fields_useful)

# ===============Ethics of LLM Usage==============
ethics_dff <- df %>% dplyr::select(Q90_1f, Q90_2f, Q90_3f, Q90_4f, Q90_5f, Q90_6f, Field, ResponseId)
questions_to_domains_ethics <- c('Q90_1f' = 'Information seeking',
                                 'Q90_2f' = 'Editing',
                                 'Q90_3f' = 'Direct Writing',
                                 'Q90_4f' = 'Ideation & Framing',
                                 'Q90_5f' = 'Data Generation',
                                 'Q90_6f' = 'Data Cleaning & Analysis')

mutated_dff_ethics <- ethics_dff %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_ethics[Question]),  # Use the mapping to get the type for each question
         Answers = value)

mutated_dff_ethics$value_numeric <- ethics_map[mutated_dff_ethics$value]
mutated_fields_ethics = mutated_dff_ethics[!is.na(mutated_dff_ethics$Field) & !is.na(mutated_dff_ethics$Type),]
mutated_fields_ethics = filter(mutated_fields_ethics, Field != "Other")


ethics_null_f <- lmer(value_numeric ~ Type + (1|ResponseId), data = mutated_fields_ethics)
ethics_field <- lmer(value_numeric ~ Field + Type + (1|ResponseId), data = mutated_fields_ethics)
ethics_field_interaction <- lmer(value_numeric ~ Field * Type + (1|ResponseId), data = mutated_fields_ethics)

df$Q91_1numeric <- comfort_map[df$Q91_1]
df$Q91_2numeric <- comfort_map[df$Q91_2]
df$Q91_3numeric <- comfort_map[df$Q91_3]
df$Q91_4numeric <- comfort_map[df$Q91_4]
df$Q91_5numeric <- comfort_map[df$Q91_5]
df$Q91_6numeric <- comfort_map[df$Q91_6]
df$Q92_1numeric <- comfort_map[df$Q92_1]
df$Q92_2numeric <- comfort_map[df$Q92_2]
df$Q92_3numeric <- comfort_map[df$Q92_3]
df$Q92_4numeric <- comfort_map[df$Q92_4]
df$Q92_5numeric <- comfort_map[df$Q92_5]
df$Q92_6numeric <- comfort_map[df$Q92_6]

# ===============Comfort Level of Disclosing to Peers ==============
peers_fields_df <- df %>% dplyr::select(Q91_1numeric, Q91_2numeric, Q91_3numeric, Q91_4numeric, Q91_5numeric, Q91_6numeric, Field, ResponseId)

mutated_fields_peers <- peers_fields_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_peers[Question]),  # Use the mapping to get the type for each question
         Answers = value)
mutated_fields_peers = mutated_fields_peers[!is.na(mutated_fields_peers$value),]
mutated_fields_peers = filter(mutated_fields_peers, Field != "Other")
mutated_peers_fields = mutated_fields_peers[!is.na(mutated_fields_peers$Field) & !is.na(mutated_fields_peers$Type),]

peers_fields_null <- lmer(value ~ Type + (1|ResponseId), data = mutated_peers_fields)
peers_fields <- lmer(value ~ Field + Type + (1|ResponseId), data = mutated_peers_fields)
peers_fields_interaction <- lmer(value ~ Field * Type + (1|ResponseId), data = mutated_peers_fields)

# ===============Comfort Level of Disclosing to Reviewers ==============
reviewers_fields_df <- df %>% dplyr::select(Q92_1numeric, Q92_2numeric, Q92_3numeric, Q92_4numeric, Q92_5numeric, Q92_6numeric, Field, ResponseId)

mutated_fields_reviewers <- reviewers_fields_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question") %>%
  mutate(Type = factor(questions_to_domains_reviewers[Question]),  # Use the mapping to get the type for each question
         Answers = value)
mutated_fields_reviewers = mutated_fields_reviewers[!is.na(mutated_fields_reviewers$value),]
mutated_fields_reviewers = filter(mutated_fields_reviewers, Field != "Other")
mutated_reviewers_fields = mutated_fields_reviewers[!is.na(mutated_fields_reviewers$Field) & !is.na(mutated_fields_reviewers$Type),]

reviewers_fields_null <- lmer(value ~ Type + (1|ResponseId), data = mutated_reviewers_fields)
reviewers_fields <- lmer(value ~ Field + Type + (1|ResponseId), data = mutated_reviewers_fields)
reviewers_fields_interaction <- lmer(value ~ Field * Type + (1|ResponseId), data = mutated_reviewers_fields)

#=======================Emmeans===========================
# swap out models and independent variables for each test
# example: model without interaction
(EMM1 <- emmeans(freq_race, ~ Race_Group))
pairs(EMM1)

# example: model with interaction
(EMM2 <- emmeans(useful_language_interaction, ~ Langauge_Group | Type))
pairs(EMM2)

#=======================Chi Squared Tests===========================

Gender_Race <- table(survey_df$Gender_Group, survey_df$Race_Group)
gender_race_test <- chisq.test(Gender_Race)

Gender_Year <- table(survey_df$Gender_Group, survey_df$Year_Group)
gender_year_test <- chisq.test(Gender_Year)

Gender_Publication <- table(survey_df$Gender_Group, survey_df$Publication_Group)
gender_publication_test <- chisq.test(Gender_Publication)

Gender_Language <- table(survey_df$Gender_Group, survey_df$Language_Group)
gender_lan_test <- chisq.test(Gender_Language)

Race_Year <- table(survey_df$Race_Group, survey_df$Year_Group)
race_year_test <- chisq.test(Race_Year)

Race_Publication <- table(survey_df$Race_Group, survey_df$Publication_Group)
race_publication_test <- chisq.test(Race_Publication)

Race_Language <- table(survey_df$Race_Group, survey_df$Language_Group)
race_lan_test <- chisq.test(Race_Language)

Year_Publication <- table(survey_df$Year_Group, survey_df$Publication_Group)
year_publication_test <- chisq.test(Year_Publication)

Year_Language <- table(survey_df$Year_Group, survey_df$Language_Group)
year_lan_test <- chisq.test(Year_Language)

Pub_Language <- table(survey_df$Publication_Group, survey_df$Language_Group)
pub_lan_test <- chisq.test(Pub_Language)

# Merge the two data frames based on the ResponseId column
merged_df <- merge(survey_df, LLM_Survey_Fields[, c("ResponseId", "Field Category")], by = "ResponseId", all.x = TRUE)
merged_df$Field[merged_df$`Field Category`== "Other"] <- NA


Gender_Field <- table(merged_df$Gender_Group, merged_df$Field)
gender_field_test <- chisq.test(Gender_Field)

Race_Field <- table(merged_df$Race_Group, merged_df$Field)
Race_field_test <- chisq.test(Race_Field)

Language_Field <- table(merged_df$Language_Group, merged_df$Field)
language_field_test <- chisq.test(Language_Field)

Year_Field <- table(merged_df$Year_Group, merged_df$Field)
year_field_test <- chisq.test(Year_Field)

Pub_Field <- table(merged_df$Publication_Group, merged_df$Field)
pub_field_test <- chisq.test(Pub_Field)

freq_risk <- table(overall_df$frequency_score, overall_df$risk_score)
freq_risk_test <- chisq.test(freq_risk)