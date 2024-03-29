### Load in packages ############################# 

library(tidyverse)

# Insert your wd path based on cloning this repository
setwd("C:/Users/loren/Documents/GitHub/sdg_tier_classifications")

### Prepare supplementary dataframes ############################# 
# Create vector of indicators that are duplicates. From here
# https://unstats.un.org/sdgs/indicators/indicators-list/
duplicates <- c(
  "7.b.1", "12.a.1",
  "8.4.1", "12.2.1",
  "8.4.2", "12.2.2",
  "10.3.1", "16.b.1",
  "10.6.1", "16.8.1",
  "13.2.1", "13.b.1",
  "15.7.1", "15.c.1",
  "15.a.1", "15.b.1",
  "1.5.1", "11.5.1", "13.1.1",
  "1.5.2", "11.5.2",
  "1.5.3", "11.b.1", "13.1.2",
  "1.5.4", "11.b.2", "13.1.3",
  "4.7.1", "12.8.1", "13.3.1"
)

# Define version
version  <- "30 Nov 2022"

### Read in and clean main dataset ############################# 
# From https://unstats.un.org/sdgs/iaeg-sdgs/tier-classification/
# 30 Nov 2022 update we have to specify the box we want Excel to read because the Excel file
# has THE ENTIRE TABLE COPIED AGAIN TO THE RIGHT, which is picked up if we read in the file normally
# The current range function specifies that we are drawing a box with a specified point
# At cell I2 and taking everything to the left. See here https://readxl.tidyverse.org/reference/cell-specification.html
df <- readxl::read_xlsx(str_c("Input/Tier Classification of SDG Indicators_", version, "_web.xlsx"), sheet = 4, range = cellranger::cell_limits(c(2, NA), c(NA, 9))) %>%
  # Pre-clean select columns
  mutate(
    # Clean Indicator column so empty spaces (only have newline character and therefore have length 1)
    # are treated as NA for later filter
    Indicator = as.character(Indicator),
    Indicator = case_when(
      str_length(Indicator) == 1 ~ NA_character_,
      TRUE ~ Indicator
      ),
    # Extract Goal information into separate column
    goal_info = str_extract(Target, "^Goal [0-9]{1,2}.*"),
    # Clean target column by replacing single character rows with NA for easier filling
    Target = as.character(Target),
    Target = case_when(
      str_length(Target) == 1 ~ NA_character_,
      TRUE ~ Target
      )) %>%
  # Carryforward goal and target designations to match with indicators.
  fill(c(goal_info, Target), .direction = "down") %>%
  # Keep just indicator rows (gets rid of empty rows that just have Goal and extra info)
  # Confirm number here https://unstats.un.org/sdgs/iaeg-sdgs/tier-classification/
  filter(!is.na(Indicator)) %>%
  # Now have 248 observations. 231 indicators plus repeating indicators, see here
  # https://unstats.un.org/sdgs/indicators/indicators-list/
  # Select and rename appropriate indicators
  select(indicator_code = `UNSD Indicator Code^`, goal_info, target = Target, indicator = Indicator,
         initial_tier = `Initial Proposed Tier (by Secretariat)`, cust_agency = `Custodian Agency(ies)`,
         partner_agency = `Partner Agency(ies)`, updated_tier = `Tier Classification`,
         notes = `Notes\r\n(post-2020 comprehensive review round; explanation and timing of updates or changes)`) %>%
  # Clean and create columns
  mutate(
    # Create Goal number and Goal text columns
    goal_num = as.numeric(str_extract(goal_info, pattern = "[0-9]{1,2}")),
    goal_text = str_extract(goal_info, pattern = "(?<=\\. ).*"),
    # Take out footnote in description of Goal 17
    goal_text = str_remove(goal_text, "\\[a\\]"),
    # Clean initial_tier column by replacing single character rows with NA
    # And removing newline characters
    initial_tier = as.character(initial_tier),
    initial_tier = case_when(
      str_length(initial_tier) == 1 ~ NA_character_,
      TRUE ~ initial_tier
      ),
    initial_tier = str_replace(initial_tier, pattern = "\\n|\\s\\n", ""),
    # Clean cust_agency column by replacing single character rows with NA
    cust_agency = as.character(cust_agency),
    cust_agency = case_when(
      str_length(cust_agency) == 1 ~ NA_character_,
      TRUE ~ cust_agency
      ),
    cust_agency = str_trim(str_replace(cust_agency, "\\r?\\n|\\r", "")),
    # Clean partner_agency column by replacing single character rows with NA
    partner_agency = as.character(partner_agency),
    partner_agency = case_when(
      str_length(partner_agency) == 1 ~ NA_character_,
      TRUE ~ partner_agency
      ),
    partner_agency = str_trim(str_replace(partner_agency, "\\r?\\n|\\r", "")),
    # Clean updated_tier column by replacing single character rows with NA
    # And removing newline characters
    updated_tier = as.character(updated_tier),
    updated_tier = case_when(
      str_length(updated_tier) == 1 ~ NA_character_,
      TRUE ~ updated_tier
      ),
    updated_tier = str_replace(updated_tier, pattern = "\\n|\\s\\n", ""),
    # Extract indicator number from indicator field to classify
    # doubles and filter for indicators easier
    indicator_num = str_extract(indicator, pattern = "^[0-9]{1,2}\\.([0-9]{1,2}|[a-z])\\.[0-9]"),
    # Create binary variable for duplicates
    # Listing is from here https://unstats.un.org/sdgs/indicators/indicators-list/
    # As is arrangement of duplicate with what.
    # Can think about categorical variable signifying number of duplicates (2|3)
    is_duplicate = case_when(
      indicator_num %in% duplicates ~ 1,
      TRUE ~ 0
      ),
    # Extract target number from target field to classify
    # doubles and filter for target easier
    target_num = str_extract(target, pattern = "^[0-9]{1,2}\\.([0-9]{1,2}|[a-z])"),
    # Create row number indicator that give each indicator its own number.
    # Can then be used to get duplicates out of the dataset using df below
    num_row = row_number(),
    # Create binary for whether or not SDG indicators are gender-specific or not,
    # according to https://data.unwomen.org/publications/progress-sustainable-development-goals-gender-snapshot-2022
    unw_gender_specific = case_when(
      indicator_num %in% c("1.1.1", "1.2.1", "1.2.2", "1.3.1", "1.4.2", "2.2.3", "2.3.2",
                           "3.1.1", "3.1.2", "3.3.1", "3.7.1", "3.7.2", "3.8.1", "4.1.1",
                           "4.2.1", "4.2.2", "4.3.1", "4.5.1", "4.6.1", "4.7.1", "4.a.1",
                           "5.1.1", "5.2.1", "5.2.2", "5.3.1", "5.3.2", "5.4.1", "5.5.1",
                           "5.5.2", "5.6.1", "5.6.2", "5.a.1", "5.a.2", "5.b.1", "5.c.1",
                           "8.3.1", "8.5.1", "8.5.2", "8.7.1", "8.8.1", "8.8.2", "10.2.1",
                           "11.2.1", "11.7.1", "11.7.2", "13.3.1", "16.1.1", "16.1.2",
                           "16.2.2", "16.2.3", "16.7.1", "16.7.2") ~ "Yes",
      TRUE ~ "No"
    ),
    # Create binary for whether or not SDG indicators are gender-specific or not according to the The Inter-Agency and Expert Group on Gender Statistics (IAEG-GS),
    # according to November 2021 List of gender-relevant indicators here https://unstats.un.org/unsd/demographic-social/gender/#IAEG-GS
    iaeg_gender_specific = case_when(
      indicator_num %in% c("1.1.1", 	"1.2.1", 	"1.2.2", 	"1.3.1", 	"1.4.2", 	"1.5.1", 	"1.b.1", 	
                           "2.1.2", 	"2.2.1", 	"2.2.2", 	"2.2.3", 	"2.3.2", 	"3.1.1", 	"3.1.2", 	
                           "3.2.1", 	"3.2.2", 	"3.3.1", 	"3.4.1", 	"3.4.2", 	"3.5.1", 	"3.5.2", 	
                           "3.6.1", 	"3.7.1", 	"3.7.2", 	"3.8.1", 	"3.9.1", 	"3.9.2", 	"3.9.3", 	
                           "3.a.1", 	"3.b.1", 	"3.c.1", 	"4.1.1", 	"4.1.2", 	"4.2.1", 	"4.2.2", 	
                           "4.3.1", 	"4.4.1", 	"4.5.1", 	"4.6.1", 	"4.7.1", 	"4.a.1", 	"5.1.1", 	
                           "5.2.1", 	"5.2.2", 	"5.3.1", 	"5.3.2", 	"5.4.1", 	"5.5.1", 	"5.5.2", 	
                           "5.6.1", 	"5.6.2", 	"5.a.1", 	"5.a.2", 	"5.b.1", 	"5.c.1", 	"6.1.1", 	
                           "6.2.1", 	"7.1.2", 	"8.3.1", 	"8.5.1", 	"8.5.2", 	"8.6.1", 	"8.7.1", 	
                           "8.8.1", 	"8.8.2", 	"8.10.2", 	"9.5.2", 	"10.2.1", 	"10.3.1", 	"11.2.1", 	
                           "11.7.1", 	"11.7.2", 	"16.1.1", 	"16.1.2", 	"16.1.3", 	"16.1.4", 	"16.2.2", 	
                           "16.2.3", 	"16.3.1", 	"16.5.1", 	"16.7.1", 	"16.7.2", 	"16.9.1", 	"17.8.1", 	"17.18.1") ~ "Yes",
      TRUE ~ "No"
    ),
    # Create binaries for Bridging the Gap indicators, see Table A1 from Bridging the Gap methodology Report
    # https://opendatawatch.com/publications/bridging-the-gap-methodology-report/#_Toc46928098
    btg_afr = case_when(
      indicator_num %in% c("1.1.1", 	"1.2.1", 	"1.2.2", 	"1.3.1", 	"1.5.1", 	"2.1.1", 	"2.2.1", 	"2.2.2", 	
                           "3.1.1", 	"3.1.2", 	"3.2.1", 	"3.2.2", 	"3.3.1", 	"3.3.2", 	"3.3.3", 	"3.3.4", 	"3.3.5", 
                           "3.4.1", 	"3.4.2", 	"3.5.2", 	"3.6.1", 	"3.7.1", 	"3.7.2", 	"3.9.1", 	"3.9.2", 	
                           "3.9.3", 	"3.a.1", 	"4.1.1", 	"4.2.2", 	"4.3.1", 	"4.4.1", 	"4.6.1", 	"4.a.1", 	"4.c.1", 
                           "5.2.1", 	"5.2.2", 	"5.3.1", 	"5.3.2", 	"5.4.1", 	"5.5.1", 	"5.5.2", 	"5.6.1", 	
                           "5.a.1", 	"5.b.1", 	"6.1.1", 	"6.2.1", 	"8.10.2", 	"8.3.1", 	"8.5.1", 	"8.5.2", 	"8.6.1", 
                           "8.7.1", 	"8.8.1", 	"9.2.2", 	"10.1.1", 	"11.1.1", 	"11.2.1", 	"16.1.1", 	"16.1.3", 	
                           "16.1.4", 	"16.2.1", 	"16.2.2", 	"16.2.3", 	"16.3.1", 	"16.3.2", 	"16.5.1", 	"16.9.1", 	"17.8.1") ~ "Yes",
      TRUE ~ "No"
    ),
    btg_lac = case_when(
      indicator_num %in% c("1.1.1", 	"1.2.1", 	"1.2.2", 	"1.3.1", 	"1.4.1", 	
                           "1.4.2", 	"1.5.1", 	"2.1.1", 	"2.1.2", 	"2.2.1", 	
                           "2.2.2", 	"2.3.2", 	"3.1.1", 	"3.1.2", 	"3.2.1", 	
                           "3.2.2", 	"3.3.1", 	"3.3.2", 	"3.3.3", 	"3.3.4", 	"3.3.5", 
                           "3.4.1", 	"3.4.2", 	"3.5.2", 	"3.6.1", 	"3.7.1", 	
                           "3.7.2", 	"3.9.1", 	"3.9.2", 	"3.9.3", 	"3.a.1", 	
                           "3.b.1", 	"4.1.1", 	"4.2.1", 	"4.2.2", 	"4.3.1", 	
                           "4.4.1", 	"4.5.1", 	"4.6.1", 	"4.a.1", 	"4.c.1", 	"5.2.1", 
                           "5.2.2", 	"5.3.1", 	"5.3.2", 	"5.4.1", 	"5.5.1", 	
                           "5.5.2", 	"5.6.1", 	"5.a.1", 	"5.b.1", 	"6.1.1", 	
                           "6.2.1", 	"8.10.2", 	"8.3.1", 	"8.5.1", 	"8.5.2", 	
                           "8.6.1", 	"8.7.1", 	"8.8.1", 	"9.1.1", 	"9.2.2", 	"10.1.1", 
                           "10.2.1", 	"10.3.1", 	"11.1.1", 	"11.2.1", 	"11.7.1", 	
                           "16.1.1", 	"16.1.2", 	"16.1.3", 	"16.1.4", 	"16.2.1", 	
                           "16.2.2", 	"16.2.3", 	"16.3.1", 	"16.3.2", 	"16.5.1", 	
                           "16.6.2", 	"16.7.1", 	"16.7.2", 	"16.9.1", 	"16.10.1", 	"17.8.1") ~ "Yes",
      TRUE ~ "No"
    ),
    btg_eap = case_when(
      indicator_num %in% c("1.1.1", 	"1.2.1", 	"1.2.2", 	"1.3.1", 	"1.4.2", 	"1.5.1", 
                           "2.1.1", 	"2.1.2", 	"2.2.1", 	"2.2.2", 	"2.2.3", 	
                           "2.3.2", 	"3.1.1", 	"3.1.2", 	"3.2.1", 	"3.2.2", 	"3.3.1", 
                           "3.3.2", 	"3.3.3", 	"3.3.4", 	"3.3.5", 	"3.4.1", 	
                           "3.4.2", 	"3.5.2", 	"3.6.1", 	"3.7.1", 	"3.7.2", 	"3.9.1", 
                           "3.9.2", 	"3.9.3", 	"3.a.1", 	"3.b.1", 	"4.1.1", 	
                           "4.1.2", 	"4.2.1", 	"4.2.2", 	"4.3.1", 	"4.4.1", 	"4.5.1", 
                           "4.6.1", 	"4.a.1", 	"4.c.1", 	"5.2.1", 	"5.2.2", 	"5.3.1", 
                           "5.3.2", 	"5.4.1", 	"5.5.1", 	"5.5.2", 	"5.6.1", 	"5.a.1", 
                           "5.b.1", 	"6.1.1", 	"6.2.1", 	"7.1.2", 	"8.10.2", 	"8.3.1", 
                           "8.5.1", 	"8.5.2", 	"8.6.1", 	"8.7.1", 	"8.8.1", 	"9.1.1", 
                           "9.2.2", 	"9.5.2", 	"10.1.1", 	"10.2.1", 	"10.3.1", 	"10.7.3", 
                           "10.7.4", 	"11.1.1", 	"11.2.1", 	"11.7.1", 	"11.7.2", 	"16.1.1", 
                           "16.1.2", 	"16.1.3", 	"16.1.4", 	"16.2.1", 	"16.2.2", 	"16.2.3", 
                           "16.3.1", 	"16.3.2", 	"16.3.3", 	"16.5.1", 	"16.6.2", 	"16.7.1", 
                           "16.7.2", 	"16.9.1", 	"16.10.1", 	"17.8.1") ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  select(indicator_code, goal_num, goal_text, target_num, target, indicator_num, indicator, is_duplicate, 
         initial_tier, updated_tier, cust_agency, partner_agency, notes, unw_gender_specific, iaeg_gender_specific, btg_afr, btg_lac, btg_eap, num_row)

### Export main dataset ############################# 
df %>%
  mutate(indicator_num = str_c(" ", indicator_num),
         target_num = str_c(" ", target_num)) %>%
  select(-num_row) %>%
  write_csv(str_c("Output/Tier classification", version, "clean.csv", sep = " "), na = "")

### Check official Tier distribution ############################# 
# Computing the current distribution of indicators  
# September 26 distribution
# 104 Tier I indicators, 89 Tier II indicators and 33 Tier III indicators. 
# In addition to these, there are 6 indicators that have multiple tiers

# November 20 distribution
# 116 Tier I indicators, 84 Tier II indicators and 27 Tier III indicators.
# In addition to these, there are 5 indicators that have multiple tiers

# December 11 2019 distribution
# 116 Tier I indicators, 92 Tier II indicators and 20 Tier III indicators. 
# In addition to these, there are 4 indicators that have multiple tiers

# 17 April 2020 distribution
# 115 Tier I indicators, 95 Tier II indicators and 
# 2 indicators that have multiple tiers (different components of the 
# indicator are classified into different tiers). There are 19 
# indicators with tiering pending a data availability review.

# 17 July 2020 distribution
# 123 Tier I indicators, 106 Tier II indicators and 
# 2 indicators that have multiple tiers (different components of the 
# indicator are classified into different tiers).

# 21 Mar 2021 distribution
# 130 Tier indicators, 97 Tier II indicators and
# 4 indicators that have multiple tiers (different components of the
# indicators are classified into different tiers).

# 4 Feb 2022 distribution
# 136 Tier indicators, 91 Tier II indicators and
# 4 indicators that have multiple tiers (different components of the
# indicators are classified into different tiers).

# 6 Apr 2022 distribution
# No official distribution
# 11.5.3 indicator is new, split out of existing indicator 11.5.2, but without
# Tier assignment. Current Tier distribution is the same as 4 Feb distribution otherwise
# 136 Tier I indicators, 90 Tier II indicators (maybe 91 if 11.5.3 is also Tier II as its original
# 11.5.2 indicator was), 4 indicators that have multiple tiers

# 30 Nov 2022 distribution from official site to check against below
# As of 30 November 2022: The updated tier classification contains 148 Tier I indicators, 
# 77 Tier II indicators and 6 indicators that have multiple tiers (different components of 
# the indicator are classified into different tiers). 

# This df will give every duplicated indicator group the same row number
dup_nums <- df %>%
  # Keep only duplicated indicators
  filter(indicator_num %in% duplicates) %>%
  # Keep only row number and indicator number
  select(num_row, indicator_num) %>%
  # Reshape wide
  pivot_wider(names_from = "indicator_num", values_from = num_row) %>%
  # Set appropriate indicator row numbers equal to each other
  mutate(`12.a.1` = `7.b.1`, `12.2.1` = `8.4.1`, `12.2.2` = `8.4.2`,
         `16.b.1` = `10.3.1`, `16.8.1` = `10.6.1`, `13.b.1` = `13.2.1`,
         `15.c.1` = `15.7.1`, `15.b.1` = `15.a.1`,
         `11.5.1` = `1.5.1`, `13.1.1` = `1.5.1`,
         `11.5.2` = `1.5.2`,
         `11.b.1` = `1.5.3`, `13.1.2` = `1.5.3`,
         `11.b.2` = `1.5.4`, `13.1.3` = `1.5.4`,
         `13.3.1` = `4.7.1`, `12.8.1` = `4.7.1`) %>%
  # Reshape back to long
  pivot_longer(`1.5.1`:`16.b.1`, names_to = "indicator_num", values_to = "dup_group")

### Check prepared df against official Tier distribution ############################# 
df %>%
  # Merge in group/row numbers of duplicate indicators
  left_join(dup_nums) %>%
  # Replace existing row numbers with group numbers for duplicate indicators
  mutate(num_row = case_when(
    indicator_num %in% duplicates ~ dup_group,
    TRUE ~ num_row
  )) %>%
  # Reduce dataset to unique row numbers, will reduce from 248 to 231
  distinct(num_row, .keep_all = TRUE) %>%
  # Tabulate tier numbers. Correct distribution!
  count(updated_tier)

### Breakdown of Tier status of each goal ############################# 
# Frequency table solution from here https://stackoverflow.com/questions/34860535/how-to-use-dplyr-to-generate-a-frequency-table/34860724

(df %>% 
  count(goal_num, goal_text, updated_tier) %>% 
  group_by(goal_num, goal_text) %>% 
  mutate(frequency = prop.table(n)) %>%
  ungroup() %>%
  select(goal_num, goal_text, updated_tier, frequency) %>%
  pivot_wider(id_cols = c("goal_num", "goal_text"), names_from = "updated_tier", values_from = "frequency") %>%
  write_csv(str_c("Output/Tier Classification frequency ", version, ".csv"), na = ""))

### List all custodian agencies #####
(df  %>% # Merge in group/row numbers of duplicate indicators
  left_join(dup_nums) %>%
  # Replace existing row numbers with group numbers for duplicate indicators
  mutate(num_row = case_when(
    indicator_num %in% duplicates ~ dup_group,
    TRUE ~ num_row
  )) %>%
  # Reduce dataset to unique row numbers, will reduce from 248 to 231
  distinct(num_row, .keep_all = TRUE) %>% 
  # Create list column based on custodian agencies by indicator being separated by comma
  mutate(new_cust = str_split(cust_agency, pattern = ",")) %>% 
  # Keep only new column
  select(new_cust) %>% 
  # Create single column out of all values in list column
  unnest_longer(new_cust) %>% 
  # Clean column of custodian names
  mutate(new_cust = str_trim(new_cust)) %>% 
  # Tabulate number of indicators per custodian agency
  count(new_cust, name = "num_indicators") %>% 
  # Export (Need additional cleaning by hand)
  write_csv(str_c("Output/List of custodian agencies ", version, ".csv")))
