### Load in packages ############################# 

library(tidyverse)

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
  "1.5.3", "11.b.1", "13.1.2",
  "1.5.4", "11.b.2", "13.1.3",
  "4.7.1", "12.8.1", "13.3.1"
)

### Read in and clean main dataset ############################# 
# From https://unstats.un.org/sdgs/iaeg-sdgs/tier-classification/
df <- readxl::read_xlsx("Input/Tier_Classification_of_SDG_Indicators_17_July_2020_web.xlsx", sheet = 3, skip = 1) %>%
  # Clean Indicator column so empty spaces (only have newline character and therefore have length 1)
  # Are treated as NA for later filter
  mutate(Indicator = as.character(Indicator),
         Indicator = case_when(
           str_length(Indicator) == 1 ~ NA_character_,
           TRUE ~ Indicator
         )) %>%
  # Keep just indicator rows (gets rid of empty rows that just have Goal info)
  # Get rid of Tier III indicators since as of April 2020, these no longer exist
  # and only exist crossed-out in the excel file, which R does not recognize
  # See here https://unstats.un.org/sdgs/iaeg-sdgs/tier-classification/
  filter(!is.na(Indicator), `Updated Tier Classification \r\n(by IAEG-SDG Members)` != "Tier III") %>%
  # Now have 247 observations. 231 indicators plus repeating indicators, see here
  # https://unstats.un.org/sdgs/indicators/indicators-list/
  # Select and rename appropriate indicators
  select(indicator_code = `UNSD Indicator Code*`, target = Target, indicator = Indicator,
         initial_tier = `Initial Proposed Tier (by Secretariat)`, cust_agency = `Possible Custodian Agency(ies)`,
         partner_agency = `Partner Agency(ies)`, updated_tier = `Updated Tier Classification \r\n(by IAEG-SDG Members)`,
         notes = `Notes \r\n(including timing of review and explanation for change in Tier)`) %>%
  # Extract goal each indicator belongs to
  mutate(goal = as.numeric(str_extract(indicator, pattern = "^[0-9]{1,2}(?=\\.)")),
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
         # Clean target column by replacing single character rows with NA
         target = as.character(target),
         target = case_when(
           str_length(target) == 1 ~ NA_character_,
           TRUE ~ target
         ),
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
         )) %>%
  # Carryforward target designations to match with indicators.
  fill(target, .direction = "down") %>%
  mutate(
    # Extract target number from target field to classify
    # doubles and filter for target easier
    target_num = str_extract(target, pattern = "^[0-9]{1,2}\\.([0-9]{1,2}|[a-z])"),
    # Create row number indicator that give each indicator its own number.
    # Can then be used to get duplicates out of the dataset using df below
    num_row = row_number()
  ) %>%
  select(indicator_code, goal, target_num, target, indicator_num, indicator, is_duplicate, initial_tier, updated_tier, cust_agency, partner_agency, notes, num_row)

### Export main dataset ############################# 
df %>%
  mutate(indicator_num = str_c(" ", indicator_num),
         target_num = str_c(" ", target_num)) %>%
  select(-num_row) %>%
  write_csv("Output/Tier classification 17 July clean.csv", na = "")

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
  # Reduce dataset to unique row numbers, will reduce from 247 to 231
  distinct(num_row, .keep_all = TRUE) %>%
  # Tabulate tier numbers. Correct distribution!
  count(updated_tier)

### Breakdown of Tier status of each goal ############################# 
# Frequency table solution from here https://stackoverflow.com/questions/34860535/how-to-use-dplyr-to-generate-a-frequency-table/34860724

df %>% 
  count(goal, updated_tier) %>% 
  group_by(goal) %>% 
  mutate(frequency = prop.table(n)) %>%
  ungroup() %>%
  select(goal, updated_tier, frequency) %>%
  pivot_wider(id_cols = "goal", names_from = "updated_tier", values_from = "frequency") %>%
  write_csv("Output/Tier Classification frequency 17 July.csv", na = "")
