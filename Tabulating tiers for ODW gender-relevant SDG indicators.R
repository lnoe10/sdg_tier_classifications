### Tabulate the tier distribution for gender-relevant indicators #####

### Load supplementary datasets #####
# Create dataframe df from "IAEG-SDG Tier Classification tally.R" file
# That will also load all the directories and packages you need for this

### Load gender-relevant indicators #####
# This loads all 84 ODW SDG -relevant indicators that are proposed by
# UN Women and added to by ODW (subset AGI - additional gender indicators). 
# These 84 were used for the BtG-LAC exercise
odw_gender <- read_csv("ODW gender-relevant SDG indicators.csv") %>%
  # Remove white space before and after indicator code to enable merging
  mutate(indicator_code = str_trim(indicator_code)) %>%
  # Rename variable to proper name for merging
  rename(indicator_num = indicator_code)

### Merge datasets #####
# Merge ODW indicators with list of 247 indicators (including duplicates)
update_tier <- df %>%
  inner_join(odw_gender)

### Analyze tier distribution across gender-relevant indicators
# Tabulate updated tiers for the 84 indicators
update_tier %>%
  count(updated_tier)
# Tabulate initial tiers for the 84 indicators
update_tier %>%
  count(initial_tier)
