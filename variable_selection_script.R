########################################
#### What variables can we explore? ####
########################################

# Packages ----
library(tidyverse)

# Read in the data ----

load("Data/wvs_timeseries.rdata")
d <- data1
rm(data1)

# Script for selecting variables ----

# Reorder dataframe
d_ra <- d %>% 
  select(-c(version,doi)) %>% 
  select(S002VS, COUNTRY_ALPHA, everything()) 

# Unique names of countries
countries <- sort(unique(d_ra$COUNTRY_ALPHA))

# Create data structure
repository_data <- data.frame(matrix(data = NA_character_, 
                          nrow = 1003, 
                          ncol = 3 + length(countries)))
colnames(repository_data) <- c("variable_name", 
                               "variable_label", 
                               "max_waves", 
                               countries)

# Loop begins
for (i in 1:1003) {
  
var_name <- colnames(d_ra)[i+2]

# Record var name 
repository_data[i,1] <- var_name

# Print progress
print(paste0(
  "working on row: ", 
  i, 
  " ; variable name: ", 
  var_name
))

# Get label
repository_data[i,2] <- ifelse(!is.null(attr(d_ra[[var_name]], "label")),
                               attr(d_ra[[var_name]], "label"), 
                               NA_character_)

# How many countries we have and how many waves for each?
summed_df <- d_ra %>% 
  select(S002VS, COUNTRY_ALPHA, {{var_name}}) %>% 
  filter(get({{var_name}}) >= (0)) %>% 
  group_by(COUNTRY_ALPHA) %>% 
  summarise(n = n_distinct(S002VS))

# MAximum number of waves
repository_data[i,3] <- summed_df %>%  
  arrange(desc(n)) %>% 
  slice(1) %>% 
  pull(n)

# Waves per country
results <- rep(0, 108)
names(results) <- countries
results[summed_df$COUNTRY_ALPHA] <- summed_df$n
repository_data[i, 4:111] <- results

}

write_csv(repository_data, "Data/variables_per_country_wave.csv")