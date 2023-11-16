# script to procces the data from the EFSA SGM species
library(readxl)
library(tidyverse)
library(reshape2)
library(httr)
library(XML)
library(rlist)
library(jsonlite)

# EPPO API Key ----------------------------------------------------------------------------------------------------------------------------------

api_key_file_path <- file.path("C:/Users/dafl/Desktop/EPPO_API_KEY.txt")
EPPO_token <- readLines(api_key_file_path, warn = FALSE)

# Function to call EPPO -------------------------------------------------------------------------------------------------------------------------

# Define a function to get the EPPO code for a single pest name
get_eppo_code <- function(pest_name) {
  if (is.na(pest_name)) {
    return(NA)
  } else {
    path.eppo.code <- "https://data.eppo.int/api/rest/1.0/tools/names2codes"
    response <- httr::POST(
      path.eppo.code,
      body = list(authtoken = EPPO_token, intext = pest_name),
      encode = "form"  # Set the encoding method
    )
    
    # Check for successful response
    if (httr::http_status(response)$category == "Success") {
      eppo_code <- strsplit(httr::content(response)[[1]], ";")[[1]][2]
      return(eppo_code)
    } else {
      return("****NOT FOUND****")
    }
  }
}


# bacteria --------------------------------------------------------------------------------------------------------------------------------------

bacteria <- read_excel("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/final results inv II bacteria.xlsx", sheet=3,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0, .name_repair = "unique")

long_bacteria <- bacteria %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

long_bacteria <- tibble::rowid_to_column(long_bacteria, "ID")

names(long_bacteria) <- c("ID", "Category", "species", "value")

# clean the species column
long_bacteria$species <- str_remove_all(long_bacteria$species, "[\r\n]")
long_bacteria$species <- gsub("\\(.*?\\)", "", long_bacteria$species)
long_bacteria$species  <- gsub("\\d+", "", long_bacteria$species) 
long_bacteria$species  <- gsub("^\\s+|\\s+$", "", long_bacteria$species )
long_bacteria$species <- gsub("\\[.*?\\]", "", long_bacteria$species)

# new column with interaction classification (outdated)
long_bacteria <- long_bacteria %>% mutate(new_value = case_when(
    value == "0" ~ "No information",
    value == "1" ~ "Major",
    value == "2" ~ "Minor",
    value == "3" ~ "Incidental",
    value == "4" ~ "wild/weed",
    value == "5" ~ "artificial",
    value == "6" ~ "unclassified",
    value == "7" ~ "alternate",
    value == "-1" ~ "Uncertain",
    TRUE ~ "Other" # for values that do not match any of the above
  ))

pest_names <- long_bacteria$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
bacteria <- left_join(long_bacteria, output_df, relationship = "many-to-many")

# find unique across all columns 
bacteria <- bacteria %>% distinct(across(everything()))

# write data to folder
rio::export(bacteria, "./Data/2_clean_data/EFSA_supporting_information_bacteria.xlsx")

# "final results inv II fungi.xlsx"    ----------------------------------------------------------------------------------------------------------

fungi <- read_excel("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/final results inv II fungi.xlsx", sheet=5,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0)

long_fungi <- fungi %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

long_fungi <- tibble::rowid_to_column(long_fungi, "ID")

names(long_fungi) <- c("ID", "Category", "species", "value")

# clean the species column
long_fungi$species <- str_remove_all(long_fungi$species, "[\r\n]")
long_fungi$species <- gsub("\\(.*?\\)", "", long_fungi$species)
long_fungi$species  <- gsub("\\d+", "", long_fungi$species) 
long_fungi$species  <- gsub("^\\s+|\\s+$", "", long_fungi$species )
long_fungi$species <- gsub("\\[.*?\\]", "", long_fungi$species)


# new column with interaction classification (outdated)
long_fungi <- long_fungi %>% mutate(new_value = case_when(
  value == "0" ~ "No information",
  value == "1" ~ "Major",
  value == "2" ~ "Minor",
  value == "3" ~ "Incidental",
  value == "4" ~ "wild/weed",
  value == "5" ~ "artificial",
  value == "6" ~ "unclassified",
  value == "7" ~ "alternate",
  value == "-1" ~ "Uncertain",
  TRUE ~ "Other" # for values that do not match any of the above
))

pest_names <- long_fungi$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
fungi <- left_join(long_fungi, output_df, relationship = "many-to-many")

# find unique across all columns 
fungi <- fungi %>% distinct(across(everything()))

fungi %>% filter(!value == "0")
fungi %>% filter(!value == "0") %>% distinct(species)

# write data to folder
rio::export(fungi, "./Data/2_clean_data/EFSA_supporting_information_fungi.xlsx")

# "final results inv II insects.xlsx"   ---------------------------------------------------------------------------------------------------------

insects <- read_xlsx("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/final results inv II insects.xlsx", sheet=5 ,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0, .name_repair = "unique")

long_insects <- insects %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

long_insects <- tibble::rowid_to_column(long_insects, "ID")

names(long_insects) <- c("ID", "Category", "species", "value")

long_insects <- long_insects %>%
  mutate(text_column = as.character(species)) %>%
  separate_rows(text_column, sep = " OR ") %>% 
  select(ID, Category, text_column, value) %>% 
  rename(species=text_column)

# clean the species column
long_insects$species <- str_remove_all(long_insects$species, "[\r\n]")
long_insects$species <- gsub("\\(.*?\\)", "", long_insects$species)
long_insects$species  <- gsub("\\d+", "", long_insects$species) 
long_insects$species  <- gsub("^\\s+|\\s+$", "", long_insects$species )
long_insects$species <- gsub("\\[.*?\\]", "", long_insects$species)


# new column with interaction classification (outdated)
long_insects <- long_insects %>% mutate(new_value = case_when(
  value == "0" ~ "No information",
  value == "1" ~ "Major",
  value == "2" ~ "Minor",
  value == "3" ~ "Incidental",
  value == "4" ~ "wild/weed",
  value == "5" ~ "artificial",
  value == "6" ~ "unclassified",
  value == "7" ~ "alternate",
  value == "-1" ~ "Uncertain",
  TRUE ~ "Other" # for values that do not match any of the above
))

pest_names <- long_insects$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
insects <- left_join(long_insects, output_df, relationship = "many-to-many")

# find unique across all columns 
insects <- insects %>% distinct(across(everything()))

insects %>% filter(!value == "0")
insects %>% filter(!value == "0") %>% distinct(species)

# write data to folder
rio::export(insects, "./Data/2_clean_data/EFSA_supporting_information_insects.xlsx")

# "final results inv II nematodes.xlsx"   -------------------------------------------------------------------------------------------------------

nematodes <- read_excel("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/final results inv II nematodes.xlsx", sheet=5,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0)

longe_nematodes <- nematodes %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

longe_nematodes <- tibble::rowid_to_column(longe_nematodes, "ID")

names(longe_nematodes) <- c("ID", "Category", "species", "value")

longe_nematodes <- longe_nematodes %>%
  mutate(text_column = as.character(species)) %>%
  separate_rows(text_column, sep = " OR ") %>% 
  select(ID, Category, text_column, value) %>% 
  rename(species=text_column)

# clean the species column
longe_nematodes$species <- str_remove_all(longe_nematodes$species, "[\r\n]")
longe_nematodes$species <- gsub("\\(.*?\\)", "", longe_nematodes$species)
longe_nematodes$species  <- gsub("\\d+", "", longe_nematodes$species) 
longe_nematodes$species  <- gsub("^\\s+|\\s+$", "", longe_nematodes$species )
longe_nematodes$species <- gsub("\\[.*?\\]", "", longe_nematodes$species)


# new column with interaction classification (outdated)
longe_nematodes <- longe_nematodes %>% mutate(new_value = case_when(
  value == "0" ~ "No information",
  value == "1" ~ "Major",
  value == "2" ~ "Minor",
  value == "3" ~ "Incidental",
  value == "4" ~ "wild/weed",
  value == "5" ~ "artificial",
  value == "6" ~ "unclassified",
  value == "7" ~ "alternate",
  value == "-1" ~ "Uncertain",
  TRUE ~ "Other" # for values that do not match any of the above
))

pest_names <- longe_nematodes$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
nematodes <- left_join(longe_nematodes, output_df, relationship = "many-to-many")

# find unique across all columns 
nematodes <- nematodes %>% distinct(across(everything()))

nematodes %>% filter(!value == "0")

# write data to folder
rio::export(nematodes, "./Data/2_clean_data/EFSA_supporting_information_nematodes.xlsx")

# "final results inv II phytoplasmas.xlsx"     --------------------------------------------------------------------------------------------------

phytoplasmas <- read_excel("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/final results inv II phytoplasmas.xlsx", sheet=5,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0)

longe_phytoplasmas <- phytoplasmas %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

longe_phytoplasmas <- tibble::rowid_to_column(longe_phytoplasmas, "ID")

names(longe_phytoplasmas) <- c("ID", "Category", "species", "value")

longe_phytoplasmas <- longe_phytoplasmas %>%
  mutate(text_column = as.character(species)) %>%
  separate_rows(text_column, sep = " OR ") %>% 
  select(ID, Category, text_column, value) %>% 
  rename(species=text_column)

# clean the species column
longe_phytoplasmas$species <- str_remove_all(longe_phytoplasmas$species, "[\r\n]")
longe_phytoplasmas$species <- gsub("\\(.*?\\)", "", longe_phytoplasmas$species)
longe_phytoplasmas$species  <- gsub("\\d+", "", longe_phytoplasmas$species) 
longe_phytoplasmas$species  <- gsub("^\\s+|\\s+$", "", longe_phytoplasmas$species )
longe_phytoplasmas$species <- gsub("\\[.*?\\]", "", longe_phytoplasmas$species)


# new column with interaction classification (outdated)
longe_phytoplasmas <- longe_phytoplasmas %>% mutate(new_value = case_when(
  value == "0" ~ "No information",
  value == "1" ~ "Major",
  value == "2" ~ "Minor",
  value == "3" ~ "Incidental",
  value == "4" ~ "wild/weed",
  value == "5" ~ "artificial",
  value == "6" ~ "unclassified",
  value == "7" ~ "alternate",
  value == "-1" ~ "Uncertain",
  TRUE ~ "Other" # for values that do not match any of the above
))

pest_names <- longe_phytoplasmas$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
phytoplasmas <- left_join(longe_phytoplasmas, output_df, relationship = "many-to-many")

# find unique across all columns 
phytoplasmas <- phytoplasmas %>% distinct(across(everything()))

phytoplasmas %>% filter(!value == "0")

# write data to folder
rio::export(phytoplasmas, "./Data/2_clean_data/EFSA_supporting_information_phytoplasmas.xlsx")


# final results inv II viruses.xlsx -------------------------------------------------------------------------------------------------------------

viruses <- read_excel("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/Final results inv II viruses.xlsx", sheet=5,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0)

longe_viruses <- viruses %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

longe_viruses <- tibble::rowid_to_column(longe_viruses, "ID")

names(longe_viruses) <- c("ID", "Category", "species", "value")

longe_viruses <- longe_viruses %>%
  mutate(text_column = as.character(species)) %>%
  separate_rows(text_column, sep = " OR ") %>% 
  select(ID, Category, text_column, value) %>% 
  rename(species=text_column)

# clean the species column
longe_viruses$species <- str_remove_all(longe_viruses$species, "[\r\n]")
longe_viruses$species <- gsub("\\(.*?\\)", "", longe_viruses$species)
longe_viruses$species  <- gsub("\\d+", "", longe_viruses$species) 
longe_viruses$species  <- gsub("^\\s+|\\s+$", "", longe_viruses$species )
longe_viruses$species <- gsub("\\[.*?\\]", "", longe_viruses$species)


# new column with interaction classification (outdated)
longe_viruses <- longe_viruses %>% mutate(new_value = case_when(
  value == "0" ~ "No information",
  value == "1" ~ "Major",
  value == "2" ~ "Minor",
  value == "3" ~ "Incidental",
  value == "4" ~ "wild/weed",
  value == "5" ~ "artificial",
  value == "6" ~ "unclassified",
  value == "7" ~ "alternate",
  value == "-1" ~ "Uncertain",
  TRUE ~ "Other" # for values that do not match any of the above
))

pest_names <- longe_viruses$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
viruses <- left_join(longe_viruses, output_df, relationship = "many-to-many")

# find unique across all columns 
viruses <- viruses %>% distinct(across(everything()))

viruses %>% filter(!value == "0")

# write data to folder
rio::export(viruses, "./Data/2_clean_data/EFSA_supporting_information_viruses.xlsx")

# "final results inv II weeds.xlsx"    ----------------------------------------------------------------------------------------------------------

weeds <- read_excel("./Data/1_raw_data/EFSA_Supporting_Publications_2015_ Bremmer/834eax1-sup-1/final results inv II weeds.xlsx", sheet=5,  col_names = T, na = "NA", trim_ws = TRUE, skip = 0)

longe_weeds <- weeds %>%
  mutate(across(-...1, as.character)) %>%
  pivot_longer(
    cols = -...1, # Selects all columns except the first for transformation
    names_to = "Category", # Name of the new column for categories
    values_to = "Value" # Name of the new column for values
  ) %>%
  filter(!is.na(Value)) # Optional: Remove rows where Value is NA

longe_weeds <- tibble::rowid_to_column(longe_weeds, "ID")

names(longe_weeds) <- c("ID", "Category", "species", "value")

longe_weeds <- longe_weeds %>%
  mutate(text_column = as.character(species)) %>%
  separate_rows(text_column, sep = " OR ") %>% 
  select(ID, Category, text_column, value) %>% 
  rename(species=text_column)

# clean the species column
longe_weeds$species <- str_remove_all(longe_weeds$species, "[\r\n]")
longe_weeds$species <- gsub("\\(.*?\\)", "", longe_weeds$species)
longe_weeds$species  <- gsub("\\d+", "", longe_weeds$species) 
longe_weeds$species  <- gsub("^\\s+|\\s+$", "", longe_weeds$species )
longe_weeds$species <- gsub("\\[.*?\\]", "", longe_weeds$species)

# new column with interaction classification (outdated)
longe_weeds <- longe_weeds %>% mutate(new_value = case_when(
  value == "0" ~ "No information",
  value == "1" ~ "Major",
  value == "2" ~ "Minor",
  value == "3" ~ "Incidental",
  value == "4" ~ "wild/weed",
  value == "5" ~ "artificial",
  value == "6" ~ "unclassified",
  value == "7" ~ "alternate",
  value == "-1" ~ "Uncertain",
  TRUE ~ "Other" # for values that do not match any of the above
))

pest_names <- longe_weeds$species

# Use lapply to get EPPO codes for multiple species
eppo_codes <- lapply(pest_names, get_eppo_code)

# Combine the pest names and their corresponding EPPO codes into a data frame
output_df <- data.frame(species = pest_names, eppo_code = unlist(eppo_codes))

# Replace the "****NOT FOUND****" with NA
output_df$eppo_code[output_df$eppo_code == "****NOT FOUND*****"] <- NA

# bind all relationships
weeds <- left_join(longe_weeds, output_df, relationship = "many-to-many")

# find unique across all columns 
weeds <- weeds %>% distinct(across(everything()))

weeds %>% filter(!value == "0")
weeds %>% filter(!value == "0") %>% distinct(species)

# write data to folder
rio::export(weeds, "./Data/2_clean_data/EFSA_supporting_information_weeds.xlsx")

# bind ------------------------------------------------------------------------------------------------------------------------------------------

input_all  <- bind_rows(insects, weeds, viruses, phytoplasmas, nematodes, bacteria, fungi)

as_tibble(input_all) %>% distinct(species)
# -----------------------------------------------------------------------------------------------------------------------------------------------

splist <- input_all$species %>% 
  unique() %>% 
  taxize::get_gbifid_(method = "backbone") %>% 
  purrr::imap(~ .x %>% mutate(input = .y)) %>% 
  bind_rows()

names(splist)

data <- splist %>%  
  select(input, scientificname, rank, status, order, phylum, kingdom) %>% 
  as_tibble()

data %>% group_split(kingdom)

# -----------------------------------------------------------------------------------------------------------------------------------------------
getwd()
rio::export(splist, "./Data/2_processed_data/sgm_species_efsa_2015.xlsx")
