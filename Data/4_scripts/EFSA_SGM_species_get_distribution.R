
# bache read all EFSA files  --------------------------------------------------------------------------------------------------------------------

library(readxl)
library(tidyverse)

file_list <- list.files(path = "./Data/2_clean_data/",
                        pattern = "EFSA.*\\.xlsx$",
                        ignore.case=T)

data_list <- lapply(file_list, function(x) read_excel(file.path("./Data/2_clean_data/", x)))
combined_data <- do.call(rbind, data_list)

combined_eppo_codes <- combined_data %>% distinct(eppo_code)

# -----------------------------------------------------------------------------------------------------------------------------------------------
api_key_file_path <- file.path("C:/Users/dafl/Desktop/EPPO_API_KEY.txt")
EPPO_key <- readLines(api_key_file_path, warn = FALSE)

# function to get EPPO distribution -------------------------------------------------------------------------------------------------------------

input <- combined_eppo_codes$eppo_code

get_names_content <- function(eppo_code, eppo_token) {
  eppo.distr.url <- paste0("https://gd.eppo.int/taxon/", eppo_code, "/distribution")
  eppo.get.request <- httr::GET(eppo.distr.url, query = list(apikey = EPPO_key, details = "true"))
  table_content <- httr::content(eppo.get.request, as = 'text')
  tables <- XML::readHTMLTable(table_content)
  tables <- rlist::list.clean(tables, fun = is.null, recursive = FALSE)
  
  if (length(tables) == 0) {
    message(paste("No data found for EPPO code:", eppo_code))
    return(NULL)
  }
  
  distr_table <- tables$dttable
  distr_table <- distr_table %>% select(Continent, Country, State, Status)
  distr_table$EPPO_code <- eppo_code  # Add the EPPO code as a new column
  return(distr_table)
}


# Use lapply to process multiple inputs and store the output in a list
output_list <- lapply(input, get_names_content, eppo_token = EPPO_token)

# Filter out NULL elements (for EPPO codes with no data returned)
output_list <- output_list[!sapply(output_list, is.null)]

if (length(output_list) == 0) {
  stop("No data found for any of the EPPO codes.")
}

# Combine the list elements into a single data frame
output_df <- do.call(rbind, output_list) %>% as_tibble()

# Reset row names
rownames(output_df) <- NULL

output_df

# -----------------------------------------------------------------------------------------------------------------------------------------------

# new column with presence in europe 
output_df <- output_df %>% 
  mutate(Europe = if_else(Continent == "Europe", "Yes", "No"),
         Norway = if_else(Country == "Norway", "Yes", "No")) 

# get eppo host -----------------------------------------------------------------------------------------------------------------------------------------------

get_hosts_content <- function(eppo_code, eppo_token) {
  eppo.hosts.url <- paste0("https://data.eppo.int/api/rest/1.0/taxon/", eppo_code, "/hosts")
  eppo.get.request <- httr::GET(eppo.hosts.url, query = list(authtoken = EPPO_key))
  hosts_content <- jsonlite::fromJSON(httr::content(eppo.get.request, as = 'text'), flatten = TRUE)
}

unique_eppo_codes <- unique(output_df$EPPO_code)


output_list <- list()

for (i in unique_eppo_codes) {
  d <- get_hosts_content(i, eppo_token = EPPO_token)
  
  # Check if d is not empty or NULL
  dd <- plyr::ldply(d, data.frame) %>% mutate(ID = i)
  output_list[[i]] <- dd
}


dada <- plyr::ldply(output_list, data.frame)

dada <- dada %>% 
  select(ID, full_name, eppocode) %>% 
  rename(EPPO_code = ID) %>% 
  rename(host_code = eppocode) 


dada <- dada %>% pivot_wider(id_cols=EPPO_code, names_from=host_code, values_from=full_name)

data <- left_join(output_df, dada, by = "EPPO_code")


# -----------------------------------------------------------------------------------------------------------------------------------------------
# write data to folder
rio::export(dada, file = "./Data/2_clean_data/EFSA_supporting_information_Europe.csv")
# -----------------------------------------------------------------------------------------------------------------------------------------------
