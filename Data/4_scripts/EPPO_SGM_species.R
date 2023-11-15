# library ---------------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(dbplyr)
library(DBI)
library(tidyverse)

# check database ----------------------------------------------------------
dir("./Data/1_raw_data/EPO sqlite")
con <- dbConnect(drv = RSQLite::SQLite(), dbname = "./Data/1_raw_data/EPO sqlite/eppocodes.sqlite")
dbListTables(con)

# check content  --------------------------------------------------------------------------------------------------------------------------------
# DBI::dbListFields(con, "t_names")

eppo<- tbl(eppo_con, "t_codes") %>% 
  left_join(tbl(eppo_con, "t_names"),  by = "codeid") %>% 
  left_join(tbl(eppo_con, "t_countries"),  by = "isocountry") %>% 
  left_join(tbl(eppo_con, "t_datatypes"),  by = "dtcode")  %>% 
  left_join(tbl(eppo_con, "t_langs"),  by = "codelang") %>% 
  left_join(tbl(eppo_con, "t_authorities"),  by = "idauth") %>% 
  left_join(tbl(eppo_con, "t_links")) %>% 
  select(., !contains("date")) %>% 
  select(., -status.x, -status.y) %>% 
  mutate_all(na_if,"") %>% 
  collect()

# -----------------------------------------------------------------------------------------------------------------------------------------------
api_key_file_path <- file.path("C:/Users/dafl/Desktop/EPPO_API_KEY.txt")
EPPO_key <- readLines(api_key_file_path, warn = FALSE)
# -----------------------------------------------------------------------------------------------------------------------------------------------

my_list <- list() 

input <- eppo %>% distinct(eppocode)

for (i in input$eppocode) {
  
  # EPPO URL to pest distribution
  eppo_distr_url   <- paste0("https://gd.eppo.int/taxon/", i ,"/pathwayshosts")
  # http GET request
  eppo_request <- httr::GET(eppo_distr_url,
                            query = list(
                              apikey=EPPO_key,
                              details = "true"
                            ))
  # parse results
  table_content    <- httr::content(eppo_request, as = 'text')
  # get table and clean list
  tables           <- XML::readHTMLTable(table_content)
  tables           <- rlist::list.clean(tables, fun = is.null, recursive = FALSE)
  
  # distribution table
  my_list[[i]] <- tables  
  
}

raw_data <- plyr::ldply(my_list, data.frame) %>% as_tibble()
raw_data
names(raw_data) <- c("eppocode", "pathway", "host")  

# -------------------------------------------------------------------------

rio::export(raw_data, "./Data/2_clean_data/EPPO_DB_pathway.xlsx")

# END -------------------------------------------------------------------------------------------------------------------------------------------


