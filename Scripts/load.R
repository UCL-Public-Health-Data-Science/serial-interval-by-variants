
# FILES -------------------------------------------------------------------

ellen_directory <- 
  "S:\\CoronaWatch\\Working\\Dashboard\\symptom profile\\Work\\_DATA_derived_realtime\\"

illness_episodes <- "symp-data04_v6_collapsed_no_PCR_repeats.csv" #name of file



# DATA INPUT --------------------------------------------------------------

ellen <- file_index(directory = ellen_directory) #fetch real time directory

last_update <- ellen %>% 
  filter(file_name == illness_episodes ) %>% #extract file of interest
  slice(which.max(date_modified)) %>% .$date_modified # get file latest update

cat(
  paste0("Input data last updated on: ", last_update),
  paste0(" = ", as.Date(Sys.Date()) - as.Date(last_update), 
         " days ago"),
  sep ="\n")            # print update info




raw_df <- ellen %>% 
  filter(file_name=="symp-data04_v6_collapsed_no_PCR_repeats.csv") %>%  
  slice(which.max(date_modified)) %>% 
  .$file_path %>% 
  read.csv()  # extract and read latest

write.csv(raw_df, 
          paste0("Input Data/raw_df_", 
                 format(last_update, "%Y-%m-%d"),
                 ".csv"), row.names = F)
cat(
  paste0("CSV file written in: Input Data directory "),
  sep ="\n")




raw_df <- read.csv( paste0("Input Data/raw_df_", 
                           format(last_update, "%Y-%m-%d"),
                           ".csv"))


rm(ellen, illness_episodes, ellen_directory)
warning("load.R ran successfully")
