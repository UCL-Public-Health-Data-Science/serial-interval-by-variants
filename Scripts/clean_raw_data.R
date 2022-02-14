raw_df$start_dt2 <- as.Date(raw_df$start_dt, format="%d%b%Y") #parse dates
raw_df$end_dt2 <- as.Date(raw_df$end_dt, format="%d%b%Y")

df <- raw_df %>%                                  #read raw data
  filter(swaboutcome=="positive") %>%             # filter for positive cases 
  mutate(nVar = as.character(nVar),               # recode the nVar 
         nVar = case_when(nVar == "" ~ "[0] Unknown",
                          nVar == "[0] Wild Type" ~ "[1] Wild Type",
                          nVar == "[1] Alpha" ~ "[2] Alpha",
                          nVar == "[2] Delta" ~ "[3] Delta",
                          nVar == "[3] Omicron" ~ "[4] Omicron")) %>% 
  group_by(household_id) %>% 
  arrange(start_dt2, .by_group=TRUE) %>% 
  mutate(diff_days= difftime(start_dt2, lag(start_dt2), units = "days"), 
         diff_days2 = ifelse(newID==lag(newID), NA, diff_days),
         case_type = ifelse(row_number()==1, "index_case", "secondary_case")) %>% 
  as.data.frame()




df <- df %>% 
  dplyr::select(illnessid,newID, household_id,start_dt2, 
                end_dt2, swabbed, swaboutcome,nVar,
                region, sex_bin, hh_age_on_entry, age3, case_type) #select variables of interest

df$illnessid<- as.character(df$illnessid)
df$newID<- as.character(df$newID)
df$household_id<- as.character(df$household_id)
df$swabbed<- as.character(df$swabbed)
df$swaboutcome<- as.character(df$swaboutcome)
df %<>% dplyr::arrange(start_dt2)




# 14 day FOLLOW-UP --------------------------------------------------------
#removing households where  index case occurs within 
#two weeks of survey data to allow for complete follow-up

last_update <- as.Date(last_update)  #extract date of last data update

#get the survey date (1st monday of from the last update date)
last_monday <- function(x) 7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin = "1970-01-01") 
date_limit <- last_monday(last_update) - 14 #2 weeks before our survey date

households_to_exclude <- df %>% 
  filter(case_type == "index_case") %>% #get all index cases
  filter(start_dt2 >= date_limit) %>%  # extract those who's symptom onset date is within 2 weeks of survey date
  .$household_id %>% unique()          # extract the household ids


#remove households where index case reports a symptom onset within 2 weeks of survey date
df <- df %>% 
  filter(! household_id %in% households_to_exclude) 

rm(last_monday)
warning("clean_raw_data.R ran successfully")
