possible_transmissions <- data.frame()
for(hid in unique(df$household_id)){
  temp_hh <- subset(df, household_id == hid)  # isolate household
  secondary_cases <- subset(temp_hh, case_type == "secondary_case") # extract secondary cases
  
  if(nrow(secondary_cases) > 0){
    for(i in 1:nrow(secondary_cases)){
      temp_sc <- secondary_cases[i,]  # get first person in 2ndary cases
      not_temp_sc <- subset(temp_hh, newID != temp_sc$newID) # get everyone else in the household for comparison
      temp_sc_start_date <- temp_sc$start_dt2 # get their start dates
      print(paste("Current ID: ", temp_sc$newID, " start date is: ",temp_sc_start_date,  sep=""))
      
      
      if(nrow(not_temp_sc) > 0){
        for(j in 1:nrow(not_temp_sc)){
          temp_not_sc <- not_temp_sc[j,] # all other individuals including the primary
          temp_not_sc_start <- temp_not_sc$start_dt2   # get start date of  individual j
          temp_not_sc_max_si <- temp_not_sc_start + max_si   # generate its upper-bound
          print(paste("     Now Comparing against",temp_not_sc$newID," start date is:",temp_not_sc_start, 
                      " maximum is", temp_not_sc_max_si ,sep = ":"))
          
          
          if(!is.na(temp_not_sc_start) & !is.na(temp_sc_start_date) & !is.na(temp_not_sc_max_si)){
            if(temp_not_sc_start <= temp_sc_start_date & temp_sc_start_date <= temp_not_sc_max_si){
              print(paste("          Found possible infector",temp_not_sc$newID,sep=":"))
              link_df <- data.frame(temp_sc$newID, temp_sc$start_dt2, temp_not_sc$newID, temp_not_sc$start_dt2)
              possible_transmissions <- rbind(possible_transmissions, link_df) # if individual j is within max_si days of individual i -> keep
            }
          }
        }
      }
    }
  }
}

rm(link_df, not_temp_sc, temp_hh, temp_sc, temp_not_sc, temp_not_sc_max_si,temp_not_sc_start,temp_sc_start_date,i,j,hid ,secondary_cases)

warning("household infector-infectee matching complete", "\n",
        "now cleaning data.....")

possible_transmissions <- possible_transmissions %>% 
  rename(infectee_ID = temp_sc.newID,
         infectee_start_date = temp_sc.start_dt2,
         infector_ID = temp_not_sc.newID,
         infector_start_date = temp_not_sc.start_dt2) #renaming columns


info <- df %>% 
  mutate(adult = ifelse(hh_age_on_entry>=16, "adult", "child")) %>% 
  select(illnessid,household_id, newID, 
         start_dt2, nVar,case_type, 
         sex_bin, adult, age3, hh_age_on_entry, region ) %>% 
  rename(sex = sex_bin)


#merge infector info
temp_infector <- merge(possible_transmissions, info, 
                       by.x=c("infector_ID", "infector_start_date"), 
                       by.y = c("newID", "start_dt2") ) %>%  
  rename(infector_type = case_type,
         infector_age = hh_age_on_entry,
         infector_ageG = age3,
         infector_adult = adult,
         infector_sex = sex,
         infector_illnessid = illnessid,
         infector_nVar = nVar) #adding infector info


#merge infectee info
temp_infectee <- merge(possible_transmissions, 
                       info, 
                       by.x=c("infectee_ID", "infectee_start_date"),
                       by.y = c("newID", "start_dt2") ) %>% 
  select(-case_type, -region) %>% 
  rename(infectee_age = hh_age_on_entry,
         infectee_ageG = age3,
         infectee_adult = adult,
         infectee_sex = sex,
         infectee_illnessid = illnessid,
         infectee_nVar = nVar) #adding infectee info


transmissions <- merge(temp_infector, temp_infectee, 
                       by = 
                         intersect(
                         colnames(temp_infector), 
                         colnames(temp_infectee)
                         )
                       ) %>% 
  group_by(household_id) %>% 
  arrange(infector_start_date, infectee_start_date) #complete data

rm(temp_infector, temp_infectee, info, possible_transmissions)

#"change" if Nvar is different between infector and infectee
transmissions <- transmissions %>% 
  mutate(nVar_change = ifelse(infector_nVar == infectee_nVar, 
                             "same nVar", 
                             "change nVar")
         ) 


.dup<-transmissions %>% 
  filter(duplicated(infectee_ID)) %>% 
  select(infectee_ID) %>% distinct() # store duplicated infectees

certain_transmissions <- transmissions %>% 
  filter(!(infectee_ID %in% .dup$infectee_ID)) # remove duplicated infectees

certain_transmissions$serial_interval <- difftime(certain_transmissions$infectee_start_date, 
                                                  certain_transmissions$infector_start_date, 
                                                  units = c("days"))

certain_transmissions$serial_interval<-as.numeric(certain_transmissions$serial_interval)

#add household size
.total_householders<- readr::read_csv("S:\\CoronaWatch\\Working\\data_cleaning_cleaned\\Baseline\\3_Output\\baseline_household_info.csv") %>% 
  select(household_id, no_of_householders)

certain_transmissions<- merge(certain_transmissions, 
                              .total_householders, by = "household_id", 
                              all.x = TRUE)

rm(transmissions)
warning("compute_pairs.R ran successfully")
