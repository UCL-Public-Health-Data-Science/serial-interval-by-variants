
# TABLES ------------------------------------------------------------------
theme_gtsummary_journal(journal = "jama") 

# storing IDs in certains transmissions
IDs<- unique(c(certain_transmissions$infector_ID, certain_transmissions$infectee_ID))




# DEMOGRAPHICS ---------------------------------------------------
table_demographics <- df %>%
  filter((newID %in% unique(IDs))) %>% 
  distinct() %>% 
  select( age3, sex_bin, region, nVar, start_dt2) %>% 
  rename(Age = age3,
         Sex = sex_bin,
         Region = region,
         Variant = nVar,
         "Date of symptom onset" = start_dt2 )  %>% 
  mutate(Sex = ifelse(Sex == "", NA, Sex),
         Region = ifelse(Region =="", NA, Region)) %>% 
  tbl_summary(by = Variant,
              missing = "ifany",
              missing_text = "NA"
  ) %>% 
  add_overall() 


print(table_demographics)



# SERIAL INTERVAL BY VARIANTS -------------------------------------
ci_df <- data.frame()
for (i in unique(certain_transmissions$infector_nVar)) {
  temp_i <- subset(certain_transmissions, infector_nVar == i)
  ci <- DescTools::MeanCI(temp_i$serial_interval) %>% t() %>% as.data.frame()
  ci <-ci %>% mutate(nVar = i)
  ci_df <- rbind(ci_df, ci)
}


temp_table <- certain_transmissions %>% 
  group_by(infector_nVar) %>% 
  summarise(no_of_pairs = n(),
            mean = round(mean(serial_interval), digits = 2),
            median = round(median(serial_interval), digits = 2),
            sd = round(sd(serial_interval), digits = 2)) %>% 
  rename(nVar = infector_nVar)


table_variants <- merge( temp_table %>% arrange(nVar), 
                ci_df[-1] %>% arrange(nVar),
                by ="nVar", 
                all.x = TRUE) %>% 
  mutate(lwr.ci = round(lwr.ci, digits = 2),
         upr.ci = round(upr.ci, digits = 2)) %>% 
  rename(Variant = nVar)


print(table_variants)




# GAMMA DISTRIBUTIONS --------------------------------------------------------------


all_dist <- data.frame()
all_params <- data.frame()

for (var in unique(certain_transmissions$infector_nVar)) {
  temp_certain_transmissions <- subset(certain_transmissions, 
                                       infector_nVar == var)
  si <- temp_certain_transmissions$serial_interval %>% 
    epitrix::fit_disc_gamma()
  
  dist_temp<- si$distribution$d(0:14) %>% as.data.frame() %>% 
    rename("d" = ".") %>% 
    mutate(infector_nVar = var,
           si = row_number())
  all_dist<- rbind(all_dist, dist_temp)
  
  params_temp <- si$distribution$parameters %>% as.data.frame() %>% 
    mutate(infector_nVar = var)
  all_params <- rbind(all_params, params_temp) 
}

table_gamma_dist <- all_params %>% 
  arrange(infector_nVar) %>% 
  rename(Variant = infector_nVar) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 3))) %>% 
  relocate(Variant)

print(table_gamma_dist)


rm(i, ci, ci_df,dist_temp, params_temp, temp_certain_transmissions, temp_i, temp_table)
warning("tables.R ran successfully")
