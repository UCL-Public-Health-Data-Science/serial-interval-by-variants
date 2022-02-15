#FROM RAW ILLNESS EPISODES DATA
cat("how many illnesses?",
    length(unique(raw_df$illnessid)),
    sep = "\n")

cat("how many individuals?",
    length(unique(raw_df$newID)),
    sep = "\n")


cat("how many households?",
    length(unique(raw_df$household_id)),
    sep = "\n")


#date interval
raw_df$start_dt2<- as.Date(raw_df$start_dt, format="%d%b%Y")
raw_df$end_dt2<- as.Date(raw_df$end_dt, format="%d%b%Y")
min(raw_df$start_dt2) 
max(raw_df$end_dt2)

cat("date interval?",
    "from:", paste0(min(raw_df$start_dt2)),
    "to:", paste0(max(raw_df$end_dt2)),
    sep = "\n")


cat("how many swabbed?",
    paste0(table(raw_df$swabbed)[2]),
    "Amongst swabbed, how many positive?",
    table(raw_df$swaboutcome)[3],
    sep = "\n")


cat("how many positive illnesses?",
    length(unique(df$illnessid)),
    sep = "\n")


#FROM TRANSMISSION DATA
cat("how many positive households?",
    length(unique(certain_transmissions$household_id)),
    sep = "\n")


#date interval:
cat("date interval from earliest COVID+ illness to lastest?",
    "from:", paste0(min(certain_transmissions$infector_start_date) ),
    "to:", paste0(max(certain_transmissions$infectee_start_date)),
    sep = "\n")


cat("how many infector/infectee pairs ?",
    nrow(certain_transmissions),
    sep = "\n")


cat("how many individuals ?",
    length(
      unique(
        c(certain_transmissions$infector_ID, 
          certain_transmissions$infectee_ID)
      )
    ),
    sep = "\n")


warning("Results.R ran successfully")
