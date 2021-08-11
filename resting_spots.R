#good place to start would be  
str(week1_2_3)
write.csv(week1_2_3, "W:/VF/LongPlain/R_scripts_etc/VF_LP/resting_spots_for_paper/week1_2_3_start.csv")
#step1 one pull out the all the resting data only.
week1_2_3 <- read.csv("W:/VF/LongPlain/R_scripts_etc/VF_LP/resting_spots_for_paper/week1_2_3_start.csv")

unique(week1_2_3$resting.)
str(week1_2_3)

week1_2_3_rest <- week1_2_3 %>%  filter(!is.na(week1_2_3$resting.))

week1_2_3_rest <- week1_2_3_rest %>%
  dplyr::select(
    deviceName,
    gpsData.lat,
    gpsData.lng,
    fencesID,
    fencesID_1,
    resting = resting.,
    local_time,
    date,
    day_since_vf_start,
    time
  )

rest_2020_10_21_9370004 <- week1_2_3_rest %>%
  filter(date == "2020-10-21" & deviceName == "9370004") %>% 
  arrange(local_time)
str(rest_2020_10_21_9370004$local_time)#its a factor??
rest_2020_10_21_9370004$local_time <- as.POSIXlt(rest_2020_10_21_9370004$local_time)


#round the local time to the closest 15 mins

rest_2020_10_21_9370004 <- rest_2020_10_21_9370004 %>%
  mutate(hours = substr(time, 1, 2),
         mins = substr(time, 4, 5),
         round_local_time = lubridate::round_date(local_time, "15 minutes") )
