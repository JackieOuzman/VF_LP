#step1 one pull out the all the resting data only.

week1_2_3 <- read_csv("W:/VF/LongPlain/R_scripts_etc/VF_LP/resting_spots_for_paper/week1_2_3_start.csv", 
                      col_types = cols(GMT = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                       date = col_date(format = "%Y-%m-%d"), 
                                       local_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                       timeOfEvent = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
week1_2_3 <- week1_2_3 %>%
  mutate(time = hms::as_hms(local_time))
#only select the resting data
week1_2_3_rest <- week1_2_3 %>%  filter(!is.na(week1_2_3$resting.))
unique(week1_2_3_rest$group)

week1_2_3_rest_control <- week1_2_3 %>%  filter(group == "control" )

week1_2_3_rest_control <- week1_2_3_rest_control %>%
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


#change the local time to correct format
week1_2_3_rest_control$local_time <- as.POSIXct(week1_2_3_rest_control$local_time)


#round the local time to the closest 10 mins

week1_2_3_rest_control <- week1_2_3_rest_control %>%
  mutate(hours = substr(time, 1, 2),
         mins = substr(time, 4, 5),
         round_local_time = lubridate::round_date(local_time, "10 minutes") )


# create a df that has regular time step No sure I need this bit??
min(week1_2_3_rest_control$round_local_time)
max(week1_2_3_rest_control$round_local_time)

time_step_df_control <- data.frame( round_local_time =
                              seq(as.POSIXct(min(week1_2_3_rest$round_local_time)), #start time
                                  as.POSIXct(max(week1_2_3_rest$round_local_time)), #end time
                                  dminutes(10)) #inteval 
)

#my time step may not match the observered data how should I make it join?
#when there are multiple entries for one rounded value I could:
#1. take the unique value or take a average.
# I will try taking a unique value and see what happens

#only keep unique round time values for each animal
week1_2_3_rest_dis_control <- week1_2_3_rest_control %>% 
  group_by(deviceName) %>% 
  distinct(round_local_time, .keep_all= TRUE)






week1_2_3_rest_dis_control <- week1_2_3_rest_dis_control %>% 
  dplyr::select(round_local_time, 
                deviceName,
                gpsData.lat,
                gpsData.lng, 
                resting,
                fencesID_1,
                day_since_vf_start)


week1_2_3_rest_dis_control <- week1_2_3_rest_dis_control %>% 
  mutate(resting_threshold = case_when(
    resting >= 20 ~ 1,
    TRUE ~ NA_real_))





week1_2_3_rest_dis_control_sf <-
  st_as_sf(week1_2_3_rest_dis_control,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

week1_2_3_rest_dis_control_sf_trans <-
  st_transform(week1_2_3_rest_dis_control_sf, crs = 28354)

week1_2_3_rest_dis_control_sf_trans_clip <-
  st_intersection(week1_2_3_rest_dis_control_sf_trans, long_plains)    







resting_location_VF_area1_control

### what are is the local time for the matching VF data match the time frames for each VF area

### should also be clipped to VF bounary time

check2 <- week1_2_3 %>% group_by( group, fencesID_1) %>% 
  summarise(min_local_time = ymd_hms(min(local_time), tz= "Australia/Adelaide"),
            max_local_time = ymd_hms(max(local_time), tz= "Australia/Adelaide")) %>% 
  arrange(min_local_time)

#check2

control_time_clip_AreaVF1 <-
  week1_2_3_rest_dis_control_sf_trans_clip %>%
  filter(between(
    ymd_hms(round_local_time),
    ymd_hms("2020-10-21 15:07:00"), #lower value
    ymd_hms("2020-10-25 10:47:16")  #higher value
  )) 


control_time_clip_AreaVF2 <-
  week1_2_3_rest_dis_control_sf_trans_clip %>%
  filter(between(
    ymd_hms(round_local_time),
    ymd_hms("2020-10-25 10:57:00"), #lower value
    ymd_hms("2020-10-30 07:59:37")  #higher value
  )) 

control_time_clip_AreaVF3 <-
  week1_2_3_rest_dis_control_sf_trans_clip %>%
  filter(between(
    ymd_hms(round_local_time),
    ymd_hms("2020-10-30 08:07:00"), #lower value
    ymd_hms("2020-11-03 11:57:16")  #higher value
  )) 

control_time_clip_AreaVF4 <-
  week1_2_3_rest_dis_control_sf_trans_clip %>%
  filter(between(
    ymd_hms(round_local_time),
    ymd_hms("2020-11-03 12:07:00"), #lower value
    ymd_hms("2020-11-09 08:57:16")  #higher value
  )) 



#### chcek plot

resting_location_VF_area1_control <- ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = filter(long_plains_Vf_area, day_since_trial_start == 1),
          color = "black", fill = NA) +
  geom_sf(data = filter(control_time_clip_AreaVF1,!is.na(resting_threshold)),
          alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "VF area 1")