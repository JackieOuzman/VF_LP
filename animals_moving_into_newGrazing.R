## week 3 

csiro_week3exports_ending_2020_11_09 <- read.csv("W:/VF/LongPlain/Collar_data/csiro_weeklyexports_ending_2020-11-09.csv")
#csiro_week3exports_novf_ending_2020_11_09 <- read.csv("W:/VF/LongPlain/Collar_data/csiro_weeklyexports_novf_ending_2020-11-09.csv")


csiro_week3exports_ending_2020_11_09 <- csiro_week3exports_ending_2020_11_09 %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"),
         group = "VF")
csiro_week3exports_ending_2020_11_09 <- csiro_week3exports_ending_2020_11_09 %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))


week3<- csiro_week3exports_ending_2020_11_09 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


week3 <- week3 %>%  filter(local_time >= ymd_hms("2020-10-21 15:07:00", tz= "Australia/Adelaide"), 
                                   local_time <=  ymd_hms("2020-11-09 09:00:00", tz= "Australia/Adelaide"))



week3_sf <-
  st_as_sf(week3,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

week3_sf_trans <-
  st_transform(week3_sf, crs = 28354)
str(week3_sf_trans)
#######################################################################################################################
# i could take a different appraoch and just select data with a certain time after the fence has be deactiveativated.

week3_11eab_only <- week3_sf_trans %>% filter(fencesID =="11eab")
max_time_11eab <- week3_11eab_only %>% group_by() %>%
  summarise(max_local_time =  max(local_time, na.rm = TRUE))                                                        
max_time_11eab # 2020-11-03 11:57:16 
fence_3_deactiveated <- max_time_11eab


window_deactive <- week3_sf_trans %>% 
  filter(between(ymd_hms(local_time), ymd_hms("2020-11-03 11:40:00"), ymd_hms("2020-11-03 16:40:00"))) 
# window_deactive %>% group_by() %>%
#   summarise(max_local_time =  max(local_time, na.rm = TRUE),
#             min_local_time =  min(local_time, na.rm = TRUE)) 
# View(window_deactive) #there is not big gap in time the data entries looks corrects




#this is VF mob 5 hour after fence has been deactivated
#clip my selected date VF mob data to the new paddock area
deactive_window_clip_VF4 <-  st_intersection(window_deactive, long_plains_Vf4)
deactive_window_clip_VF3 <-  st_intersection(window_deactive, long_plains_Vf3)
deactive_window_clip_VF1_4 <- st_intersection(window_deactive, long_plains_Vf)


View(deactive_window_clip_VF4)
View(deactive_window_clip_VF1_4)

#check that its sensible
fence_3_deactiveated_move <- deactive_window_clip_VF4 %>% group_by() %>% 
  summarise(min_local_time =    min(local_time,na.rm = TRUE))#this is just a chcek


fence_3_deactiveated_move <- st_set_geometry(fence_3_deactiveated_move, NULL)
fence_3_deactiveated <- st_set_geometry(fence_3_deactiveated, NULL)

fence_3_deactiveated_move
fence_3_deactiveated

fence_deactivated <- rbind("2020-11-03 11:57:16", "2020-11-03 16:07:01")
fence_deactivated


fence_details_xxx <- c("fence_deactivate", "fence_deactivate_animal_move")
animal_move_03 <- data.frame(fence_details_xxx, fence_deactivated) 
animal_move_03
animal_move_03 <- animal_move_03 %>%  dplyr::rename("fence_details" = "fence_details_xxx")


##################################################################################################################
## week 1

csiro_weeklyexports_ending_2020_10_26 <- read.csv("C:/Users/ouz001/working_from_home/VF_Long_plain/logged_cattle/csiro_weeklyexports_ending_2020-10-26.csv")



csiro_weeklyexports_ending_2020_10_26 <- csiro_weeklyexports_ending_2020_10_26 %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"),
         group = "VF")
csiro_weeklyexports_ending_2020_10_26 <- csiro_weeklyexports_ending_2020_10_26 %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))


week1<- csiro_weeklyexports_ending_2020_10_26 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


week1 <- week1 %>%  filter(local_time >= ymd_hms("2020-10-21 15:07:00", tz= "Australia/Adelaide"), 
                           local_time <=  ymd_hms("2020-11-09 09:00:00", tz= "Australia/Adelaide"))



week1_sf <-
  st_as_sf(week1,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

week1_sf_trans <-
  st_transform(week1_sf, crs = 28354)
str(week1_sf_trans)
 
#######################################################################################
week1_1922f_only <- week1_sf_trans %>% filter(fencesID =="1922f")
min_time_1922f <- week1_1922f_only %>% group_by() %>%
  summarise(min_local_time =  min(local_time, na.rm = TRUE))                                                        
min_time_1922f # 2020-10-25 10:57:00 # I am happy with this 
fence_2_activated <- min_time_1922f


window_active1922f <- week1_sf_trans %>% 
  filter(between(ymd_hms(local_time), ymd_hms("2020-10-25 10:57:00"), ymd_hms("2020-10-25 18:57:00"))) 
# window_active1922f %>% group_by() %>%
#   summarise(max_local_time =  max(local_time, na.rm = TRUE),
#             min_local_time =  min(local_time, na.rm = TRUE))
# View(window_active1922f) #there is not big gap in time the data entries looks corrects




#this is VF mob 5 hour after fence has been deactivated
#clip my selected date VF mob data to the new paddock area
active1922F_window_clip_VF2 <-  st_intersection(window_active1922f, long_plains_Vf2)
View(active1922F_window_clip_VF2)

#check that its sensible
fence_2_active_move <- active1922F_window_clip_VF2 %>% group_by() %>% 
  summarise(min_local_time = min(local_time,na.rm = TRUE))#this is just a chcek
fence_2_active_move

fence_2_active_move <- st_set_geometry(fence_2_active_move, NULL)
fence_2_activated <- st_set_geometry(fence_2_activated, NULL)

fence_2_active_move
fence_2_activated

fence_2_active <- rbind(fence_2_activated, fence_2_active_move)
fence_2_active


fence2_details <- c("fence_activated", "fence_activate_animal_move")
animal_move_25 <- data.frame(fence2_details, fence_2_active) 
animal_move_25
animal_move_25 <- animal_move_25 %>%  dplyr::rename("fence_details" = "fence2_details")


animal_move_03 <- animal_move_03 %>%  dplyr::rename("min_local_time" = "fence_deactivated")
animal_move_25_03 <- rbind(animal_move_25, animal_move_03)
animal_move_25_03


#####################################################################################################################
csiro_week2exports_ending_2020_11_02 <- read.csv("W:/VF/LongPlain/Collar_data/csiro_weeklyexports_ending_2020-11-02.csv")

csiro_week2exports_ending_2020_11_02 <- csiro_week2exports_ending_2020_11_02 %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"),
         group = "VF")
csiro_week2exports_ending_2020_11_02 <- csiro_week2exports_ending_2020_11_02 %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Adelaide"))

week2<- csiro_week2exports_ending_2020_11_02 %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Adelaide"),
         DOY = yday(date))


week2 <- week2 %>%  filter(local_time >= ymd_hms("2020-10-21 15:07:00", tz= "Australia/Adelaide"), 
                           local_time <=  ymd_hms("2020-11-09 09:00:00", tz= "Australia/Adelaide"))



week2_sf <-
  st_as_sf(week2,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

week2_sf_trans <-
  st_transform(week2_sf, crs = 28354)
str(week2_sf_trans)
#######################################################################################
week2_11eab_only <- week2_sf_trans %>% filter(fencesID =="11eab")
min_time_11aeb <- week2_11eab_only %>% group_by() %>%
  summarise(min_local_time =  min(local_time, na.rm = TRUE))                                                        
min_time_11aeb #  2020-10-30 08:07:00 # I am happy with this 
fence_2_activated <- min_time_1922f


window_active11eab <- week2_sf_trans %>% 
  filter(between(ymd_hms(local_time), ymd_hms("2020-10-30 08:07:00"), ymd_hms("2020-10-30 09:07:00"))) 
# window_active11eab %>% group_by() %>%
#   summarise(max_local_time =  max(local_time, na.rm = TRUE),
#             min_local_time =  min(local_time, na.rm = TRUE))
# View(window_active1922f) #there is not big gap in time the data entries looks corrects




#this is VF mob 5 hour after fence has been deactivated
#clip my selected date VF mob data to the new paddock area
active11eab_window_clip_VF3 <-  st_intersection(window_active11eab, long_plains_Vf3)
#View(active11eab_window_clip_VF3)

#check that its sensible
fence_3_active_move <- active11eab_window_clip_VF3 %>% group_by() %>% 
  summarise(min_local_time = min(local_time,na.rm = TRUE))#this is just a chcek
fence_3_active_move
