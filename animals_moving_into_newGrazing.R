
long_plains_Vf2 <- long_plains_Vf %>% 
  filter(VF_Agersen == "VF2") 
long_plains_Vf1 <- long_plains_Vf %>% 
  filter(VF_Agersen == "VF1")
long_plains_Vf3 <- long_plains_Vf %>% 
  filter(VF_Agersen == "VF3") 
long_plains_Vf4 <- long_plains_Vf %>% 
  filter(VF_Agersen == "VF4") 


## week 3 import raw data and format

csiro_week3exports_ending_2020_11_09 <- read.csv("W:/VF/LongPlain/Collar_data/csiro_weeklyexports_ending_2020-11-09.csv")

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

#######################################################################################################################
# Use raw data and select a window that is close to the fence moving

week3_11eab_only <- week3_sf_trans %>% filter(fencesID =="11eab")
max_time_11eab <- week3_11eab_only %>% group_by() %>%
  summarise(max_local_time =  max(local_time, na.rm = TRUE))                                                        
max_time_11eab # 2020-11-03 11:57:16 
fence_3_deactiveated <- max_time_11eab


window_deactive <- week3_sf_trans %>% 
  filter(between(ymd_hms(local_time), ymd_hms("2020-11-03 11:40:00"), ymd_hms("2020-11-03 16:40:00"))) 

#this is VF mob 5 hour after fence has been deactivated
#clip my selected date VF mob data to the new paddock area
deactive_window_clip_VF4 <-  st_intersection(window_deactive, long_plains_Vf4)
deactive_window_clip_VF1_4 <- st_intersection(window_deactive, long_plains_Vf)

#check that its sensible
fence_3_deactiveated_move <- deactive_window_clip_VF4 %>% group_by() %>% 
  summarise(min_local_time =    min(local_time,na.rm = TRUE))#this is just a chcek


fence_3_deactiveated_move <- st_set_geometry(fence_3_deactiveated_move, NULL)
fence_3_deactiveated <- st_set_geometry(fence_3_deactiveated, NULL)
fence_3_deactiveated_move #(min_local_time)
fence_3_deactiveated #max_local_time
fence_3_deactiveated <- fence_3_deactiveated %>%  
  dplyr::rename("local_time" = "max_local_time")
fence_3_deactiveated_move <- fence_3_deactiveated_move %>%  
  dplyr::rename("local_time" = "min_local_time")

fence_deactivated <- rbind(fence_3_deactiveated, fence_3_deactiveated_move)

fence_details_xxx <- c("fence_deactivate", "fence_deactivate_animal_move")
animal_move_03 <- data.frame(fence_details_xxx, fence_deactivated) 
animal_move_03 <- animal_move_03 %>%  dplyr::rename("fence_details" = "fence_details_xxx")
animal_move_03


#Plot all data / animals in window - facet is the hours
deactive_window_clip_VF1_4 <- deactive_window_clip_VF1_4 %>% 
  mutate(time = hms::as_hms(local_time),
         hours = substr(time, 1, 2))  

ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = deactive_window_clip_VF1_4 ,colour = "skyblue4", alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "deactivation of fence 4 2020-11-03 11:56:00")+
  facet_wrap(.~ hours)


  
##################################################################################################################
## week 1 import raw data and format

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

 
#######################################################################################
week1_1922f_only <- week1_sf_trans %>% filter(fencesID =="1922f")
min_time_1922f <- week1_1922f_only %>% group_by() %>%
  summarise(min_local_time =  min(local_time, na.rm = TRUE))                                                        
min_time_1922f # 2020-10-25 10:57:00 # I am happy with this 
fence_2_activated <- min_time_1922f


window_active1922f <- week1_sf_trans %>% 
  filter(between(ymd_hms(local_time), ymd_hms("2020-10-25 10:57:00"), ymd_hms("2020-10-25 18:57:00"))) 

#this is VF mob 5 hour after fence has been deactivated
#clip my selected date VF mob data to the new paddock area
active1922F_window_clip_VF2 <-  st_intersection(window_active1922f, long_plains_Vf2)

#check that its sensible
fence_2_active_move <- active1922F_window_clip_VF2 %>% group_by() %>% 
  summarise(min_local_time = min(local_time,na.rm = TRUE))#this is just a chcek
fence_2_active_move

fence_2_active_move <- st_set_geometry(fence_2_active_move, NULL)
fence_2_activated <- st_set_geometry(fence_2_activated, NULL)

fence_2_active <- rbind(fence_2_activated, fence_2_active_move)

fence2_details <- c("fence_activated", "fence_activate_animal_move")
animal_move_25 <- data.frame(fence2_details, fence_2_active) 
animal_move_25 <- animal_move_25 %>%  dplyr::rename("fence_details" = "fence2_details",
                                                    "local_time" = "min_local_time")

animal_move_25_03 <- rbind(animal_move_25, animal_move_03)
animal_move_25_03

#Plot all data / animals in window - facet is the hours
window_active1922f <- window_active1922f %>% 
  mutate(time = hms::as_hms(local_time),
         hours = substr(time, 1, 2))  
View(window_active1922f)
ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = window_active1922f ,colour = "skyblue4", alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Activation of fence 2 2020-10-25 10:57:00")+
  facet_wrap(.~ hours)

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

#######################################################################################
week2_11eab_only <- week2_sf_trans %>% filter(fencesID =="11eab")
min_time_11aeb <- week2_11eab_only %>% group_by() %>%
  summarise(min_local_time =  min(local_time, na.rm = TRUE))                                                        
min_time_11aeb #  2020-10-30 08:07:00 # I am happy with this 
fence_3_activated <- min_time_11aeb
fence_3_activated

window_active11eab <- week2_sf_trans %>% 
  filter(between(ymd_hms(local_time), ymd_hms("2020-10-30 08:07:00"), ymd_hms("2020-10-30 08:50:00"))) 

#this is VF mob 5 hour after fence has been deactivated
#clip my selected date VF mob data to the new paddock area
active11eab_window_clip_VF3 <-  st_intersection(window_active11eab, long_plains_Vf3)
#View(active11eab_window_clip_VF3)

#check that its sensible
fence_3_active_move <- active11eab_window_clip_VF3 %>% group_by() %>% 
  summarise(min_local_time = min(local_time,na.rm = TRUE))#this is just a chcek
fence_3_active_move
fence_3_active_move <- st_set_geometry(fence_3_active_move, NULL)
fence_3_activated <- st_set_geometry(fence_3_activated, NULL)
fence_3_active <- rbind(fence_3_activated, fence_3_active_move)

fence3_details <- c("fence_activated", "fence_activate_animal_move")
animal_move_30 <- data.frame(fence3_details, fence_3_active) 
animal_move_30 <- animal_move_30 %>%  dplyr::rename("fence_details" = "fence3_details",
                                                    "local_time" = "min_local_time")

animal_move_25_03_30 <- rbind(animal_move_25_03, animal_move_30)
animal_move_25_03_30

round_hms(as_hms("12:34:56"), 60)


#Plot all data / animals in window - facet is the hours
window_active11eab <- window_active11eab %>% 
  mutate(time = hms::as_hms(local_time),
         hours = substr(time, 1, 2),
         mins = substr(time, 4, 5),
         min_round_10mins = round_any(as.double(mins), 10))  
str(window_active11eab)
View(window_active11eab)
ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf3, color = "green", fill =NA) +
  geom_sf(data = window_active11eab ,colour = "skyblue4", alpha = 1) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Activation of fence 3 2020-10-30 08:07:00",
       subtitle = "facet wrap is every 10 min from 08:10 to 8:50" )+
  facet_wrap(.~ min_round_10mins)

