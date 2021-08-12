
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
library(rgdal)
library(sf)

library(plotKML)
library(knitr)
library(png)
library(readxl)
library(ggmap)


#good place to start would be  

#step1 one pull out the all the resting data only.
week1_2_3 <- read.csv("W:/VF/LongPlain/R_scripts_etc/VF_LP/resting_spots_for_paper/week1_2_3_start.csv")


#only select the resting data
week1_2_3_rest <- week1_2_3 %>%  filter(!is.na(week1_2_3$resting.))
week1_2_3_rest <- week1_2_3 %>%  filter(group == "VF" )

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
# ## work with a small subset of data one animal one day.
# rest_2020_10_21_9370004 <- week1_2_3_rest %>%
#   filter(date == "2020-10-21" & deviceName == "9370004") %>% 
#   arrange(local_time)

#change the local time to correct format
week1_2_3_rest$local_time <- as.POSIXct(week1_2_3_rest$local_time)


#round the local time to the closest 10 mins

week1_2_3_rest <- week1_2_3_rest %>%
  mutate(hours = substr(time, 1, 2),
         mins = substr(time, 4, 5),
         round_local_time = lubridate::round_date(local_time, "10 minutes") )


# create a df that has regular time step No sure I need this bit??
min(week1_2_3_rest$round_local_time)
max(week1_2_3_rest$round_local_time)

time_step_df <- data.frame( round_local_time =
seq(as.POSIXct(min(week1_2_3_rest$round_local_time)), #start time
    as.POSIXct(max(week1_2_3_rest$round_local_time)), #end time
    dminutes(10)) #inteval 
)

#my time step may not match the observered data how should I make it join?
#when there are multiple entries for one rounded value I could:
#1. take the unique value or take a average.
# I will try taking a unique value and see what happens

#only keep unique round time values for each animal
week1_2_3_rest_dis <- week1_2_3_rest %>% 
  group_by(deviceName) %>% 
    distinct(round_local_time, .keep_all= TRUE)
# ##seems to be working :)
# check <- week1_2_3_rest_dis %>% 
#   filter(deviceName == "9370004")
  
  
unique(week1_2_3_rest_dis$deviceName)
str(time_step_df)
#if I need this bit turn it into a loop
time_step_by_9370004 <- time_step_df %>% 
  mutate(round_local_time_device = 
           paste0(round_local_time, "_", "9370004"))
time_step_by_9370088 <- time_step_df %>% 
  mutate(round_local_time_device = 
           paste0(round_local_time, "_", "9370088"))

#join my time step to the observed data 
#(this should ensure that if I have missing data I still have a placeholder)
#rest_template_one_animal <- left_join(time_step_df,week1_2_3_rest_dis) 


 
 week1_2_3_rest_dis <- week1_2_3_rest_dis %>% 
  dplyr::select(round_local_time, 
                deviceName,
                gpsData.lat,
                gpsData.lng, 
                resting,
                fencesID_1,
                day_since_vf_start)
                

 week1_2_3_rest_dis <- week1_2_3_rest_dis %>% 
  mutate(resting_threshold = case_when(
    resting >= 20 ~ 1,
    TRUE ~ NA_real_))


#need to make df into spatial object for graphing and try clipping to paddock boundary
long_plains <- st_read("W:/VF/LongPlain/LP Blk Bound/LongPlain_GDA_internal_bound.shp")
long_plains_Vf <- st_read("W:/VF/LongPlain/LP Blk Bound/LongPlainVF_bound.shp")

#str(week1_2_3)


week1_2_3_rest_dis_sf <-
  st_as_sf(week1_2_3_rest_dis,
           coords = c("gpsData.lng", "gpsData.lat"),
           crs = 4326,
           agr = "constant")

week1_2_3_rest_dis_sf_trans <-
  st_transform(week1_2_3_rest_dis_sf, crs = 28354)

week1_2_3_rest_dis_sf_trans_clip <-
  st_intersection(week1_2_3_rest_dis_sf_trans, long_plains)    
###  paddock bounadry
#assign a coord ref epsg
st_crs(long_plains) <- 28354

#make this the epsg that will be used for all data
the_crs <- st_crs(long_plains, asText = TRUE)

### VF lines
st_crs(long_plains_Vf) <- 28354

##All of the data
ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = filter(week1_2_3_rest_dis_sf_trans_clip,!is.na(resting_threshold)),
          alpha = 0.1) +
    theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Resting data only")


### Clip the data for VR area.
long_plains_Area_Vf1 <- long_plains_Vf %>% 
  filter(VF_Agersen == "VF1")

long_plains_Area_Vf1_VF2 <- long_plains_Vf %>% 
  filter(VF_Agersen == "VF2" | VF_Agersen == "VF1") 

long_plains_Area_Vf1_VF2_Vf3 <- long_plains_Vf %>% 
  filter( VF_Agersen == "VF3" |VF_Agersen == "VF2" | VF_Agersen == "VF1")  

long_plains_Area_Vf1_VF2_Vf3_Vf4 <- long_plains_Vf %>% 
  filter( VF_Agersen == "VF4"|VF_Agersen == "VF3" |VF_Agersen == "VF2" | VF_Agersen == "VF1")   

# ggplot() +
#   geom_sf(data = long_plains, color = "black", fill = NA) +
#   geom_sf(data = long_plains_Area_Vf1, color = "green", fill = NA) +
#   
#   geom_sf(data = long_plains_Area_Vf1_VF2_Vf3_Vf4, color = "grey", fill = NA) +
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
#   labs(title= "location of boundary")


week1_2_3_rest_dis_clip_AreaVF1 <-
  st_intersection(week1_2_3_rest_dis_sf_trans_clip, long_plains_Area_Vf1)   
week1_2_3_rest_dis_clip_AreaVF1_2 <-
  st_intersection(week1_2_3_rest_dis_sf_trans_clip, long_plains_Area_Vf1_VF2) 
week1_2_3_rest_dis_clip_AreaVF1_2_3 <-
  st_intersection(week1_2_3_rest_dis_sf_trans_clip, long_plains_Area_Vf1_VF2_Vf3) 
week1_2_3_rest_dis_clip_AreaVF1_2_3_4 <-
  st_intersection(week1_2_3_rest_dis_sf_trans_clip, long_plains_Area_Vf1_VF2_Vf3_Vf4) 


resting_location_VF_area1 <- ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = filter(week1_2_3_rest_dis_clip_AreaVF1,!is.na(resting_threshold)),
          alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Resting data only VF area 1")

resting_location_VF_area1_2 <-ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = filter(week1_2_3_rest_dis_clip_AreaVF1_2,!is.na(resting_threshold)),
          alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Resting data only VF area 1 and 2")

resting_location_VF_area1_2_3 <-ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  geom_sf(data = filter(week1_2_3_rest_dis_clip_AreaVF1_2_3,!is.na(resting_threshold)),
          alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Resting data only VF area 1, 2 and 3")

resting_location_VF_area1_2_3_4 <-ggplot() +
  geom_sf(data = long_plains, color = "black", fill = NA) +
  geom_sf(data = long_plains_Vf, color = "black", fill = NA) +
  #geom_sf(data = long_plains_Area_Vf1_VF2_Vf3_Vf4, color = "green", fill = NA) +
  geom_sf(data = filter(week1_2_3_rest_dis_clip_AreaVF1_2_3_4,!is.na(resting_threshold)),
          alpha = 0.01) +
  theme_bw()+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title= "Resting data only VF area 1, 2, 3 and 4")


library("cowplot")
resting_location_by_area <- plot_grid(resting_location_VF_area1, 
          resting_location_VF_area1_2, 
          resting_location_VF_area1_2_3, 
          resting_location_VF_area1_2_3_4)#, #if you want to add the below code remove the bracket and replace with ,
          #labels = c("A", "B", "C", "D"),
          #ncol = 2, nrow = 2)
resting_location_by_area
