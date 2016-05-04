# install.packages('xlsx')
githubDir = "https://raw.githubusercontent.com/somsubhra88/Vehicle_Route_Optimization/master/"
library(xlsx)
library(dplyr)
## Setting up the Working Date
setwd('C:/Users/somsubhra.g/Google Drive/eclipse/routeOptimization_R')
## Data File
fileName = "kol_data_30-4.xlsx"
## Hub Lat Lng
hubLatLng = c(77.6533668, 12.8852659) #Kudlu
# hubLatLng = c(88.317647, 22.739977 )  #Kolkata
# hubLatLng = c(73.034974, 19.237481)   #Bhiwandi

data = 
  read.xlsx(file = fileName,sheetIndex = 1) %>%
  dplyr::select(.,tracking_id,address_line1,address_line2,address_pincode,address_city,
           address_state,fsn_length,fsn_breadth,fsn_height,slot_start_dt,slot_end_dt) %>%
  transform(.,slot_start_dt = 100*as.POSIXlt(slot_start_dt+1)$hour,
              slot_end_dt = 100*as.POSIXlt(slot_end_dt+1)$hour) %>%
  rename(.,order_external_id = tracking_id,Addr1 = address_line1,Addr2 = address_line2,
         Pincode = address_pincode,City = address_city,State = address_state,
         Length = fsn_length,	Height = fsn_height, Width = fsn_breadth,
         Slot_Start_Time = slot_start_dt, Slot_End_Time = slot_end_dt) %>%
  transform(.,Slot_Start_Time = ifelse(is.na(Slot_Start_Time),800,Slot_Start_Time),
            Slot_End_Time = ifelse(is.na(Slot_End_Time),2000,Slot_End_Time)) %>%
  filter(.,!is.na(order_external_id))
