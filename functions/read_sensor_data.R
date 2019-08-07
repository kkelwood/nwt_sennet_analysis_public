# Read in sensor data from niwot sennet

# file <- "../sennet_data/sn_006_cr1000x_tenminute (7).dat"
# filename <- "../sennet_data/sn_006_cr1000x_tenminute (7).dat"
# filename <- data_list[1]
library(dplyr)
read_sensor_data <- function(filename) {
    temp <- read.csv(filename, skip = 1, na.strings = "NAN")
    units <- temp[1:2,]
    assign("units", units, envir = globalenv())
    temp <- temp[3:nrow(temp),]
    temp2 <- temp %>% 
        mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>% 
        mutate(RECORD = as.numeric(as.character(RECORD))) %>% 
        mutate(sensornode = as.numeric(as.character(sensornode))) %>% 
        mutate(airtemp_Max = as.numeric(as.character(airtemp_Max))) %>% 
        mutate(airtemp_TMx = as.POSIXct(airtemp_TMx, format = "%Y-%m-%d %H:%M:%S")) %>% 
        mutate(airtemp_Min = as.numeric(as.character(airtemp_Min))) %>% 
        mutate(airtemp_TMn = as.POSIXct(airtemp_TMn, format = "%Y-%m-%d %H:%M:%S")) %>% 
        mutate(airtemp_Avg = as.numeric(as.character(airtemp_Avg))) %>% 
        mutate(rh_Max = as.numeric(as.character(rh_Max))) %>% 
        mutate(rh_TMx = as.POSIXct(rh_TMx, format = "%Y-%m-%d %H:%M:%S")) %>% 
        mutate(rh_Min = as.numeric(as.character(rh_Min))) %>% 
        mutate(rh_TMn = as.POSIXct(rh_TMn, format = "%Y-%m-%d %H:%M:%S")) %>% 
        mutate(rh_Avg = as.numeric(as.character(rh_Avg))) %>% 
        mutate(soiltemp_5cm_Avg = as.numeric(as.character(soiltemp_5cm_Avg))) %>% 
        mutate(soiltemp_30cm_Avg = as.numeric(as.character(soiltemp_30cm_Avg))) %>% 
        mutate(vwca_5cm_Avg = as.numeric(as.character(vwca_5cm_Avg))) %>% 
        mutate(vwca_30cm_Avg = as.numeric(as.character(vwca_30cm_Avg))) %>% 
        mutate(vwcb_5cm_Avg = as.numeric(as.character(vwcb_5cm_Avg))) %>% 
        mutate(vwcb_30cm_Avg = as.numeric(as.character(vwcb_30cm_Avg))) %>% 
        mutate(vwcc_5cm_Avg = as.numeric(as.character(vwcc_5cm_Avg))) %>% 
        mutate(vwcc_30cm_Avg = as.numeric(as.character(vwcc_30cm_Avg))) %>% 
        mutate(panltemp = as.numeric(as.character(panltemp))) %>% 
        mutate(battvolt_Min = as.numeric(as.character(battvolt_Min))) %>% 
        mutate(airtemp_Std = as.numeric(as.character(airtemp_Std))) %>% 
        mutate(rh_Std = as.numeric(as.character(rh_Std))) %>% 
        mutate(soiltemp_5cm_Std = as.numeric(as.character(soiltemp_5cm_Std))) %>% 
        mutate(soiltemp_30cm_Std = as.numeric(as.character(soiltemp_30cm_Std))) %>% 
        mutate(vwca_5cm_Std = as.numeric(as.character(vwca_5cm_Std))) %>% 
        mutate(vwca_30cm_Std = as.numeric(as.character(vwca_30cm_Std))) %>% 
        mutate(vwcb_5cm_Std = as.numeric(as.character(vwcb_5cm_Std))) %>% 
        mutate(vwcb_30cm_Std = as.numeric(as.character(vwcb_30cm_Std))) %>% 
        mutate(vwcc_5cm_Std = as.numeric(as.character(vwcc_5cm_Std))) %>% 
        mutate(vwcc_30cm_Std = as.numeric(as.character(vwcc_30cm_Std)))
    # occassional columns:
    if("ppt_Tot" %in% colnames(temp2)) temp2$ppt_Tot = as.numeric(as.character(temp2$ppt_Tot))
    if("ppt2_Tot" %in% colnames(temp2)) {temp2$ppt2_Tot = as.numeric(as.character(temp2$ppt2_Tot))}
    if("ppt_Std" %in% colnames(temp2)) temp2$ppt_Std = as.numeric(as.character(temp2$ppt_Std))
    if("ppt2_Std" %in% colnames(temp2)) {temp2$ppt2_Std = as.numeric(as.character(temp2$ppt2_Std))}
    # if("snowdepth" %in% colnames(temp2)) {temp2$dts = as.numeric(as.character(temp2$snowdepth))}
    if("snowdepth" %in% colnames(temp2)) {names(temp2)[names(temp2) == 'snowdepth'] <- 'dts'}
    # if("snowdepth_Std" %in% colnames(temp2)) {temp2$dts_Std = as.numeric(as.character(temp2$snowdepth_Std))}
    if("snowdepth_Std" %in% colnames(temp2)) {names(temp2)[names(temp2) == 'snowdepth_Std'] <- 'dts_Std'}
    if("dts" %in% colnames(temp2)) {temp2$dts = as.numeric(as.character(temp2$dts))}
    if("dts_Min" %in% colnames(temp2)) {temp2$dts_Min = as.numeric(as.character(temp2$dts_Min))}
    if("dts_Std" %in% colnames(temp2)) {temp2$dts_Std = as.numeric(as.character(temp2$dts_Std))}
    sensor_number <- as.character(mean(temp2$sensornode))
    sensor_number_2 <- ifelse(nchar(sensor_number) == 1, paste0("0", sensor_number), sensor_number)
    assign(x = paste0("sensor_", sensor_number_2), temp2, envir = globalenv())
    
}
