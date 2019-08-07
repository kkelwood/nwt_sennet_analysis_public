# weekly summary
library(lubridate)
library(dplyr)
weekly_summary <- function(df, year) {
    temp_ground <- quantile(df$dts, c(0.9), na.rm = TRUE)
    df$snowdepth <- temp_ground - df$dts
    sensor_name <- mean(df$sensornode)
    temp <- df %>% 
        filter(year(TIMESTAMP) == year) %>% 
        group_by(week(TIMESTAMP)) %>% 
        summarise(sm5a_weekly_avg = mean(vwcb_5cm_Avg), 
                  sm30a_weekly_avg = mean(vwcb_30cm_Avg),
                  sm5b_weekly_avg = mean(vwcb_5cm_Avg), 
                  sm30b_weekly_avg = mean(vwcb_30cm_Avg),
                  sm5c_weekly_avg = mean(vwcb_5cm_Avg), 
                  sm30c_weekly_avg = mean(vwcb_30cm_Avg),
                  sm5a_weekly_min = min(vwcb_5cm_Avg), 
                  sm30a_weekly_min = min(vwcb_30cm_Avg),
                  sm5b_weekly_min = min(vwcb_5cm_Avg), 
                  sm30b_weekly_min = min(vwcb_30cm_Avg),
                  sm5c_weekly_min = min(vwcb_5cm_Avg), 
                  sm30c_weekly_min = min(vwcb_30cm_Avg),
                  stemp5_weekly_avg = mean(soiltemp_5cm_Avg),
                  stemp30_weekly_avg = mean(soiltemp_30cm_Avg),
                  snowdepth_weekly_avg = mean(snowdepth),
                  air_weekly_avg = mean(airtemp_Avg))
    colnames(temp)[1] <- "WEEK"
    sensor_chr <- ifelse(nchar(sensor_name) == 1, paste0("0", sensor_name), sensor_name)
    temp$sensor <- as.factor(sensor_chr)
    sensor_number_2 <- ifelse(nchar(sensor_name) == 1, paste0("0", sensor_name), sensor_name)
    assign(paste0("sensor", sensor_number_2, "_weekly_summary"), temp, envir = globalenv())
}

# daily summary
daily_summary <- function(df, year) {
    # temp_ground <- quantile(df$dts, c(0.9), na.rm = TRUE)
    # df$snowdepth <- temp_ground - df$dts
    df$snowdepth <- df$dts
    sensor_name <- mean(df$sensornode)
    temp <- df %>% 
        filter(year(TIMESTAMP) == year) %>% 
        group_by(date(TIMESTAMP)) %>% 
        summarise(sm5b_daily_avg = mean(vwcb_5cm_Avg), 
                  sm30b_daily_avg = mean(vwcb_30cm_Avg),
                  sm5b_daily_avg = mean(vwcb_5cm_Avg), 
                  sm30b_daily_avg = mean(vwcb_30cm_Avg),
                  sm5c_daily_avg = mean(vwcb_5cm_Avg), 
                  sm30c_daily_avg = mean(vwcb_30cm_Avg),
                  sm5a_daily_min = min(vwcb_5cm_Avg), 
                  sm30a_daily_min = min(vwcb_30cm_Avg),
                  sm5b_daily_min = min(vwcb_5cm_Avg), 
                  sm30b_daily_min = min(vwcb_30cm_Avg),
                  sm5c_daily_min = min(vwcb_5cm_Avg), 
                  sm30c_daily_min = min(vwcb_30cm_Avg),
                  stemp5_daily_avg = mean(soiltemp_5cm_Avg),
                  stemp30_daily_avg = mean(soiltemp_30cm_Avg),
                  snowdepth_daily_avg = mean(dts),
                  air_daily_avg = mean(airtemp_Avg))
    colnames(temp)[1] <- "DAY"
    sensor_chr <- ifelse(nchar(sensor_name) == 1, paste0("0", sensor_name), sensor_name)
    temp$sensor <- as.factor(sensor_chr)
    sensor_number_2 <- ifelse(nchar(sensor_name) == 1, paste0("0", sensor_name), sensor_name)
    assign(paste0("sensor", sensor_number_2, "_daily_summary"), temp, envir = globalenv())
}

# daily summary (90th percentile of 3-day moving window)

# practice variables
df <- sensor_08

daily_summary_3day90th <- function(df) {
    output_df <- data.frame()
    # temp_ground <- quantile(df$dts, c(0.9), na.rm = TRUE)
    # df$snowdepth <- temp_ground - df$dts
    df$snowdepth <- df$dts
    sensor_name <- mean(df$sensornode)
    sensors_at_B <- c(6,7,8,9,10,11,12,13,14,16,17,18,19,20)
    df$DATE <- as.Date(df$TIMESTAMP)
    df <- df[order(df$TIMESTAMP),]
    first_day <- min(date(df$TIMESTAMP), na.rm = TRUE)
    last_day <- max(date(df$TIMESTAMP), na.rm = TRUE)
    # second_to_last_day <- max(date(df$TIMESTAMP)) - 1
    day_list <- seq(as.Date(first_day), as.Date(last_day), by="days")
    days_to_loop <- length(day_list) - 1
    for(d in 2:days_to_loop) {
        subset_day <- subset(df, DATE == day_list[d])
        subset_3day <- subset(df, DATE == day_list[d] | DATE == day_list[d + 1] | DATE == day_list[d - 1])
        
        # populate the phenocam and soil (i.e. plot) numbers
        output_df[d, "phenocam"] <- sensor_name
        output_df[d, "DATE"] <- as.character(day_list[d])
        
        # calculate 90th percentile soil moisture at sensor closest to plot
        if(sensor_name %in% sensors_at_B) {
            output_df[d, "soil_sensor"] <- "b"
            output_df[d, "sm5cm_daily_mean"] <- mean(subset_day$vwcb_5cm_Avg, na.rm = TRUE)
            output_df[d, "sm30cm_daily_mean"] <- mean(subset_day$vwcb_30cm_Avg, na.rm = TRUE)
            output_df[d, "sm5cm_3day90th"] <- quantile(subset_3day$vwcb_5cm_Avg, 
                                                       0.9, na.rm = TRUE)
            output_df[d, "sm30cm_3day90th"] <- quantile(subset_3day$vwcb_30cm_Avg, 
                                                        0.9, na.rm = TRUE)
        }
        if(sensor_name == 15) {
            output_df[d, "soil_sensor"] <- "a"
            output_df[d, "sm5cm_daily_mean"] <- mean(subset_day$vwca_5cm_Avg, na.rm = TRUE)
            output_df[d, "sm30cm_daily_mean"] <- mean(subset_day$vwca_30cm_Avg, na.rm = TRUE)
            output_df[d, "sm5cm_3day90th"] <- quantile(subset_3day$vwca_5cm_Avg, 0.9, na.rm = TRUE)
            output_df[d, "sm30cm_3day90th"] <- quantile(subset_3day$vwca_30cm_Avg, 0.9, na.rm = TRUE)
        }
        if(sensor_name == 21) {
            output_df[d, "soil_sensor"] <- "c"
            output_df[d, "sm5cm_daily_mean"] <- mean(subset_day$vwcc_5cm_Avg, na.rm = TRUE)
            output_df[d, "sm30cm_daily_mean"] <- mean(subset_day$vwcc_30cm_Avg, na.rm = TRUE)
            output_df[d, "sm5cm_3day90th"] <- quantile(subset_3day$vwcc_5cm_Avg, 0.9, na.rm = TRUE)
            output_df[d, "sm30cm_3day90th"] <- quantile(subset_3day$vwcc_30cm_Avg, 0.9, na.rm = TRUE)
        }
        # calculate temp and snowdepth
        output_df[d, "stemp5_daily_mean"] <- mean(subset_day$soiltemp_5cm_Avg, na.rm = TRUE)
        output_df[d, "stemp30_daily_mean"] <-mean(subset_day$soiltemp_30cm_Avg, na.rm = TRUE)
        output_df[d, "airtemp_daily_mean"] <- mean(subset_day$airtemp_Avg, na.rm = TRUE)
        output_df[d, "airtemp_daily_min"] <- min(subset_day$airtemp_Avg, na.omit = TRUE)        
        output_df[d, "snowdepth_daily_mean"] <- mean(subset_day$snowdepth, na.rm = TRUE)
        output_df[d, "stemp5_3day90th"] <- quantile(subset_3day$soiltemp_5cm_Avg, 0.9, na.rm = TRUE)
        output_df[d, "stemp30_3day90th"] <- quantile(subset_3day$soiltemp_30cm_Avg, 0.9, na.rm = TRUE)
        output_df[d, "snowdepth_3day90th"] <- quantile(subset_3day$snowdepth, 0.9, na.rm = TRUE)

        output_df[d, "air_3day90th"] <- quantile(subset_3day$airtemp_Avg, 0.9, na.rm = TRUE)
    }
    output_df <- output_df[-c(1), ]
    sensor_number_2 <- ifelse(nchar(sensor_name) == 1, paste0("0", sensor_name), sensor_name)
    assign(paste0("sensor", as.character(sensor_number_2), "_3day90th"), output_df, envir = globalenv())
}


