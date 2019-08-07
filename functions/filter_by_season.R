# filter
library(lubridate)

# df <- sensor_06
JJA_only <- function(df) {
    temp <- subset(df, month(TIMESTAMP) >= 6 & month(TIMESTAMP) <= 8)
}
