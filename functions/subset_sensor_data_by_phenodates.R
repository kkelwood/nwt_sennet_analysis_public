# subset by phenodates

# practice variables - delete before using
# df1 <- sennet_data
# df2 <- phenodates50_2018
# x <- plot_list
# year = 2018
# i <- plot_list[1]

subset_by_phenodate <- function(df1, df2, x, year){
    # df1 = the df to be subset
    # df2 = the df with the dates determining the subset
    # x = list of plots/phenocams
    # year
    
    output_df <- data.frame()
    for(i in x) {
        plot <- i
        df1_ploti <- subset(df1, subset = df1$phenocam == i)
        df1_ploti_2 <- subset(df1_ploti, subset = df1_ploti$doy >= df2$Greenup[which(df2$phenocam == i)])
        df1_ploti_3 <- subset(df1_ploti_2, subset = df1_ploti_2$doy <= df2$Dormancy[which(df2$phenocam == i)])        
        output_df <- rbind(output_df, df1_ploti_3)
    }
    assign("sennet_subset_by_phenodate", output_df, envir = globalenv())
}
