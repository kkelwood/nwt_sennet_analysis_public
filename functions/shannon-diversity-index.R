# Shannon-Diversity Index

# data = veg
# YEAR = 2017

shannon_div <- function(data, YEAR, plot_list) {
    subset_year <- subset(data, year == YEAR)
    non_species_list <- c("Litter", "Soil", "Rock", "Lichen", "Moss")
    temp <- subset(subset_year, sensor_node == plot_list[1])
    temp <- temp[ ! temp$species %in% non_species_list, ]
    N <- sum(temp$abundance)
    temp$p_i <- temp$abundance/N
    temp$p_i_ln <- log(temp$p_i)
    temp$H_i <- temp$p_i*temp$p_i_ln
    H <- sum(temp$H_i)*(-1)
    S <- length(unique(temp$species)) 
    E_H <- H/log(S)
    
    diversity_df <- temp %>% 
        select(plot_name, sensor_node, year, date_YYYY.MM.DD, multi.hit_or_top.hit) %>% 
        unique() %>% 
        mutate(diversity_shannon = H) %>% 
        mutate(shannon_equitability = E_H)
    
    for(i in plot_list[2:length(plot_list)]) {
        temp <- subset(subset_year, sensor_node == i)
        temp <- temp[ ! temp$species %in% non_species_list, ]
        N <- sum(temp$abundance)
        temp$p_i <- temp$abundance/N
        temp$p_i_ln <- log(temp$p_i)
        temp$H_i <- temp$p_i*temp$p_i_ln
        H <- sum(temp$H_i)*(-1)
        S <- length(unique(temp$species))
        E_H <- H/log(S)
        temp2 <- temp %>% 
            select(plot_name, sensor_node, year, date_YYYY.MM.DD, multi.hit_or_top.hit) %>% 
            unique() %>% 
            mutate(diversity_shannon = H) %>% 
            mutate(shannon_equitability = E_H)
        diversity_df <- rbind(diversity_df, temp2)
    }
    diversity_df <- unique(diversity_df)
    assign(paste0("diversity_", YEAR), diversity_df, envir = parent.frame())
}