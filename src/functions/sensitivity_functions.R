library(tidyverse)
library(ggplot2)
library(furrr)
library(future)
library(parallelly)


get_cluster_and_transmission_fraction <- function(snp_and_epi_data, metadata, 
                                                  snp_distance_threshold, 
                                                  temporal_distance_threshold){
    epi_snp_graph <- get_cluster_graph(snp_and_epi_data,
                                       dist_column = c('dist', 'weeks'),
                                       pair_location_column = "pair_location",
                                       dist_threshold = c(snp_distance_threshold,
                                                          temporal_distance_threshold))
    epi_snp_clusters <- get_cluster_membership_from_graph(epi_snp_graph) %>% 
        dplyr::right_join(metadata, by = 'id') # contains rows for ALL samples
    estimates <- get_cluster_estimates(epi_snp_clusters)
    
    return(list(
        "distance_threshold" = snp_distance_threshold,
        "temporal_threshold" = temporal_distance_threshold,
        "cluster_prop" = estimates$cluster_prop,
        "transmission_prop" = estimates$transmission_prop
    ))
}


get_cluster_sensitivity <- function(snp_and_epi_data, metadata, 
                                    snp_range = 1:25, 
                                    date_range = 1:52) {
    grid <- tidyr::expand_grid(snp_range, date_range)
    sensitivity <- dplyr::bind_rows(
        mapply(get_cluster_and_transmission_fraction, 
               MoreArgs = list(snp_and_epi_data=snp_and_epi_data, metadata=metadata), 
               snp_distance_threshold=grid[[1]], 
               temporal_distance_threshold=grid[[2]], 
               SIMPLIFY = FALSE)
    )
    return(sensitivity)
}



