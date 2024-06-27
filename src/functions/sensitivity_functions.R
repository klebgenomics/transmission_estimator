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
    
    return(list(
        "distance_threshold" = snp_distance_threshold,
        "temporal_threshold" = temporal_distance_threshold,
        "cluster_prop" = get_cluster_estimates(epi_snp_clusters)$cluster_prop,
        "transmission_prop" = get_cluster_estimates(epi_snp_clusters)$transmission_prop
    ))
}


get_cluster_sensitivity <- function(snp_and_epi_data, metadata, 
                                     snp_range=c(1:25),
                                     date_range=c(1:52)) {
    sensitivity <- tidyr::expand_grid(snp_range, date_range) |> 
        (\(z) dplyr::bind_rows(
            furrr::future_map2(
                z[[1]], z[[2]],
                ~get_cluster_and_transmission_fraction(snp_and_epi_data, metadata, .x, .y)
            )
        ))()
    return(sensitivity)
}



