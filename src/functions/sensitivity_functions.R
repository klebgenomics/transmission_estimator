library(tidyverse)
library(ggplot2)
library(future)


get_cluster_and_transmission_fraction <- function(snp_and_epi_data, metadata, 
                                                  snp_distance_threshold, 
                                                  temporal_distance_threshold){
    epi_snp_graph <- get_cluster_graph(snp_and_epi_data,
                                       dist_column = c('dist', 'days'),
                                       pair_location_column = "pair_location",
                                       dist_threshold = c(snp_distance_threshold,
                                                          temporal_distance_threshold))
    epi_snp_clusters <- get_cluster_membership_from_graph(epi_snp_graph) %>% 
        dplyr::right_join(metadata, by = 'id') # contains rows for ALL samples
    cluster_proportion <- calculate_cluster_proportion(epi_snp_clusters)
    transmission_proportion <- calc_prop_samples_due_to_transmission(epi_snp_clusters)
    
    return(list(
        "snp_threshold" = snp_distance_threshold,
        "temporal_threshold" = temporal_distance_threshold,
        "cluster_prop" = cluster_proportion,
        "transmission_prop" = transmission_proportion
    ))
}


get_cluster_sensitivity <- function(snp_and_epi_data, metadata, 
                                     snp_range=c(1:25),
                                     date_range=c(1:52)) {
    # set multithreading plan
    if (parallelly::supportsMulticore()){
        future::plan(future::multicore(), workers = parallelly::availableCores())
    } else {
        future::plan(future::multisession(), workers = parallelly::availableCores())
    }
    # get all snp-date pairs; change weeks to days for cluster function
    sensitivity <- tidyr::expand_grid(snp_range, date_range*7) |> 
        (\(z) dplyr::bind_rows(
            furrr::future_map2(
                z[[1]], z[[2]],
                ~get_cluster_and_transmission_fraction(snp_and_epi_data, metadata, .x, .y)
            )
        ))()
    future::plan("default")
    
    return(sensitivity)
}



