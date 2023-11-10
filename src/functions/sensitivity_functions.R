library(tidyverse)
library(ggplot2)

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
        "cluster_prop" = cluster_proportion,
        "transmission_prop" = transmission_proportion
    ))
    
}

get_cluster_sensitivity <- function(snp_and_epi_data, metadata, 
                                snp_range=c(1:25),
                                date_range=7*c(1:52)) {
    date_range = date_range*7 # change weeks to days for cluster function
    i <- 1
    results <- vector("list", length(snp_range) * length(date_range))
    for (snps in snp_range){
        for (days in date_range) {
            cluster_and_transmission_prop <- get_cluster_and_transmission_fraction(
                snp_and_epi_data, metadata,
                snp_distance_threshold = snps, temporal_distance_threshold = days
            )
            # Append the results
            results[[i]] <- c(
                snp_threshold = snps,
                temporal_threshold = days,
                cluster_prop = cluster_and_transmission_prop$cluster_prop,
                transmission_prop = cluster_and_transmission_prop$transmission_prop
            )
            i <- i + 1
        }
    } # End loop
    df <- dplyr::bind_rows(results)
    return(df)
}

