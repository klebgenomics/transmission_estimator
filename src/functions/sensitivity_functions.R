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

plot_sensitivity_heatmap <- function(cluster_and_transmission_sensitivity_df, 
                                     prop_var = 'transmission_prop',
                                     plot_title = 'Transmission proportion at different thresholds'){
    cluster_and_transmission_sensitivity_df %>% 
        ggplot2::ggplot(aes(x = temporal_threshold, y = snp_threshold,
                            fill = !!rlang::sym(prop_var))) +
        ggplot2::geom_tile() +
        # viridis::scale_fill_viridis(limits = c(0, 1), discrete = F) +
        # ggplot2::scale_fill_gradient(low = "white", high = "blue", limits = c(0,1)) +
        ggplot2::scale_fill_gradient2(low = "#F7F7F7", mid = "#0571B0", high = "#CA0020", midpoint = 0.5,
                                      limits = c(0,1)) +
        ggplot2::labs(x = "Max temporal distance threshold", y = "Max SNP threshold",
                      fill = "Proportion", title = plot_title) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12)
        )
}

