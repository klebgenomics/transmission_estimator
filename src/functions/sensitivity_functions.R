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
    # init empty df
    df <- data.frame(
        snp_threshold = numeric(0),
        temporal_threshold = numeric(0),
        cluster_prop = numeric(0),
        transmission_prop = numeric(0)
    )
    for (snps in snp_range){
        for (days in date_range) {
            cluster_and_transmission_prop <- get_cluster_and_transmission_fraction(
                snp_and_epi_data, metadata,
                snp_distance_threshold = snps, temporal_distance_threshold = days
            )
            cluster_prop <- cluster_and_transmission_prop$cluster_prop
            transmission_prop <- cluster_and_transmission_prop$transmission_prop
            # Append the results
            df <- rbind(df, data.frame(
                snp_threshold = snps,
                temporal_threshold = days,
                cluster_prop = cluster_prop,
                transmission_prop = transmission_prop
            ))
        }
    } # End loop
    return(df)
}

custom_plots_theme <- ggplot2::theme(
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 12),
    plot.title = element_text(size = 12)
)

plot_sensitivity_linegraph <- function(cluster_and_transmission_sensitivity_df,
                                       y_var="cluster_prop", x_var = "snp_threshold",
                                       color_var="temporal_threshold", color_title = "TempDist (weeks)",
                                       x_title="SNP threshold", y_title="Cluster proportion",
                                       plot_title="Cluster proportion at different temporal dist thresholds"){
    cluster_and_transmission_sensitivity_df %>% 
        ggplot2::ggplot(aes(x=!!sym(x_var), y=!!sym(y_var),
                            colour=as.factor(!!sym(color_var)))) + 
        ggplot2::geom_line() +
        viridis::scale_color_viridis(discrete = T) +
        ggplot2::theme_minimal() +
        ggplot2::labs(colour = color_title, x = x_title, y = y_title) +
        custom_plots_theme
}

plot_sensitivity_heatmap <- function(cluster_and_transmission_sensitivity_df, 
                                     prop_var = 'transmission_prop',
                                     plot_title = 'Transmission proportion at different thresholds'){
    mid_range <- mean(c(max(cluster_and_transmission_sensitivity_df[[prop_var]]), 
                      min(cluster_and_transmission_sensitivity_df[[prop_var]])) )
    cluster_and_transmission_sensitivity_df %>% 
        ggplot2::ggplot(aes(x = temporal_threshold, y = snp_threshold,
                            fill = !!rlang::sym(prop_var))) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(low = "#0571B0", mid = "#F7F7F7", high = "#CA0020", 
                                      midpoint = mid_range, space = "Lab") +
        ggplot2::labs(x = "Max temporal distance threshold (weeks)", y = "Max SNP threshold",
                      fill = "Proportion", title = plot_title) +
        ggplot2::theme_minimal() +
        custom_plots_theme
}

plot_sensitivity_SNP_vs_temp_range <- function(
        cluster_and_transmission_sensitivity_df,
        temp_dist_range_vals = c(1, 2, 4, 8, 12),
        prop_var = 'cluster_prop', y_title = "Proportion in clusters",
        plot_title = NULL){
    # rescale days to weeks
    cluster_and_transmission_sensitivity_df %<>% 
        dplyr::mutate(temporal_threshold = temporal_threshold / 7)
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", temp_dist_range_vals, " weeks threshold") 
    # wrangle and plot
    cluster_and_transmission_sensitivity_df %>% 
        dplyr::filter(temporal_threshold %in% temp_dist_range_vals) %>% 
        unique() %>% 
        dplyr::select(snp_threshold, temporal_threshold, !!rlang::sym(prop_var)) %>% 
        tidyr::pivot_wider(id_cols = snp_threshold, 
                           names_from = temporal_threshold, values_from = !!rlang::sym(prop_var)) %>% 
        dplyr::rename_at(vars(as.character(temp_dist_range_vals)), 
                         ~tidyselect::all_of(y_vars)) %>% 
        ggplot2::ggplot(aes(x = snp_threshold)) +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[2]]], ymax = .data[[y_vars[4]]], 
                                 x = snp_threshold), fill = "#8b0000") +
        ggplot2::geom_line(aes(y = .data[[y_vars[3]]]), colour = "white") +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[1]]], ymax = .data[[y_vars[2]]]), 
                             fill = "#ffc1c1", alpha = 0.75) +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[4]]], ymax = .data[[y_vars[5]]]), 
                           fill = "#ffc1c1", alpha = 0.75) +
        ggplot2::theme_minimal() + ggplot2::ylim(0, 1) + 
        ggplot2::labs(x = "SNP threshold", y = y_title, title = plot_title) +
        custom_plots_theme 
}

plot_sensitivity_temp_dist_vs_snp_range <- function(
        cluster_and_transmission_sensitivity_df,
        snp_range_vals = c(2, 5, 10, 20, 25),
        prop_var = 'cluster_prop', y_title = "Proportion in clusters",
        plot_title = NULL){
    # rescale days to weeks
    cluster_and_transmission_sensitivity_df %<>% 
        dplyr::mutate(temporal_threshold = temporal_threshold / 7)
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", snp_range_vals, " SNPs threshold")
    
    # wrangle and plot
    cluster_and_transmission_sensitivity_df %>% 
        dplyr::filter(snp_threshold %in% snp_range_vals) %>% 
        unique() %>% 
        dplyr::select(snp_threshold, temporal_threshold, !!rlang::sym(prop_var)) %>% 
        tidyr::pivot_wider(id_cols = temporal_threshold, 
                           names_from = snp_threshold, values_from = !!rlang::sym(prop_var)) %>% 
        dplyr::rename_at(vars(as.character(snp_range_vals)), 
                         ~tidyselect::all_of(y_vars)) %>% 
        ggplot2::ggplot(aes(x = temporal_threshold)) +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[2]]], ymax = .data[[y_vars[4]]], 
                                 x = temporal_threshold), fill = "#8b0000") +
        ggplot2::geom_line(aes(y = .data[[y_vars[3]]]), colour = "white") +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[1]]], ymax = .data[[y_vars[2]]]), 
                             fill = "#ffc1c1", alpha = 0.75) +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[4]]], ymax = .data[[y_vars[5]]]), 
                             fill = "#ffc1c1", alpha = 0.75) +
        ggplot2::theme_minimal() + ggplot2::ylim(0, 1) +
        ggplot2::labs(x = "Temporal distance threshold (weeks)", y = y_title,
                      title = plot_title) +
        custom_plots_theme
}

