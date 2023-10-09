library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(plotly)


plot_dist_distribution <- function(distance_data, dist_column, x_label = "Pairwise distance", 
                                   plot_title = "Distribution of pairwise distances",
                                   scale_y = F, facet_plot = F, facet_column = NULL){
    if(! all(c('iso1', 'iso2') %in% colnames(distance_data)) ) {
        stop("'iso1' and 'iso2' columns required")
    }
    if(! dist_column %in% colnames(distance_data) ) {
        stop(glue::glue("'{dist_column}' column missing from distance_data"))
    }
    if(facet_plot){
        if(is.null(facet_column)){
            stop("'facet_plot' set to TRUE but facet column not provided")
        }
        if(! facet_column %in% colnames(distance_data) ) {
            stop(glue::glue("'{facet_column}' column missing from distance_data"))
        }
    }
    # plot
    dist_plot <- ggplot(distance_data, aes(x = !!rlang::sym(dist_column))) +
        ggplot2::geom_histogram(binwidth = 10, color = "black", alpha = 0.8) +
        ggplot2::labs(title = plot_title, x = x_label, y = "Number of isolate pairs") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = element_text(size = 14)) +
        ggplot2::theme(axis.title = element_text(size = 14)) +
        ggplot2::theme(legend.text = element_text(size = 14)) +
        ggplot2::theme(plot.title = element_text(size = 14, face = "bold")) +
    if (scale_y){
        dist_plot <- dist_plot + scale_y_log10(labels = scales::comma_format())
    }
    if (facet_plot){
        dist_plot <- dist_plot + 
            facet_wrap(~pair_location, nrow=2, scales="free_y")
    }
    return(dist_plot)
}


plot_snp_vs_temporal_dist <- function(distance_data, snp_column = 'dist', temporal_dist_column = 'days', 
                                      y_label = "Pairwise SNP distances", 
                                      x_label = "Pairwise temporal distances (days)",
                                      plot_title = "Distribution of pairwise distances"){
    if(! all(c(snp_column, temporal_dist_column) %in% colnames(distance_data)) ) {
        stop(glue::glue("{snp_column} and/or 'temporal_dist_column' columns not present in distance data"))
    }
    # plot
    p <- ggplot2::ggplot(distance_data, aes(y = !!rlang::sym(snp_column), 
                                            x = !!rlang::sym(temporal_dist_column))) +
        ggplot2::geom_point(color="#808080") +
        ggplot2::theme(legend.position="none") +
        ggplot2::labs(x = x_label, y = y_label, title = plot_title) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = element_text(size = 14)) +
        ggplot2::theme(axis.title = element_text(size = 14)) +
        ggplot2::theme(legend.title = element_text(size = 14)) +
        ggplot2::theme(legend.text = element_text(size = 14)) +
        ggplot2::theme(plot.title = element_text(size = 14, face = "bold")) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#2a77be")
    snp_vs_temporal_dist_plot <- ggExtra::ggMarginal(p, type = "density", fill = "#2a77be")
    return(snp_vs_temporal_dist_plot)
}


plot_transmission_network <- function(snp_graph, kleborate_data, var1) {
    snp_graph %>% 
        dplyr::left_join(
            kleborate_data %>% dplyr::select(all_of(c('Genome Name', var1))),
            by=c('name'='Genome Name')) %>%
        ggplot2::ggplot(aes(x=x, y=y, xend=xend, yend=yend, label=name)) +
        ggnetwork::geom_edges() +
        ggnetwork::geom_nodes(
            aes(col=.data[[var1]]), 
            size=3.5, shape=16, alpha=.5
        ) +
        ggnetwork::theme_blank() +
        ggplot2::guides(col="none", fill="none")
}


# TO DO. Add options for customising plot
plot_clusters <- function(clusters_data, min_cluster_size = 3) {
    # filter
    clusters_data %<>% 
        dplyr::filter(!is.na(Cluster)) %>% 
        dplyr::add_count(Cluster, name = "cluster_size") %>% 
        dplyr::filter(cluster_size >= min_cluster_size)
    # Calculate point sizes based on the number of isolates on each date per cluster
    size_scale_factor <- clusters_data %>%
        dplyr::group_by(Cluster, formatted_date) %>%
        dplyr::reframe(size_scale = n())
    # merge
    clusters_data %<>% dplyr::left_join(size_scale_factor, by = c("Cluster", "formatted_date"))
    # plot
    ggpubr::ggscatter(
        clusters_data, x = "formatted_date", y = "ST", 
        color = "Cluster", palette = "viridis", ellipse = TRUE, ellipse.type = "convex",
        # shape = "Country",
        size = clusters_data$size_scale,  # Use the size scaling factor
        legend = "right", ggtheme = theme_bw(), 
        ylab = "ST",
        xlab = "Isolation date"
    ) + ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = "4 weeks") +
        ggplot2::theme(axis.text = element_text(size = 14)) +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggplot2::theme(axis.title = element_text(size = 14)) +
        ggplot2::theme(legend.text = element_text(size = 14))
}

