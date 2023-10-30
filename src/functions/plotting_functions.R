library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(plotly)


plot_dist_distribution <- function(distance_data, dist_column, x_label = "Pairwise distance", 
                                   plot_title = NULL, scale_y = F, bins = 10,
                                   facet_plot = F, facet_column = NULL){
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
    distance_data %<>% dplyr::filter(!is.na(!!sym(dist_column))) 
    if(dist_column == "days") {
        distance_data$Weeks <- distance_data$days / 7
        dist_column <- "Weeks"
    } else if (dist_column == "dist") {
        distance_data %<>% dplyr::rename("SNPs" = "dist") 
        dist_column <- "SNPs"
    }
    
    # plot
    dist_plot <- ggplot(distance_data, aes(x = !!rlang::sym(dist_column))) +
        ggplot2::geom_histogram(binwidth = bins, color = "black", alpha = 0.8) +
        ggplot2::labs(title = plot_title, x = x_label, y = "Number of isolate pairs") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = element_text(size = 10),
                       axis.title = element_text(size = 12),
                       legend.text = element_text(size = 10),
                       legend.title = element_text(size = 12),
                       plot.title = element_text(size = 12, face = "bold")) +
    if (scale_y){
        dist_plot <- dist_plot + scale_y_log10(labels = scales::comma_format())
    }
    if (facet_plot){
        dist_plot <- dist_plot + 
            facet_wrap(~facet_column, nrow=2, scales="free_y")
    }
    return(dist_plot)
}


plot_snp_vs_temporal_dist <- function(distance_data, snp_column = 'dist', temporal_dist_column = 'days',
                                      max_snp_dist = 40, max_temporal_dist = 365,
                                      y_label = "Pairwise SNP distances", 
                                      x_label = "Pairwise temporal distances (days)",
                                      plot_title = "Distribution of pairwise distances"){
    if(! all(c(snp_column, temporal_dist_column) %in% colnames(distance_data)) ) {
        stop(glue::glue("{snp_column} and/or 'temporal_dist_column' columns not present in distance data"))
    }
    # filter
    distance_data %<>% dplyr::filter(!!rlang::sym(snp_column) <= max_snp_dist)
    distance_data %<>% dplyr::filter(!!rlang::sym(temporal_dist_column) <= max_temporal_dist)
    # plot
    p <- ggplot2::ggplot(distance_data, aes(y = !!rlang::sym(snp_column), 
                                            x = !!rlang::sym(temporal_dist_column))) +
        ggplot2::geom_point(color="#808080") +
        ggplot2::theme(legend.position="none") +
        ggplot2::labs(x = x_label, y = y_label, title = plot_title) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = element_text(size = 10),
                       axis.title = element_text(size = 12),
                       legend.text = element_text(size = 10),
                       legend.title = element_text(size = 12),
                       plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
    # corr
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#2a77be")
    # add marginal plots; does not work with plotly
    # p <- ggExtra::ggMarginal(p, type = "density", fill = "#2a77be")
    return(p)
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


plot_clusters <- function(clusters_data, min_cluster_size = 3) {
    # filter
    clusters_data %<>% 
        dplyr::filter(!is.na(Cluster)) %>% 
        dplyr::add_count(Cluster, name = "cluster_size") %>% 
        dplyr::filter(cluster_size >= min_cluster_size)
    # Calculate point sizes based on the number of isolates on each date per cluster
    size_scale_factor <- clusters_data %>%
        dplyr::group_by(Cluster, formatted_date) %>%
        dplyr::reframe(Cases = n())
    # merge and order
    clusters_data %<>% dplyr::left_join(size_scale_factor, by = c("Cluster", "formatted_date")) %>% 
        dplyr::rename("Date" = "formatted_date") %>% 
        dplyr::mutate(Cluster = fct_reorder(Cluster, desc(ST)))

    # plot
    plot <- clusters_data %>% ggplot(aes(x = Date, y = Cluster,
                                         size = Cases, fill = factor(ST), color = factor(ST))) +
        ggplot2::geom_point() +
        ggplot2::scale_size_continuous(name = "Cases") +
        ggplot2::geom_line(linewidth = 0.8) +
        viridis::scale_fill_viridis(discrete = TRUE) +
        viridis::scale_color_viridis(discrete = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Isolation date", y = "Cluster", color = "Sequence type") +
        ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), 
                              breaks = scales::breaks_pretty(n = 6)) +
        ggplot2::theme(
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12)
        ) +
        ggplot2::guides(fill = "none", colour = "none")
    
    return(plot)
}

plot_clusters2 <- function(clusters_data, min_cluster_size = 2, color_column = 'ST') {
    # filter
    clusters_data %<>% 
        dplyr::filter(!is.na(Cluster)) %>% 
        dplyr::add_count(Cluster, name = "cluster_size") %>% 
        dplyr::filter(cluster_size >= min_cluster_size)
    # Calculate point sizes based on the number of isolates on each date per cluster
    size_scale_factor <- clusters_data %>%
        dplyr::group_by(Cluster, formatted_date) %>%
        dplyr::reframe(Cases = n())
    # merge and order
    clusters_data %<>% dplyr::left_join(size_scale_factor, by = c("Cluster", "formatted_date")) %>% 
        dplyr::rename("Date" = "formatted_date") %>% 
        dplyr::mutate(Cluster = fct_reorder(Cluster, desc(ST)))
    
    # plot
    plot <- clusters_data %>% 
        ggplot(aes(x = Date, y = ST, group = Cluster)) +
        ggplot2::geom_point(aes(size = Cases, color = !!sym(color_column)),
                            position=ggstance::position_dodgev(height = 1)) +
        ggplot2::scale_size_continuous(name = "Cases") +
        ggplot2::geom_line(aes(group = Cluster, alpha = 0.25), color = "grey50", linewidth = 0.8, 
                           position=ggstance::position_dodgev(height = 1)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Isolation date", y = "Sequence type") +
        ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), 
                              breaks = scales::breaks_pretty(n = 6)) +
        ggplot2::theme(
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12)
        ) +
        ggplot2::guides(fill = "none", colour = "none", alpha = "none")
    
    return(plot)
}

