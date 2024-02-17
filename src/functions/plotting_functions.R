library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(plotly)

custom_plots_theme <- ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 14),
    plot.title = element_text(size = 14)
)

rename_vars_for_plotting <- function(d, column){
    virulence_score_names <- c(
        '0'='0: None',
        '1'='1: ybt',
        '2'='2: ybt + clb',
        '3'='3: iuc (VP)',
        '4'='4: ybt + iuc (VP)',
        '5'='5: ybt + clb + iuc (VP)'
    )
    resistance_score_names <- c(
        '0'='0: ESBL-, Carb-',
        '1'='1: ESBL+, Carb-',
        '2'='2: Carb+',
        '3'='3: Carb+, Col+'
    )
    if (column == 'resistance_score'){
        d %<>% dplyr::mutate(resistance_score = dplyr::case_when(
            resistance_score %in% names(resistance_score_names) ~ resistance_score_names[as.character(resistance_score)],
            is.na(resistance_score) ~ NA,
            TRUE ~ as.character(resistance_score))
        ) 
        
    } else if (column == 'virulence_score'){
        d %<>% dplyr::mutate(virulence_score =  dplyr::case_when(
            virulence_score %in% names(virulence_score_names) ~ virulence_score_names[as.character(virulence_score)],
            is.na(virulence_score) ~ NA,
            TRUE ~ as.character(virulence_score))
        ) 
        
    }
    return(d)
}

plot_dist_distribution <- function(distance_data, dist_column, x_label = "Pairwise distance", 
                                   plot_title = NULL, binwidth = 10, transform_y = F, 
                                   transformation = NULL){
    if(! all(c('iso1', 'iso2') %in% colnames(distance_data)) ) {
        stop("'iso1' and 'iso2' columns required")
    }
    if(! dist_column %in% colnames(distance_data) ) {
        stop(glue::glue("'{dist_column}' column missing from distance_data"))
    }
    if(transform_y) {
        if (is.null(transformation) || !transformation %in% c('log2', 'log10', 'sqrt') ) {
            stop(glue::glue("`transformation` must be one of 'log2', 'log10', or 'sqrt'"))
        }
    }
    
    distance_data %<>% dplyr::filter(!is.na(!!sym(dist_column))) 
    if(dist_column == "weeks") {
        distance_data %<>% dplyr::rename("Weeks" = "weeks") 
        dist_column <- "Weeks"
    } else if (dist_column == "dist") {
        distance_data %<>% dplyr::rename("SNPs" = "dist") 
        dist_column <- "SNPs"
    }
    # plot
    dist_plot <- ggplot(distance_data, aes(x = !!rlang::sym(dist_column))) +
        ggplot2::geom_histogram(binwidth = binwidth, color = "black", alpha = 0.8) +
        ggplot2::labs(title = plot_title, x = x_label, y = "Number of isolate pairs") +
        ggplot2::theme_minimal() +
        custom_plots_theme +
        ggplot2::theme(plot.title = element_text(face = "bold", size = 12),
                       axis.text = ggplot2::element_text(size = 10),
                       axis.title = ggplot2::element_text(size = 12))
    if (transform_y){
        dist_plot <- dist_plot + 
            ggplot2::scale_y_continuous(trans=transformation, 
                                        labels = scales::comma_format()) +
            ggplot2::labs(y = glue::glue('Number of isolate pairs ({transformation})'))
    }
    return(dist_plot)
}


plot_snp_vs_temporal_dist <- function(distance_data, snp_column = 'dist', temporal_dist_column = 'weeks',
                                      max_snp_dist = 40, max_temporal_dist = 52,
                                      y_label = "Pairwise SNP distances", 
                                      x_label = "Pairwise temporal distances (weeks)",
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
        custom_plots_theme +
        ggplot2::theme(plot.title = element_text(face = "bold", hjust = 0.5))
    # corr
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "#2a77be")
    # add marginal plots; does not work with plotly
    # p <- ggExtra::ggMarginal(p, type = "density", fill = "#2a77be")
    return(p)
}

#### Clusters -------------

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
        custom_plots_theme +
        ggplot2::theme(
            axis.text.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
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
        dplyr::select(-any_of("Date")) %>% # remove Date column if exists
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
        custom_plots_theme +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggplot2::guides(fill = "none", colour = "none", alpha = "none")
    
    return(plot)
}

#### Sensitivity -------------

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
        temp_dist_vals = c(1, 2, 4, 8, 12),
        prop_var = 'cluster_prop', y_title = "Proportion in clusters",
        plot_title = NULL){
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", temp_dist_vals, " weeks threshold") 
    # wrangle and plot
    cluster_and_transmission_sensitivity_df %>% 
        dplyr::filter(temporal_threshold %in% temp_dist_vals) %>% 
        unique() %>% 
        dplyr::select(snp_threshold, temporal_threshold, !!rlang::sym(prop_var)) %>% 
        tidyr::pivot_wider(id_cols = snp_threshold, 
                           names_from = temporal_threshold, values_from = !!rlang::sym(prop_var)) %>% 
        dplyr::rename_at(vars(as.character(temp_dist_vals)), 
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

#### Comparisons -------------

plot_comparisons <- function(
        comparison_data, snp_val, temp_dist_vals, comparison_var = "comparison_group",
        prop_var = 'cluster_prop', y_title = "Proportion in clusters", 
        plot_title = NULL){
    
    if(length(snp_val) != 1){stop("Exactly one snp value required")}
    if(length(temp_dist_vals) != 5){stop("Exactly five temporal dist values (weeks) required")}
    
    # Rename comparison group
    d <- comparison_data %>% dplyr::rename("Group" := !!rlang::sym(comparison_var))
    
    # subset and delineate public data if included 
    d_pub <- d %>% filter(data_source == "Preloaded public data")
    if (nrow(d_pub) > 0) {
        d <- d %>% dplyr::mutate(Group = dplyr::if_else(
            data_source == "Preloaded public data", paste0(study_SN, " - ", Group), Group
        ))
    }

    # arrange sites by prop_var at midpoint temp_dist
    ordered_d <- d %>% 
        dplyr::filter(temporal_threshold == temp_dist_vals[3]) %>% 
        dplyr::arrange(data_source, desc(.data[[prop_var]]))
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", temp_dist_vals, " week(s) threshold") 
    # plot data
    d <- d %>% 
        dplyr::filter(snp_threshold == snp_val & temporal_threshold %in% temp_dist_vals) %>% 
        dplyr::select(snp_threshold, temporal_threshold, data_source, Group, all_of(c(prop_var))) %>% 
        dplyr::mutate(Group = factor(Group, levels = ordered_d$Group)) %>% 
        tidyr::pivot_wider(id_cols = c(snp_threshold, data_source, Group), 
                           names_from = temporal_threshold, values_from = .data[[prop_var]]) %>% 
        dplyr::rename_at(vars(as.character(temp_dist_vals)), 
                         ~tidyselect::all_of(y_vars)) 
    
    d_pub <- d %>% filter(data_source == "Preloaded public data")
    
    # plot
    p <- d %>% 
        ggplot2::ggplot(aes(x = Group)) +
        # Estimates - User sites
        ggplot2::geom_linerange(aes(ymin = .data[[y_vars[1]]], ymax = .data[[y_vars[5]]]), 
                                col = "#ffc1c1", lwd = 4) +
        ggplot2::geom_linerange(aes(ymin = .data[[y_vars[2]]], ymax = .data[[y_vars[4]]]), 
                                col = "#8b0000", lwd = 2.5) +
        ggplot2::geom_point(aes(y = .data[[y_vars[3]]]), shape = 22, color = "#8b0000", fill = "white", size = 2)
    
    if (nrow(d_pub) > 0) {
        p <- p + 
            # Estimates - preloaded data, if loaded 
            ggplot2::geom_linerange(data = d_pub, 
                                    aes(ymin = .data[[y_vars[1]]], ymax = .data[[y_vars[5]]]),
                                    col = "lightblue", lwd = 4) +
            ggplot2::geom_linerange(data = d_pub,
                                    aes(ymin = .data[[y_vars[2]]], ymax = .data[[y_vars[4]]]), 
                                    col = "navyblue", lwd = 2.5) +
            ggplot2::geom_point(data = d_pub,
                                aes(y = .data[[y_vars[3]]]), 
                                shape = 22, color = "navyblue", fill = "white", size = 2)
    }
    # Finish plot
    p <- p + ggplot2::theme_minimal() + ggplot2::ylim(0, 1) + 
        ggplot2::labs(x = NULL, y = y_title) +
        custom_plots_theme +
        ggplot2::theme(plot.title = element_text(face = 'bold', size = 12),
                       axis.text = element_text(size = 10),
                       axis.title = element_text(size = 12)) +
        ggplot2::coord_flip() +
        ggplot2::ggtitle(plot_title)
    
    return(p)
}

