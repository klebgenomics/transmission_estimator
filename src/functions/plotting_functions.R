library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(plotly)
library(Polychrome)

# Helpers
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

get_colours <- function(column_values, seedcolors = c("#ff0000", "#00ff00", "#0000ff")){
    if(length(unique(column_values)) > 3) {
        colours <- setNames(Polychrome::createPalette(length(unique(column_values)), 
                                                      seedcolors=seedcolors),
                            sort(unique(column_values)))
    } else {
        colours <- setNames(palette()[1:length(unique(column_values))],
                            sort(unique(column_values)))
    }
    return(colours)
}

calculate_plot_height <- function(num_y_vars, base_threshold=15, base_height=400, increment=10) {
    # Increment height for each var above the base_threshold
    max_height <- base_height + max(0, num_y_vars - base_threshold) * increment
    return(min(base_height + max_height, max_height))
}

# Distribution

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
        distance_data %<>% dplyr::rename("Distance" = "dist") 
        dist_column <- "Distance"
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


#### Clusters -------------

plot_transmission_network <- function(snp_graph, metadata, var1) {
    snp_graph %>% 
        dplyr::left_join(
            metadata %>% dplyr::select(all_of(c('id', var1))),
            by=c('name'='id')) %>%
        ggplot2::ggplot(aes(x=x, y=y, xend=xend, yend=yend, label=name)) +
        ggnetwork::geom_edges() +
        ggnetwork::geom_nodes(aes(col=.data[[var1]]), size=3.5, shape=16, alpha=.5) +
        ggnetwork::theme_blank() +
        ggplot2::guides(col="none", fill="none")
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
        dplyr::mutate(Cluster = fct_reorder(Cluster, desc(ST))) %>% 
        dplyr::mutate(text = glue::glue("Cluster {Cluster}\nCluster size: {cluster_size}\nDate: {Date}\nCases: {Cases}\nST: {ST}"))
    # colors
    my_colours <- get_colours(clusters_data[[color_column]], c("#0571B0", "#CA0020", "#FFBF00"))
    # plot
    plot <- clusters_data %>% 
        ggplot(aes(x = Date, y = ST, group = Cluster, text = text)) +
        ggplot2::geom_line(aes(group = Cluster, alpha = 0.25), color = "grey50", linewidth = 0.8, 
                           position=ggstance::position_dodgev(height = 1)) +
        ggplot2::geom_point(aes(size = Cases, color = !!sym(color_column)),
                            position=ggstance::position_dodgev(height = 1)) +
        ggplot2::scale_size_continuous(
            name = "Cases", breaks=function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1))))
        ) +
        scale_color_manual(values = my_colours) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Isolation date", y = "Sequence type") +
        ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), 
                              breaks = scales::breaks_pretty(n = 6)) +
        custom_plots_theme +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggplot2::guides(fill = "none", alpha = "none")
    
    return(plot)
}

### Sensitivity -------------

plot_sensitivity_SNP_vs_temp_range <- function(
        cluster_and_transmission_sensitivity_df,
        temp_dist_vals = c(1, 2, 4, 8, 12),
        prop_var = 'cluster_prop', y_title = "Proportion in clusters",
        plot_title = NULL){
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", temp_dist_vals, " week(s) threshold") 
    # wrangle and plot
    cluster_and_transmission_sensitivity_df %>% 
        dplyr::filter(temporal_threshold %in% temp_dist_vals) %>% 
        unique() %>% 
        dplyr::select(distance_threshold, temporal_threshold, !!sym(prop_var)) %>% 
        tidyr::pivot_wider(id_cols = distance_threshold, 
                           names_from = temporal_threshold, values_from = !!sym(prop_var)) %>% 
        dplyr::rename_at(vars(as.character(temp_dist_vals)), list(~y_vars)) %>% 
        ggplot2::ggplot(aes(x = distance_threshold)) +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[1]]], ymax = .data[[y_vars[5]]]), 
                             fill = "#ffc1c1") +
        ggplot2::geom_ribbon(aes(ymin = .data[[y_vars[2]]], ymax = .data[[y_vars[4]]], 
                                 x = distance_threshold), fill = "#8b0000") +
        ggplot2::geom_line(aes(y = .data[[y_vars[3]]]), colour = "white") +
        ggplot2::theme_minimal() + ggplot2::ylim(0, 1) + 
        ggplot2::labs(x = "Genetic distance threshold", y = y_title, title = plot_title) +
        custom_plots_theme 
}

#### Comparisons -------------

plot_comparisons <- function(
        comparison_data, snp_val, temp_dist_vals, comparison_var = "comparison_group",
        prop_var = 'cluster_prop', y_title = "Proportion in clusters", 
        plot_title = NULL){
    stopifnot(length(snp_val) == 1, length(temp_dist_vals) == 5)
    d <- comparison_data %>% dplyr::rename('Group' := !!sym(comparison_var))
    # arrange sites by prop_var at midpoint temp_dist
    ordered_d <- d %>% 
        dplyr::filter(temporal_threshold == temp_dist_vals[3]) %>%
        dplyr::mutate(pub_data_sn = as.numeric(str_extract(Group, "\\d+?(?= -)"))) %>% 
        dplyr::arrange(data_source, pub_data_sn, desc(.data[[prop_var]]))
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", temp_dist_vals, " week(s) threshold") 
    # plot data
    d_plot <- d %>% 
        dplyr::filter(distance_threshold == snp_val, temporal_threshold %in% temp_dist_vals) %>% 
        dplyr::select(distance_threshold, temporal_threshold, data_source, Group, all_of(c(prop_var))) %>% 
        dplyr::mutate(Group = factor(Group, levels = ordered_d$Group)) %>% 
        tidyr::pivot_wider(id_cols = c(distance_threshold, data_source, Group), 
                    names_from = temporal_threshold, values_from = all_of(c(prop_var))) %>% 
        dplyr::rename_at(vars(as.character(temp_dist_vals)), list(~y_vars)) 
    # plot
    p <- d_plot %>% ggplot(aes(x = Group)) +
        geom_linerange(aes(ymin=.data[[y_vars[1]]], ymax=.data[[y_vars[5]]]), col="#ffc1c1", lwd=4) +
        geom_linerange(aes(ymin=.data[[y_vars[2]]], ymax=.data[[y_vars[4]]]), col="#8b0000", lwd=2.5) +
        geom_point(aes(y=.data[[y_vars[3]]]), shape=22, color="#8b0000", fill="white", size=2)
    # Plot public data in diff colours if exist
    d_pub <- d_plot %>% filter(data_source == "Preloaded public data")
    if (nrow(d_pub) > 0) {
        p <- p + 
            geom_linerange(data=d_pub, aes(ymin=.data[[y_vars[1]]], ymax=.data[[y_vars[5]]]), col="lightblue", lwd=4) +
            geom_linerange(data=d_pub, aes(ymin=.data[[y_vars[2]]], ymax=.data[[y_vars[4]]]), col="navyblue", lwd=2.5) +
            geom_point(data=d_pub, aes(y=.data[[y_vars[3]]]), shape=22, color="navyblue", fill="white", size=2)
    }
    # Finish plot
    p <- p + ggplot2::theme_minimal() + ggplot2::ylim(0, 1) + 
        ggplot2::labs(x = NULL, y = y_title) +
        custom_plots_theme +
        ggplot2::theme(plot.title = element_text(size = 12),
                       axis.text = element_text(size = 10),
                       axis.title = element_text(size = 12)) +
        ggplot2::coord_flip() +
        ggplot2::ggtitle(plot_title)
    
    return(p)
}

