library(tidyverse)
library(igraph)



#' Create a graph from table with distances and a geographic column 
#' @param distance_data A tibble with columns: 'iso1', 'iso2', 
#' and one or more columns containing distances (e.g., SNPs)
#' between the isolates in the 'iso1' and 'iso2' columns
#' @param dist_columns Vector of names of each of the columns containing distances 
#' e.g., dist_columns = c('SNPs', 'weeks')
#' @param dist_thresholds Vector of distance thresholds [int] to be applied for each distance column
#' Length of this vector must be equal to the length of dist_columns
#' e.g., dist_threshold = c(10, 14)
#' @param pair_location_column Name of the geographic column. Column must contain either of the
#' values "Same" or "Different" representing isolate pairs from the same or different locations
#' @export
get_cluster_graph <- function(snp_and_epi_data, dist_columns, dist_thresholds, 
                              pair_location_column = "pair_location", directed=FALSE) {
    if (length(dist_columns) != length(dist_thresholds)) {
        stop("The lengths of vars `dist_columns` and `dist_thresholds` must be equal")
    }
    if(!all(dist_columns %in% names(snp_and_epi_data))){
        stop('One or more specified columns missing from snp_and_epi_data')
    }
    if(!pair_location_column %in% names(snp_and_epi_data)){
        stop(glue::glue("The specified '{pair_location_column}' column is missing from snp_and_epi_data"))
    }
    if(!tibble::is_tibble(snp_and_epi_data)){stop('snp_and_epi_data is not a tibble')}
    # filter out pairs based on the threshold for each distance column
    for (i in seq(1:length(dist_columns))) {
        snp_and_epi_data %<>% 
            dplyr::filter(!!rlang::sym(dist_columns[i]) <= dist_thresholds[i]) %>% 
            dplyr::select(-c(!!rlang::sym(dist_columns[i]))) 
    }
    # filter out pairs with same isolate 
    # filter out pairs belonging to the same geo location
    # generate graph
    snp_and_epi_data %>% 
        dplyr::filter(! iso1 == iso2) %>% 
        dplyr::filter(!!rlang::sym(pair_location_column) == "Same") %>% 
        dplyr::select(c(iso1, iso2)) %>% 
        as.matrix() %>% 
        igraph::graph_from_edgelist(., directed = directed)
}



# adapted from KleborateR function; removed dependency on kleborate data
get_cluster_membership_from_graph <- function(cluster_graph) {
    if(!igraph::is_igraph(cluster_graph)){stop('cluster_graph is not an igraph')}
    igraph::components(cluster_graph)$membership %>%
        tibble::as_tibble(rownames = "id") %>%
        dplyr::mutate(Cluster = as.character(value)) %>%
        dplyr::select(!value)
}

calculate_cluster_proportion <- function(clusters_data){
    clusters_data %>% 
        dplyr::summarise(
            cluster_proportion = round(sum(!is.na(Cluster)) / n(), 2)
        ) %>% dplyr::pull(cluster_proportion) %>% round(digits = 2)
}

calc_prop_samples_due_to_transmission <- function(clusters_data){
    clusters_data %>% dplyr::select(id, Cluster) %>% 
        dplyr::filter(!is.na(Cluster)) %>% 
        # count isolates per cluster; remove 1 (index case)
        dplyr::group_by(Cluster) %>% 
        dplyr::summarise(nc = sum(!is.na(Cluster)) - 1) %>% 
        dplyr::ungroup() %>% 
        dplyr::summarise(prop = round(sum(nc) / nrow(clusters_data), digits = 2)) %>% 
        dplyr::pull(prop) %>% round(digits = 2)
}



summarise_cluster <- function(clusters_data) {
    summary <- tibble::tribble(
        ~name, ~value,
        'Total isolates', n_distinct(clusters_data$id),
        'Clusters', n_distinct(clusters_data$Cluster, na.rm = T),
        'N isolates in clusters', clusters_data %>% filter(!is.na(Cluster)) %>% nrow(),
        'Median cluster size', clusters_data %>% filter(!is.na(Cluster)) %>% 
            dplyr::add_count(Cluster, name = "cluster_size") %>% 
            dplyr::pull(cluster_size) %>% median() %>% round(digits = 0),
        'Cluster STs', clusters_data %>% dplyr::filter(!is.na(Cluster)) %>% 
            pull(ST) %>% n_distinct(na.rm = T)
    )
    return(summary)
    
}

summarise_cluster2 <- function(clusters_data, snp_distance_threshold, temporal_distance_threshold) {
    summary <- tibble::tribble(
        ~name, ~value,
        "Prop in clusters", calculate_cluster_proportion(clusters_data),
        "Prop due to transmission", calc_prop_samples_due_to_transmission(clusters_data),
        'SNPs threshold used',  snp_distance_threshold,
        'Temporal distance threshold used (weeks)', temporal_distance_threshold,
    )
    return(
        dplyr::bind_rows(
            summarise_cluster(clusters_data),
            summary
        )
    )
}

get_cluster_info <- function(clusters_data){
    clusters_data %>% 
        dplyr::filter(!is.na(Cluster)) %>% 
        dplyr::group_by(Cluster) %>% 
        dplyr::reframe(Site = paste0(sort(unique(Site)), collapse = '; '),
                       "N isolates" = n(),
                       ST = paste0(sort(unique(ST)), collapse = '; '),
                       "Median resistance score" = median(resistance_score),
                       "Date first isolate" = min(formatted_date, na.rm = T),
                       "Date last isolate" = max(formatted_date, na.rm = T)
        ) %>% 
        dplyr::mutate(`Duration (weeks)` = abs(floor(as.numeric(
            difftime(`Date first isolate`, `Date last isolate`, units = "weeks")
        )))) %>% 
        dplyr::arrange(desc(`N isolates`), desc(`Duration (weeks)`)) %>% 
        dplyr::mutate(`Duration (weeks)` = if_else(
            `Duration (weeks)` %in% c(0,1), "<=1", as.character(`Duration (weeks)`)
        ))
}

cluster_stats_by_variable <- function(clusters_data, grouping_var = "Country", group_label = grouping_var){
    clust_prop <- calculate_cluster_proportion(clusters_data)
    transmission_prop <- calc_prop_samples_due_to_transmission(clusters_data)
    stats <- clusters_data %>% 
        dplyr::rename("Group" = all_of(grouping_var)) %>% 
        dplyr::group_by(Group) %>% 
        dplyr::reframe(`Cluster proportion` = calculate_cluster_proportion(dplyr::pick(tidyselect::everything())),
                       `Transmission proportion` = calc_prop_samples_due_to_transmission(dplyr::pick(tidyselect::everything())),
                       n_isolates = n()) %>% 
        dplyr::arrange(`Cluster proportion`)
    cols <- c("Transmission proportion" = "#2bbed8", "Cluster proportion" = "#0770b5")
    plot <- stats %>%
        dplyr::mutate(Group = factor(Group, levels = Group)) %>%
        tidyr::pivot_longer(-c(Group, n_isolates), names_to = "name", values_to = "value") %>% 
        dplyr::mutate(name = factor(name, levels = c('Transmission proportion','Cluster proportion'))) %>%
        ggplot2::ggplot(aes(x = Group, y = value, fill = name)) + 
        ggplot2::geom_bar(stat = 'identity', position = 'dodge') + 
        ggplot2::geom_hline(yintercept = transmission_prop, lty = 2, colour = cols[1]) +
        ggplot2::geom_hline(yintercept = clust_prop, lty = 2, colour = cols[2]) +
        ggplot2::scale_fill_manual(values = cols, name = NULL) +
        ggplot2::coord_flip(clip = 'off') + 
        ggplot2::geom_text(aes(label = paste("N=", n_isolates), hjust = 0, 
                               y = max(stats$`Cluster proportion`)+.1)) +
        scale_y_continuous(breaks = seq(0, 1, 0.2)) +
        ggplot2::theme_minimal() + 
        ggplot2::labs(x = group_label, y = "Proportion") +
        ggplot2::theme(axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14),
                       legend.text = element_text(size = 12),
                       legend.position = "bottom",
                       panel.border=element_blank(),
                       axis.line = element_line(),
                       panel.grid.major=element_blank(),
                       panel.grid.minor = element_blank())
    return(list("stats" = stats, "plot" = plot))
}


