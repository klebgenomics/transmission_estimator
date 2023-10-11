library(tidyverse)
library(igraph)



#' Create a graph from table with distances and a geographic column 
#' @param distance_data A tibble with columns: 'iso1', 'iso2', 
#' and one or more columns containing distances (e.g., SNPs)
#' between the isolates in the 'iso1' and 'iso2' columns
#' @param dist_columns Vector of names of each of the columns containing distances 
#' e.g., dist_columns = c('SNPs', 'days')
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
        ) %>% dplyr::pull(cluster_proportion)
}

calc_prop_samples_due_to_transmission <- function(clusters_data){
    clusters_data %>% dplyr::select(id, Cluster) %>% 
        dplyr::filter(!is.na(Cluster)) %>% 
        # count isolates per cluster; remove 1 (index case)
        dplyr::group_by(Cluster) %>% 
        dplyr::summarise(nc = sum(!is.na(Cluster)) - 1) %>% 
        dplyr::ungroup() %>% 
        dplyr::summarise(prop = round(sum(nc) / nrow(clusters_data), digits = 2)) %>% 
        dplyr::pull(prop)
}



summarise_cluster <- function(clusters_data) {
    return(
        tribble(
            ~name, ~value,
            'Samples', n_distinct(clusters_data$id),
            'Clusters', n_distinct(clusters_data$Cluster, na.rm = T),
            'N isolates in clusters', clusters_data %>% filter(!is.na(Cluster)) %>% nrow(),
            'Countries', n_distinct(clusters_data$Country, na.rm = T),
            'Years', n_distinct(clusters_data$Year, na.rm = T),
            'STs', n_distinct(clusters_data$ST, na.rm = T)
        )
    )}


