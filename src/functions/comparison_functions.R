library(tidyverse)
library(ggplot2)
library(furrr)
library(parallelly)
library(future)



compare_estimates_by_group <- function(metadata, snp_and_epi_data, comparison_var, 
                                        snp_range, date_range) {
    # Vector of groups to iterate
    comparison_groups <- unique(metadata[[comparison_var]])
    # parallel loop using furrr
    comparisons <- furrr::future_map_dfr(comparison_groups, ~{
        group_metadata <- metadata %>% dplyr::filter(!!sym(comparison_var) == .x)
        group_snp_and_epi_data <- snp_and_epi_data %>%
            dplyr::filter(iso1 %in% group_metadata$id & iso2 %in% group_metadata$id)
        
        get_cluster_sensitivity(group_snp_and_epi_data, group_metadata,
                                snp_range = snp_range, date_range = date_range) %>%
            dplyr::mutate(comparison_group = .x)
    })
    return(comparisons)
}

    
    