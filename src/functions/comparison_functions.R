library(tidyverse)
library(ggplot2)
library(furrr)
library(parallelly)
library(future)


compare_estimates_by_group <- function(metadata, snp_and_epi_data, comparison_column, 
                                        snp_range, date_range) {
    # Vector of groups to iterate
    comparison_groups <- unique(metadata[[comparison_column]])
    # parallel loop using furrr
    comparisons <- furrr::future_map_dfr(comparison_groups, ~{
        group_metadata <- metadata %>% dplyr::filter(!!sym(comparison_column) == .x)
        group_snp_and_epi_data <- snp_and_epi_data %>%
            dplyr::filter(iso1 %in% group_metadata$id & iso2 %in% group_metadata$id)
        
        get_cluster_sensitivity(group_snp_and_epi_data, group_metadata,
                                snp_range = snp_range, date_range = date_range) %>%
            dplyr::mutate(comparison_group = .x)
    })
    return(comparisons)
}

get_comparison_group_per_dataset <- function(
        combined_datasets_df, user_input_var, dataset_id_col="Study"){
    # Vector of groups to iterate
    unique_datasets <- unique(combined_datasets_df[[dataset_id_col]])
    # parallel loop and combine all datasets afterwards
    d <- furrr::future_map_dfr(unique_datasets, ~{
        # subset
        dataset_df <- combined_datasets_df %>% 
            dplyr::filter(!!sym(dataset_id_col) == .x)
        # Stratify by user_input_var if exists (not NAs)
        if (all(is.na(dataset_df[[user_input_var]]))){
            comp_var <- dataset_id_col
            msg <- glue::glue('Selected stratification variable ({user_input_var}) 
                        not available for the {.x} study. Showing estimates for the
                        entire dataset instead.')
            tryCatch({
                showNotification(msg, type='message', duration=10)
            }, error = function(err) { print(msg) } )
        } else {comp_var <- user_input_var}
        dataset_df %>% dplyr::mutate(comparison_column = !!sym(comp_var))
    })
    return(d)
}


    