library(tidyverse)
library(ggplot2)


plot_site_sensitivity_comparisons <- function(
        sites_sensitivity_data, snp_val, temp_dist_range_vals,
        prop_var = 'cluster_prop', comparison_var = "Site",
        y_title = "Proportion in clusters", plot_title = NULL){
    if(length(snp_val) != 1){stop("Exactly one snp value required")}
    if(length(temp_dist_range_vals) != 5){stop("Exactly five temporal dist values (weeks) required")}

    # rescale days to weeks
    sites_sensitivity_data %<>% 
        dplyr::mutate(temporal_threshold = temporal_threshold / 7) %>% 
        # get only rows with snp_val
        dplyr::filter(snp_threshold == snp_val)
    # arrange sites by prop_var at mid point temp dist value
    ordered_d <- sites_sensitivity_data %>% 
        dplyr::filter(temporal_threshold == temp_dist_range_vals[3]) %>% 
        dplyr::arrange(.data[[prop_var]])
    # Use intuitive var names for interactive plot
    y_vars <- paste0(prop_var, " at ", temp_dist_range_vals, " weeks threshold") 
    # wrangle and plot
    sites_sensitivity_data %>% 
        dplyr::filter(temporal_threshold %in% temp_dist_range_vals) %>% 
        unique() %>% 
        dplyr::select(snp_threshold, temporal_threshold, all_of(c(prop_var, comparison_var))) %>% 
        dplyr::mutate(Site = factor(Site, levels = ordered_d$Site)) %>% 
        tidyr::pivot_wider(id_cols = c(snp_threshold, .data[[comparison_var]]), 
                           names_from = temporal_threshold, values_from = .data[[prop_var]]) %>% 
        dplyr::rename_at(vars(as.character(temp_dist_range_vals)), 
                         ~tidyselect::all_of(y_vars)) %>% 
        ggplot2::ggplot(aes(x = .data[[comparison_var]])) +
        ggplot2::geom_linerange(aes(ymin = .data[[y_vars[1]]], ymax = .data[[y_vars[5]]]), 
                                col = "#ffc1c1", lwd = 4) +
        ggplot2::geom_linerange(aes(ymin = .data[[y_vars[2]]], ymax = .data[[y_vars[4]]]), 
                                col = "#8b0000", lwd = 2) +
        ggplot2::geom_point(aes(y = .data[[y_vars[3]]]), col = "white", size = 1) +
        ggplot2::theme_dark() + ggplot2::ylim(0, 1) + 
        ggplot2::theme(panel.grid = element_blank()) +
        ggplot2::labs(x = "Sites", y = y_title, title = plot_title) +
        ggplot2::coord_flip()
}

    
    
    