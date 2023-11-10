
### GET CLUSTER/TRANSMISSION PROPORTION FOR DIFFERENT SITES --------------

# get cluster data for each site
sites_sensitivity_data <- shiny::reactive({
    shiny::req(metadata(), snp_and_epi_data(), sensitivity_temp_dist_range_vals(), input$snp_threshold)
    results <- vector("list", length(unique(metadata()$Site)))
    i <- 1
    for (site in unique(metadata()$Site)) {
        site_metadata <- metadata() %>% dplyr::filter(Site == site)
        site_snp_and_epi_data <- snp_and_epi_data() %>% 
            dplyr::filter(iso1 %in% site_metadata$id & iso2 %in% site_metadata$id)
        comparison_sensitivity_data <- 
            get_cluster_sensitivity(site_snp_and_epi_data, site_metadata,
                                    snp_range = input$snp_threshold, # selected value in cluster tab
                                    date_range = sensitivity_temp_dist_range_vals()) %>% # selected range in sensitivity tab
            dplyr::mutate(Site = site)
        # store res
        results[[i]] <- comparison_sensitivity_data
        i <- i + 1
    }
    dplyr::bind_rows(results)
})

# Plots
output$comparison_cluster_prop_plot <- plotly::renderPlotly({
    shiny::req(sites_sensitivity_data())
    p <- plot_site_sensitivity_comparisons(sites_sensitivity_data(), input$snp_threshold,
                                           sensitivity_temp_dist_range_vals(),
                                           prop_var = 'cluster_prop', comparison_var = "Site",
                                           y_title = "Proportion in clusters", 
                                           plot_title = glue::glue("Proportion in clusters (SNPs = {input$snp_threshold})"))
    plotly::ggplotly(p, height = 500)
})
output$comparison_transmission_prop_plot <- plotly::renderPlotly({
    shiny::req(sites_sensitivity_data())
    p <- plot_site_sensitivity_comparisons(sites_sensitivity_data(), input$snp_threshold,
                                           sensitivity_temp_dist_range_vals(),
                                           prop_var = 'transmission_prop', comparison_var = "Site",
                                           y_title = "Proportion due to transmission", 
                                           plot_title = glue::glue("Proportion due to transmission (SNPs = {input$snp_threshold})"))
    plotly::ggplotly(p, height = 500)
})


# TESTING
# results <- vector("list", length(unique(BARNARDS_metadata$Site)))
# i <- 1
# for (site in unique(BARNARDS_metadata$Site)) {
#     print(site)
#     site_metadata <- BARNARDS_metadata %>% dplyr::filter(Site == site)
#     site_snp_and_epi_data <- BARNARDS_snp_and_epi_data %>%
#         dplyr::filter(iso1 %in% site_metadata$id & iso2 %in% site_metadata$id)
#     comparison_sensitivity_data <-
#         get_cluster_sensitivity(site_snp_and_epi_data, site_metadata,
#                                 snp_range = 10,
#                                 date_range = c(1, 2, 4, 8, 52)) %>%
#         dplyr::mutate(Site = site)
#     # store res
#     results[[i]] <- comparison_sensitivity_data
#     i <- i + 1
# }
# sites_sensitivity_data <- dplyr::bind_rows(results)
# plot_site_sensitivity_comparisons(sites_sensitivity_data, snp_val = 10,
#                                   temp_dist_range_vals = c(1, 2, 4, 8, 52))

# PLOT 





