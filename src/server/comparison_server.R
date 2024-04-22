
# tab help info
output$compare_estimates_tab_info <- renderText({
    glue::glue("Compare sensitivity estimates across different groups. For each group, 
    clustering is calculated using unique combinations of temporal thresholds (adjust 
    above), the genetic distance threshold ('{input$snp_threshold}' - adjust in the Clusters tab), 
    and the spatial clustering variable ('{input$geo_column_picker}' - adjust in the Clusters tab).")
})
shiny::outputOptions(output, "compare_estimates_tab_info", suspendWhenHidden = FALSE)

### GET CLUSTER/TRANSMISSION PROPORTION FOR DIFFERENT SITES --------------

# Comparison variable picker
output$comparison_var <- shiny::renderUI({
    shiny::req(metadata())
    choices <- get_variable_choices(metadata())
    shiny::selectInput(inputId = "comparison_var_picker", 
                       label = "Comparison variable:", 
                       choices = choices,
                       selected = "Site") 
})

# get sensitivity data for each group in user data
user_comparison_data <- shiny::reactive({
    shiny::req(metadata(), snp_and_epi_data(), input$comparison_var_picker,
               sensitivity_temp_dist_vals(), input$snp_threshold)
    compare_estimates_by_group(
        metadata(), snp_and_epi_data(), 
        comparison_column=input$comparison_var_picker,
        snp_range=input$snp_threshold, date_range=sensitivity_temp_dist_vals()
        ) %>% dplyr::mutate(data_source = "User data")
})

# get sensitivity data for each public dataset
preloaded_comparison_data <- shiny::reactive({
    shiny::req(PUBLIC_COMP_METADATA, PUBLIC_COMP_SNP_AND_EPI_DATA,
               sensitivity_temp_dist_vals(), input$snp_threshold)
    # Stratify data user input if var exists; else compute estimates per study
    if (input$comparison_var_picker %in% names(PUBLIC_COMP_METADATA)) {
        # choose comparison group based on availability per dataset
        public_metadata <- set_comparison_column(
            PUBLIC_COMP_METADATA, input$comparison_var_picker, 
            dataset_id_col="Study")
    } else {
        public_metadata <- PUBLIC_COMP_METADATA %>% mutate(comparison_column=Study)
        showNotification(
            glue::glue('Selected stratification variable ({input$comparison_var_picker}) not 
            available for the public dataset(s). Showing estimates for the entire dataset(s) instead.'),
            type='message', duration=10)
    }
    # delineate public data comparison groups (add study serial number)
    public_metadata %<>% mutate(comparison_column=paste0(study_SN,' - ',comparison_column))
    compare_estimates_by_group(
        public_metadata, PUBLIC_COMP_SNP_AND_EPI_DATA, 
        comparison_column='comparison_column', 
        snp_range=input$snp_threshold, date_range=sensitivity_temp_dist_vals()
    ) %>% dplyr::mutate(data_source = "Preloaded public data")
})

# final comparison data
all_comparison_data <- shiny::reactive({
    shiny::req(user_comparison_data(),
               sensitivity_temp_dist_vals(), input$snp_threshold)
    if (input$add_public_data_toggle == TRUE) {
        dplyr::bind_rows(user_comparison_data(), preloaded_comparison_data())
    } else {
        user_comparison_data()
    }
})

comparison_plot_height <- shiny::reactive({
    n_groups <- length(unique(all_comparison_data()$comparison_group))
    calculate_plot_height(num_y_vars=n_groups)
})

### PLOTS ---------------------------------------------------------------------

# Cluster proportions
cluster_comparison_plot <- shiny::reactive({
    shiny::req(all_comparison_data())
    plot_comparisons(
        all_comparison_data(), input$snp_threshold,
        sensitivity_temp_dist_vals(), comparison_var = "comparison_group",
        prop_var = 'cluster_prop', y_title = "Proportion in clusters",
        plot_title = paste("Cluster proportion",
            glue("estimates per {input$comparison_var_picker} (Distance={input$snp_threshold})"),
            sep = "\n")
    )
})
output$cluster_comparison_plot <- plotly::renderPlotly({
    plotly::ggplotly(cluster_comparison_plot(), height=comparison_plot_height())
})
# render plot UI here for dynamic height
output$cluster_comparison_plot_ui <- shiny::renderUI({
    plotlyOutput("cluster_comparison_plot", height=comparison_plot_height())
})

# Transmission proportions
transmission_comparison_plot <- shiny::reactive({
    shiny::req(all_comparison_data())
    plot_comparisons(
        all_comparison_data(), input$snp_threshold,
        sensitivity_temp_dist_vals(),
        prop_var = 'transmission_prop', comparison_var = "comparison_group",
        y_title = "Proportion due to transmission", 
        plot_title = paste("Transmission proportion",
            glue("estimates per {input$comparison_var_picker} (Distance={input$snp_threshold})"),
            sep = "\n")
    )
})
output$transmission_comparison_plot <- plotly::renderPlotly({
    plotly::ggplotly(transmission_comparison_plot(), height=comparison_plot_height())
})
# render plot UI here for dynamic height
output$transmission_comparison_plot_ui <- shiny::renderUI({
    plotlyOutput("transmission_comparison_plot", height=comparison_plot_height())
})

# Public data sources (footnote)
output$public_data_sources <- shiny::renderUI({
    shiny::req(input$add_public_data_toggle)
    p.sources <-  PUBLIC_COMP_METADATA %>% dplyr::arrange(study_SN) %>% 
        dplyr::pull(study_citation) %>% unique()
    public_data_sources <- list()
    for (i in seq(length(p.sources))){
        u <- shiny::helpText(p.sources[i])
        public_data_sources[[i]] <- u
    }
    tagList(public_data_sources)
})


