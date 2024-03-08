
# tab help info
output$compare_estimates_tab_info <- renderText({
    glue::glue("Compare the sensitivity estimates across different groups. For each group, clustering is calculated for a 
    range of temporal thresholds (adjustable above) using the genetic distance threshold ({input$snp_threshold}) 
    and spatial clustering variable ('{input$geo_column_picker}') selected in the Clusters tab.")
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
    compare_estimates_by_group(metadata(), snp_and_epi_data(), 
                               input$comparison_var_picker, 
                               snp_range = input$snp_threshold, # selected value in cluster tab
                               date_range = sensitivity_temp_dist_vals()) %>% # selected range in sensitivity tab
        dplyr::mutate(data_source = "User data")
})

# get sensitivity data for each public dataset
preloaded_comparison_data <- shiny::reactive({
    shiny::req(PUBLIC_COMP_METADATA, PUBLIC_COMP_SNP_AND_EPI_DATA, PUBLIC_COMP_STUDY_DETAILS,
               sensitivity_temp_dist_vals(), input$snp_threshold)
    # Stratify data by comparison group if selected stratifying var exists
    # else compute estimates per study
    if (input$comparison_var_picker %in% names(PUBLIC_COMP_METADATA)) {
        comparison_var <- input$comparison_var_picker
    } else {comparison_var <- "Study"}
    # delineate comparison groups (add study serial number)
    PUBLIC_COMP_METADATA %<>% mutate(comparison_column = paste0(study_SN, ' - ', !!sym(comparison_var)))
    compare_estimates_by_group(
        PUBLIC_COMP_METADATA, PUBLIC_COMP_SNP_AND_EPI_DATA, 'comparison_column', 
        snp_range = input$snp_threshold, # selected value in cluster tab
        date_range = sensitivity_temp_dist_vals() # selected range in sensitivity tab
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
    plotly::ggplotly(cluster_comparison_plot(), height = 500) # %>% 
        # plotly::layout(yaxis = list(title = list(standoff = 30L)), title = list(x = 0))
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
    plotly::ggplotly(transmission_comparison_plot(), height = 500) # %>% 
        # plotly::layout(yaxis = list(title = list(standoff = 30L)), title = list(x = 0))
})

# Public data sources
output$public_data_sources <- shiny::renderUI({
    shiny::req(input$add_public_data_toggle)
    p.sources <-  PUBLIC_COMP_METADATA %>% dplyr::arrange(study_SN) %>% 
        dplyr::pull(study_publication) %>% unique()
    public_data_sources <- list()
    for (i in seq(length(p.sources))){
        u <- shiny::helpText(p.sources[i])
        public_data_sources[[i]] <- u
    }
    tagList(public_data_sources)
})


