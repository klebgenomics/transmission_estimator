library(shiny)
library(ggpubr)
library(plotly)

### SENSITIVITY OPTIONS ----------------------------------------------

# Threshold values for sensitivity calculation; all combinations are pre-calculated
snp_range <- seq(0, 25, by = 5)
date_range <- seq(1, 52, by = 1) # weeks

# tab help info
output$estimates_tab_info <- renderText({
    glue::glue("Explore the sensitivity of the transmission estimates to the choice of 
    temporal and genetic distance thresholds. Clustering is calculated using unique 
    combinations of temporal thresholds (adjust above), genetic distance thresholds
    ({paste(range(snp_range),collapse=' - ')}), and the spatial clustering variable 
    ('{input$geo_column_picker}' - adjust in the Clusters tab).")
})
shiny::outputOptions(output, "estimates_tab_info", suspendWhenHidden = FALSE)


# User-set thresholds for sensitivity plots
output$date_threshold <- shiny::renderUI({
    shiny::numericInput(inputId = 'date_threshold',
                        label = 'Temporal threshold (weeks)',
                        value = 4, min = 3, max = 24)
})
output$date_threshold_range <- shiny::renderUI({
    # set intuitive colours for both sliders 
    shiny::fluidPage(shinyWidgets::setSliderColor(c("#ffc1c1", "#8b0000"),
                                                  sliderId = c(1, 2)),
                     shiny::sliderInput(inputId = "date_threshold_range", 
                                        label = "Temporal threshold range", 
                                        min = 2, max = 25, value = c(2, 8))
    )
})
output$ext_date_threshold_range <- shiny::renderUI({
    shiny::sliderInput(inputId = "ext_date_threshold_range", 
                       label = "Extreme temporal threshold range", 
                       min = 1, max = 52, value = c(1, 52))
})
lapply(c("date_threshold", "date_threshold_range", "ext_date_threshold_range"),
       function(x) shiny::outputOptions(output, x, suspendWhenHidden = FALSE))

# Constrain thresholds in sensible ranges
observeEvent(req(input$date_threshold), {
    val = input$date_threshold
    if (input$date_threshold >= input$date_threshold_range[2]){
        showNotification("Selected value outside allowed range", type='warning', duration=2)
        val = input$date_threshold - 1
    }
    updateNumericInput(session, "date_threshold", value = val)
})
observeEvent(input$date_threshold_range, {
    min_dtr = input$date_threshold_range[1]
    max_dtr = input$date_threshold_range[2]
    if (input$date_threshold_range[1] >= input$date_threshold){
        showNotification("Selected value outside allowed range", type='warning', duration=2)
        min_dtr = input$date_threshold - 1
    } 
    if (input$date_threshold_range[2] <= input$date_threshold){
        showNotification("Selected value outside allowed range", type='warning', duration=2)
        max_dtr = input$date_threshold + 1
    }
    updateSliderInput(session, "date_threshold_range", value = c(min_dtr, max_dtr))
})
observeEvent(input$ext_date_threshold_range, {
    min_edtr = input$ext_date_threshold_range[1]
    max_edtr = input$ext_date_threshold_range[2]
    if (input$ext_date_threshold_range[1] >= input$date_threshold_range[1]){
        showNotification("Selected value outside allowed range", type='warning', duration=2)
        min_edtr = input$date_threshold_range[1] - 1
    } 
    if (input$ext_date_threshold_range[2] <= input$date_threshold_range[2]){
        showNotification("Selected value outside allowed range", type='warning', duration=2)
        max_edtr = input$date_threshold_range[2] + 1
    }
    updateSliderInput(session, "ext_date_threshold_range", value = c(min_edtr, max_edtr))
})

# temp dist values for plot (n=5)
sensitivity_temp_dist_vals <- shiny::reactive({
    c(input$ext_date_threshold_range[1],
      input$date_threshold_range[1],
      input$date_threshold,
      input$date_threshold_range[2],
      input$ext_date_threshold_range[2])
}) 


### GET CLUSTER/TRANSMISSION PROPORTION FOR COMBINATION OF THRESHOLD VALUES ----

sensitivity_df <- shiny::reactive({
    shiny::req(metadata(), snp_and_epi_data(), snp_range, date_range)
    future::plan(multisession, workers = 4)
    sensitivity_df <- get_cluster_sensitivity(snp_and_epi_data(), metadata(), 
                                              snp_range, date_range)
    future::plan("default")
    return(sensitivity_df)
}) %>% shiny::bindCache(metadata(), snp_and_epi_data(), snp_range, date_range)


### SENSITIVITY PLOTS ----------------------------------------------

# Proportion of cases in clusters by SNP threshold, for range of temporal thresholds
cluster_sensitivity_plot <- shiny::reactive({
    shiny::req(sensitivity_df(), 
               sensitivity_temp_dist_vals(),
               !any(is.na(sensitivity_temp_dist_vals())),
               !any(duplicated(sensitivity_temp_dist_vals())))
    plot_sensitivity_SNP_vs_temp_range(
        sensitivity_df(), temp_dist_vals = sensitivity_temp_dist_vals(), 
        prop_var = "cluster_prop", y_title = "Proportion in clusters",
        plot_title = "Proportion in clusters")
})
output$cluster_sensitivity_plot <- plotly::renderPlotly({
    plotly::ggplotly(cluster_sensitivity_plot(), height = 400)
})

# Proportion of cases due to transmission by SNP threshold, for range of temporal thresholds
transmission_sensitivity_plot <- shiny::reactive({
    shiny::req(sensitivity_df(), 
               sensitivity_temp_dist_vals(),
               !any(is.na(sensitivity_temp_dist_vals())),
               !any(duplicated(sensitivity_temp_dist_vals())))
    plot_sensitivity_SNP_vs_temp_range(
        sensitivity_df(), temp_dist_vals = sensitivity_temp_dist_vals(), 
        prop_var = "transmission_prop", y_title = "Proportion due to transmission",
        plot_title = "Proportion due to transmission")
})
output$transmission_sensitivity_plot <- plotly::renderPlotly({
    plotly::ggplotly(transmission_sensitivity_plot(), height = 400)
})

# Sens legend
sensitivity_legend <- shiny::reactive({
    shiny::req(sensitivity_df())
    manual_sensitivity_legend(mid_temp_val=sensitivity_temp_dist_vals()[3],
                              temp_range_1 = sensitivity_temp_dist_vals()[c(2,4)],
                              temp_range_2 = sensitivity_temp_dist_vals()[c(1,5)])
})

