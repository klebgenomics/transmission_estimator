library(shiny)
library(DT)
library(ggpubr)
library(ggnetwork)
library(plotly)


### DYNAMIC INPUT / OPTIONS --------------------------------
shiny::observeEvent(req(input$snp_threshold), {
    if(input$snp_threshold > MAX_SNP_DIST){
        showNotification(paste0("Selected value outside allowed range. Using max allowed value: ", MAX_SNP_DIST), 
                         type='warning', duration=3)
        shiny::updateNumericInput(session, 'snp_threshold', value = MAX_SNP_DIST)
    }
}, ignoreInit = TRUE)
shiny::observeEvent(req(input$temporal_threshold), {
    if(input$temporal_threshold > MAX_TEMP_DIST){
        showNotification(paste0("Selected value outside allowed range. Using max allowed value: ", MAX_TEMP_DIST), 
                         type='warning', duration=3)
        shiny::updateNumericInput(session, 'temporal_threshold', value = MAX_TEMP_DIST)
    }
}, ignoreInit = TRUE)

# Column in metadata to use for spatial clustering
output$geo_column_picker <- shiny::renderUI({
    choices <- select_metadata_and_kleborate_var_choices(metadata())
    shiny::selectInput(inputId = "geo_column_picker", 
                       label = "Spatial clustering variable", 
                       choices = choices,
                       selected = "Site") # Site is a mandatory metadata column
})

# Column in metadata data to use for colouring clusters plot
output$clusters_plot_colour_var <- shiny::renderUI({
    shiny::req(epi_snp_clusters(), metadata())
    choices <- select_metadata_and_kleborate_var_choices(
        metadata(), epi_snp_clusters()
    )
    shiny::selectInput(inputId = "clusters_plot_colour_var", 
                       label = "Colour plot by:", 
                       choices = choices,
                       selected = "Site") # Site is a mandatory metadata column
})

# Column in metadata data to use for stratifying cluster stats
output$cluster_stats_grouping_var <- shiny::renderUI({
    shiny::req(epi_snp_clusters(), metadata())
    choices <- select_metadata_and_kleborate_var_choices(
        metadata(), epi_snp_clusters()
    )
    shiny::selectInput(inputId = "cluster_stats_grouping_var", 
                       label = "Grouping variable:", 
                       choices = choices,
                       selected = "Country") 
})

## Show / hide clustering options
observeEvent(input$cluster_options_button, {
    if(input$cluster_options_button %% 2 == 1){
        shinyjs::hide(id = "cluster_options_box")
        shiny::updateActionButton(session, inputId = "cluster_options_button", 
                                  icon = shiny::icon("plus"), label = "Expand: Clustering options")
        
    }else{
        shinyjs::show(id = "cluster_options_box")
        shiny::updateActionButton(session, inputId = "cluster_options_button", 
                                  icon = shiny::icon("minus"), label = "Collapse")
    }
})

# # Distribution plot options
# output$max_snp_option <- shiny::renderUI({
#     shiny::req(snp_and_epi_data())
#     max_val <- max(snp_and_epi_data()$dist, na.rm = T)
#     shiny::sliderInput(inputId = 'max_snp_option',
#                        label = "Max SNP value", 
#                        min = 0, max = max_val,
#                        value = max_val)
# })
# output$max_temporal_dist_option <- shiny::renderUI({
#     shiny::req(snp_and_epi_data())
#     max_val <- max(snp_and_epi_data()$weeks, na.rm = T)
#     shiny::sliderInput(inputId = 'max_temporal_dist_option',
#                        label = "Max temporal distance value", 
#                        min = 0, max = max_val,
#                        value = max_val)
# })


### GET CLUSTERS ------------------------------------------------
# Get cluster graph based on SNPs, dates of isolation, and spatial (geo) clustering
epi_snp_graph <- shiny::reactive({
    shiny::req(snp_and_epi_data(), input$snp_threshold, input$temporal_threshold)
    get_cluster_graph(snp_and_epi_data(),
                      dist_columns = c('dist', 'weeks'),
                      dist_thresholds = c(input$snp_threshold,
                                          input$temporal_threshold),
                      pair_location_column = "pair_location")
})
# Get clusters; Rejoin to metadata and kleborate data
epi_snp_clusters <- shiny::reactive({
    shiny::req(metadata(), kleborate_data(), sample_dates(), epi_snp_graph())
    get_cluster_membership_from_graph(epi_snp_graph()) %>% 
        dplyr::right_join(metadata(), by = 'id') %>% 
        dplyr::left_join(sample_dates(), by = 'id') %>% 
        dplyr::left_join(kleborate_data(), by = c('id' = 'Genome Name'))
})


### CLUSTERS EXIST ? -------------------------------------------------------
# Check if clusters exist
clusters_exist <- shiny::reactive({
    shiny::req(epi_snp_clusters())
    n_clusters <- n_distinct(epi_snp_clusters()$Cluster, na.rm = T)
    if (n_clusters >= 1) { clusters_exist <- TRUE} 
    else {clusters_exist <- FALSE}
    return(clusters_exist)
})
# Clusters exist ? - Boolean to UI
output$clusters_exist <- shiny::reactive({
    clusters_exist()
})
shiny::outputOptions(output, "clusters_exist", suspendWhenHidden = FALSE)


### OUTPUT -----------------------------------------------

# Clusters summary 
output$clusters_summary <- shiny::renderTable({
    shiny::req(epi_snp_clusters())
    summarise_cluster(epi_snp_clusters()) %>% 
        dplyr::mutate(value = as.character(value))
}, 
colnames = FALSE, align = 'l')

# Proportion of cases part of a cluster
output$cluster_proportion <- shiny::renderText({
    shiny::req(epi_snp_clusters())
    cluster_proportion <- calculate_cluster_proportion(epi_snp_clusters())
    glue::glue("Proportion of cases part of a transmission cluster = {cluster_proportion}")
})

# Proportion of cases due to transmission
output$transmission_proportion <- shiny::renderText({
    shiny::req(epi_snp_clusters())
    transmission_proportion <- calc_prop_samples_due_to_transmission(epi_snp_clusters())
    glue::glue("Proportion of cases due to transmission = {transmission_proportion}")
})

# SNP distance distribution plot
snp_distribution_plot <- shiny::reactive({
    shiny::req(snp_and_epi_data())
    p <- plot_dist_distribution(snp_and_epi_data(), dist_column = "dist", 
                                x_label = "Pairwise distance (SNPs)",
                                plot_title = NULL, bins = 10)
    plotly::ggplotly(p)
})
output$snp_distribution_plot <- plotly::renderPlotly(snp_distribution_plot())

# Temporal distance distribution plot
temporal_distribution_plot <- shiny::reactive({
    shiny::req(snp_and_epi_data())
    p <- plot_dist_distribution(snp_and_epi_data(), dist_column = "weeks", 
                                x_label = "Pairwise temporal distance (weeks)",
                                plot_title = NULL, bins = 1)
    plotly::ggplotly(p)
})
output$temporal_distribution_plot <- plotly::renderPlotly(temporal_distribution_plot())

# Clusters plot
clusters_plot <- shiny::reactive({
    shiny::req(clusters_exist(), epi_snp_clusters(), 
               input$min_cluster_size, input$clusters_plot_colour_var)
    d <- rename_vars_for_plotting(epi_snp_clusters(), input$clusters_plot_colour_var)
    clusters_plot <- plot_clusters2(d, min_cluster_size = input$min_cluster_size,
                                    color_column = input$clusters_plot_colour_var)
    clusters_plot <- clusters_plot + ggplot2::guides(fill = "none", size = "none", color = "none")
    plotly::ggplotly(clusters_plot, height = 600, tooltip = c("x", "y", "colour", "size")) %>% 
        plotly::layout(yaxis = list(title = list(standoff = 30L)))
})
output$clusters_plot <- plotly::renderPlotly(clusters_plot())

# Cluster stats by grouping variable
clusters_by_group <- shiny::reactive({
    shiny::req(clusters_exist(), epi_snp_clusters(), input$cluster_stats_grouping_var)
    d <- rename_vars_for_plotting(epi_snp_clusters(), input$cluster_stats_grouping_var)
    cluster_stats_by_variable(d, grouping_var = input$cluster_stats_grouping_var)
})
clusters_by_group_plot <- shiny::reactive({
    shiny::req(clusters_by_group())
    p <- clusters_by_group()$plot 
    plotly::ggplotly(p, height = 600, tooltip = c("x", "y", "fill")) %>%
        plotly::style(hoverinfo = "none", traces = c(4:6), 
                      textposition = "right", cliponaxis = FALSE) %>% 
        plotly::layout(yaxis = list(title = list(standoff = 30L)),
                       margin = list(r = 80),
                       legend=list(x=0, y = -.15,
                                   orientation='h'))
})
output$clusters_by_group_plot <- plotly::renderPlotly(clusters_by_group_plot())


# Transmission graphs plot
transmission_graph <- shiny::reactive({
    shiny::req(clusters_exist(), epi_snp_graph(), kleborate_data())
    user_network <- ggnetwork::ggnetwork(epi_snp_graph())
    transmission_plot <- plot_transmission_network(user_network, kleborate_data(), "ST")
    plotly::ggplotly(transmission_plot,
                     height = 600)
})





