library(shiny)
library(DT)
library(ggpubr)
library(ggnetwork)
library(plotly)

### GET CLUSTERS ----------------
# Get cluster graph based on SNPs, dates of isolation, and spatial (geo) clustering
epi_snp_graph <- shiny::reactive({
    shiny::req(snp_and_epi_data(), input$snp_threshold, input$temporal_threshold)
    get_cluster_graph(snp_and_epi_data(),
                      dist_columns = c('dist', 'days'),
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


### DYNAMIC INPUT / OPTIONS ----------------
# Column in metadata to use for spatial clustering
output$geo_column_picker <- shiny::renderUI({
    choices <- metadata() %>% dplyr::select(where(is.character)) %>% 
        names() %>% unique() %>% as.character()
    choices <- setdiff(choices, NO_CHOICE_VARS)
    shiny::selectInput(inputId = "geo_column_picker", 
                       label = "Geographic column for clustering", 
                       choices = choices,
                       selected = "Country") # Country is a mandatory metadata column
})
# Column in metadata data to use for colouring clusters plot
output$clusters_plot_colour_var <- shiny::renderUI({
    choices <- metadata() %>% dplyr::select(where(is.character)) %>% 
        names() %>% unique() %>% as.character()
    choices <- setdiff(choices, NO_CHOICE_VARS)
    shiny::selectInput(inputId = "clusters_plot_colour_var", 
                       label = "Colour plot by:", 
                       choices = choices,
                       selected = "Country") # Country is a mandatory metadata column
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
#     max_val <- max(snp_and_epi_data()$days, na.rm = T)
#     shiny::sliderInput(inputId = 'max_temporal_dist_option',
#                        label = "Max temporal distance value", 
#                        min = 0, max = max_val,
#                        value = max_val)
# })

### RENDER UI ---------------
## Show / hide clustering options
observeEvent(input$cluster_options_button, {
    if(input$cluster_options_button %% 2 == 1){
        shinyjs::hide(id = "cluster_options_box")
        shiny::updateActionButton(session, inputId = "cluster_options_button", 
                                  icon = shiny::icon("plus"))
       
    }else{
        shinyjs::show(id = "cluster_options_box")
        shiny::updateActionButton(session, inputId = "cluster_options_button", 
                                  icon = shiny::icon("minus"))
    }
})

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

# distance distributions  
output$snp_distribution_plot <- plotly::renderPlotly({
    shiny::req(snp_and_epi_data())
    p <- plot_dist_distribution(snp_and_epi_data(), dist_column = "dist", 
                                x_label = "Pairwise distance (SNPs)",
                                plot_title = NULL, bins = 10)
    plotly::ggplotly(p)
})
output$temporal_distribution_plot <- plotly::renderPlotly({
    shiny::req(snp_and_epi_data())
    p <- plot_dist_distribution(snp_and_epi_data(), dist_column = "days", 
                                x_label = "Pairwise temporal distance (weeks)",
                                plot_title = NULL, bins = 1)
    plotly::ggplotly(p)
})


# Clusters summary 
output$clusters_summary <- shiny::renderTable({
        shiny::req(epi_snp_clusters())
        summarise_cluster(epi_snp_clusters())
    }, 
    colnames = FALSE, align = 'l')

# Plot clusters 
output$clusters_plot <- plotly::renderPlotly({
    shiny::req(epi_snp_clusters(), input$min_cluster_size)
    clusters_plot <- plot_clusters2(epi_snp_clusters(), min_cluster_size = input$min_cluster_size,
                                    color_column = input$clusters_plot_colour_var)
    clusters_plot <- clusters_plot + ggplot2::guides(fill = "none", size = "none", color = "none")
    plotly::ggplotly(clusters_plot, height = 600, tooltip = c("x", "y", "colour"))
})

#  Get cluster stats by grouping variable
output$cluster_stats_stratify_var <- shiny::renderUI({
    shiny::req(epi_snp_clusters())
    choices <- epi_snp_clusters() %>%
        dplyr::select(where(is.character)) %>% 
        dplyr::select(!truncated_resistance_hits:spurious_virulence_hits) %>% 
        names() %>% unique() %>% as.character()
    choices <- setdiff(choices, c(NO_CHOICE_VARS, "Cluster"))
    shiny::selectInput(inputId = "cluster_stats_stratify_var", 
                       label = "Get cluster stats by:", 
                       choices = choices,
                       selected = "Country") 
})
stratified_cluster_stats <- shiny::reactive({
    shiny::req(epi_snp_clusters(), input$cluster_stats_stratify_var)
    cluster_stats_by_variable(epi_snp_clusters(), grouping_var = input$cluster_stats_stratify_var)
})
output$cluster_stats_stratified <- shiny::renderTable({
    shiny::req(stratified_cluster_stats())
    stratified_cluster_stats()$stats
}, align = 'l')
output$cluster_stats_stratified_plot <- plotly::renderPlotly({
    shiny::req(stratified_cluster_stats())
    plotly::ggplotly(stratified_cluster_stats()$plot, height = 600) # %>% 
        # plotly::layout(legend = list(orientation = 'h', 
        #                              x=0, xanchor='left', yanchor='bottom', 
        #                              orientation='h'))
})


# Plot transmissions 
output$transmission_plot <- plotly::renderPlotly({
    shiny::req(epi_snp_graph(), kleborate_data())
    user_network <- ggnetwork::ggnetwork(epi_snp_graph())
    transmission_plot <- plot_transmission_network(user_network, kleborate_data(), "ST")
    plotly::ggplotly(transmission_plot,
                     height = 600)
})





