library(shiny)
library(DT)
library(ggpubr)
library(ggnetwork)
library(plotly)

### GET CLUSTERS ----------------
# format dates
sample_dates <- shiny::reactive({
    shiny::req(metadata())
    #print(input$filter_data_column)
    #print(input$filter_data_options)
    format_sample_dates(metadata())
})
# get df of snp and date (days) distances, and shared geolocation
snp_and_epi_data <- shiny::reactive({
    shiny::req(snp_data(), metadata(), sample_dates(), input$geo_column_picker)
    get_snp_and_epi_data(snp_data(), sample_dates(), metadata(), 
                         geo_column = input$geo_column_picker) 
})
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
    choices <- metadata() %>% 
        names() %>% unique() %>% as.character()
    shiny::selectInput(inputId = "geo_column_picker", 
                       label = "Geographic column for clustering", 
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
    # plot snp distance vs temporal distance
    p <- plot_dist_distribution(snp_and_epi_data(), dist_column = "dist", 
                                x_label = "Pairwise distance (SNPs)",
                                plot_title = NULL)
    plotly::ggplotly(p)
})
output$temporal_distribution_plot <- plotly::renderPlotly({
    shiny::req(snp_and_epi_data())
    # plot snp distance vs temporal distance
    p <- plot_dist_distribution(snp_and_epi_data(), dist_column = "days", 
                                x_label = "Pairwise temporal distance (days)",
                                plot_title = NULL)
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
    clusters_plot <- plot_clusters(epi_snp_clusters(), min_cluster_size = input$min_cluster_size)
    clusters_plot <- clusters_plot + ggplot2::guides(fill = "none", size = "none", color = "none")
    plotly::ggplotly(clusters_plot, height = 600, tooltip = c("x", "y", "colour"))
})

# Plot transmissions 
output$transmission_plot <- plotly::renderPlotly({
    shiny::req(epi_snp_graph(), kleborate_data())
    user_network <- ggnetwork::ggnetwork(epi_snp_graph())
    transmission_plot <- plot_transmission_network(user_network, kleborate_data(), "ST")
    plotly::ggplotly(transmission_plot,
                     height = 600)
})





