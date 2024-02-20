
### IS DATA LOADED? -------------------------------------------------------

# check data loaded
data_loaded <- shiny::reactive({
    if (is.null(dataset$snp_data) ||
        is.null(dataset$metadata) ||
        is.null(dataset$kleborate_data)) {
        data_loaded <- FALSE
    } else {
        data_loaded <- TRUE
        final_data$snp_data <- dataset$snp_data
        final_data$metadata <- dataset$metadata
        final_data$kleborate_data <- dataset$kleborate_data
    }
    return(data_loaded)
})
# Is data loaded? - Boolean to UI
output$data_loaded <- shiny::reactive({
    data_loaded()
})
shiny::outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)


### FILTER DATA -----------------------------------------------------------

# Get filtering options for sidebar; From column(s) in metadata
output$filter_data_columns <- shiny::renderUI({
    shiny::req(dataset$metadata)
    d <- dataset$metadata %>% dplyr::select(-c(id, Day))
    choices <- d %>% names() %>% unique() %>% as.character()
    shinyWidgets::pickerInput(inputId = "filter_data_columns",
                              label= glue::glue("Filter data by: "),
                              # selected = "Country", # Country is a mandatory metadata column
                              multiple = TRUE,
                              options = pickerOptions(actionsBox = TRUE,
                                                      noneSelectedText = "No filter applied"),
                              choices = choices) 
})
output$filter_data_options <- shiny::renderUI({
    shiny::req(input$filter_data_columns, dataset$metadata)
    # data
    d <- dataset$metadata %>% dplyr::select(-c(id, Day))
    # empty list to store UI elements
    filter_data_inputs <- list()
    # new picker element for each selected filter column 
    for (filter_col in input$filter_data_columns) {
        choices <- d %>%
            dplyr::pull(!!rlang::sym(filter_col)) %>%
            unique() %>% sort() %>% as.character()
        # Construct the pickerInput element for each filter column
        filter_data_input <- shinyWidgets::pickerInput(
            # dynamic input ID to use in accessing filter values
            inputId = paste0("filter_data_options_", filter_col),
            label = glue::glue("{filter_col} filter"),
            # selected = choices, # Select all by default
            multiple = TRUE,
            options = pickerOptions(
                actionsBox = TRUE,
                noneSelectedText = glue::glue("No {filter_col} filter applied")
            ),
            choices = choices,
        )
        # Append the UI element to the list
        filter_data_inputs[[filter_col]] <- filter_data_input
    }
    tagList(filter_data_inputs)
})
# Apply user filters (if any)
shiny::observeEvent(input$apply_filters,{
    shiny::req(dataset, input$apply_filters)
    # reset final data
    final_data$snp_data <- dataset$snp_data
    final_data$metadata <- dataset$metadata
    final_data$kleborate_data <- dataset$kleborate_data
    # apply filters
    for (filter_col in input$filter_data_columns){
        input_id <- paste0("filter_data_options_", filter_col)
        filter_values <- input[[input_id]]
        final_data$metadata <- filter_data(final_data$metadata, filter_col, filter_values)
    }
})
# Clear all filters
shiny::observeEvent(input$clear_filters, {
    shiny::req(dataset)
    # reset final data
    final_data$snp_data <- dataset$snp_data
    final_data$metadata <- dataset$metadata
    final_data$kleborate_data <- dataset$kleborate_data
    # clear filter options
    shinyWidgets::updatePickerInput(session, 'filter_data_columns', selected = character(0))
})


### PREP FINAL DATA ------------------------------------------------

# Only use data with matching IDs in SNP data, metadata, and kleborate data
matching_ids <- shiny::reactive({
    shiny::req(final_data)
    s <- unique(c(final_data$snp_data$iso1, final_data$snp_data$iso2))
    m <- final_data$metadata$id
    k <- final_data$kleborate_data$`Genome Name`
    return(base::intersect(base::intersect(s,m),k))
})
metadata <- shiny::reactive({
    shiny::req(final_data)
    # Report N samples used in final data
    if (length(matching_ids()) >= 1) {
        shiny::showNotification(glue::glue('Using {length(matching_ids())} samples 
                                           with matching rows in metadata, 
                                           kleborate data, and snp data'), 
                                type = 'message', duration = 3)
    } else {
        shiny::showNotification('No samples with matching rows in the metadata, 
                                kleborate data, and snp data', 
                                type = 'error', duration = 5)
    }
    
    final_data$metadata %>% dplyr::filter(id %in% matching_ids())
})
kleborate_data <- shiny::reactive({
    shiny::req(final_data)
    final_data$kleborate_data %>% dplyr::filter(`Genome Name` %in% matching_ids())
})
snp_data <- shiny::reactive({
    shiny::req(final_data)
    final_data$snp_data %>% 
        dplyr::filter(iso1 %in% matching_ids()) %>% 
        dplyr::filter(iso2 %in% matching_ids())
})


### IS DATA EMPTY? -------------------------------------------------------

# check if data empty
data_empty <- shiny::reactive({
    if (length(matching_ids() >= 1)) { data_empty <- FALSE} 
    else {data_empty <- TRUE}
    return(data_empty)
})
# Is data empty? - Boolean to UI
output$data_empty <- shiny::reactive({
    data_empty()
})
shiny::outputOptions(output, "data_empty", suspendWhenHidden = FALSE)


# HIDE / SHOW TABS IF DATA LOADED / NON-EMPTY -------------------------------
shiny::observe({
    if ((! data_loaded()) || data_empty()) {
        shiny::hideTab(inputId = "main_tabs", target = "Clusters")
        shiny::hideTab(inputId = "main_tabs", target = "Sensitivity")
    } else {
        shiny::showTab(inputId = "main_tabs", target = "Clusters")
        shiny::showTab(inputId = "main_tabs", target = "Sensitivity")
        if (input$data_option == "Upload dataset") {
            shiny::updateTabsetPanel(inputId = "main_tabs", selected = "Clusters")
        }
    }
})


### PRE_PROCESSING AND SUMMARY ------------------------------------------------

# format dates
sample_dates <- shiny::reactive({
    shiny::req(metadata())
    format_sample_dates(metadata())
})
# get df of snp and date (weeks) distances, and shared geolocation
snp_and_epi_data <- shiny::reactive({
    shiny::req(snp_data(), metadata(), sample_dates(), input$geo_column_picker)
    get_snp_and_epi_data(snp_data(), sample_dates(), metadata(), 
                         geo_column = input$geo_column_picker) 
})
# Dataset summary
data_summary <- shiny::reactive({
    shiny::req(metadata(), kleborate_data())
    summarise_dataset(metadata(), kleborate_data(), matching_ids())
})
output$data_summary <- shiny::renderTable(data_summary(), colnames = FALSE, align = 'l')

