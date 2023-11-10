
### IS DATA LOADED? ------------------------------- 

# TODO check and report whether samples in SNP_DATA == METADATA == KLEBORATE 

# check data loaded
data_loaded <- reactive({
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
output$data_loaded <- reactive({
    data_loaded()
})
shiny::outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

# hide tabs if complete data not loaded
shiny::observe({
    if (! data_loaded()) {
        shiny::hideTab(inputId = "main_tabs", target = "Clusters")
        shiny::hideTab(inputId = "main_tabs", target = "Sensitivity")
        shiny::hideTab(inputId = "main_tabs", target = "Comparisons")
    } else {
        shiny::showTab(inputId = "main_tabs", target = "Clusters")
        shiny::showTab(inputId = "main_tabs", target = "Sensitivity")
        shiny::showTab(inputId = "main_tabs", target = "Comparisons")
        if (input$data_option == "Upload dataset") {
            shiny::updateTabsetPanel(inputId = "main_tabs", selected = "Clusters")
        }
    }
})



### DYNAMIC SIDEBAR OPTIONS ---------------

# Filter data by any column(s) in metadata
output$filter_data_columns <- shiny::renderUI({
    shiny::req(dataset$metadata)
    d <- dataset$metadata %>% dplyr::select(-c(id, Day))
    choices <- d %>% names() %>% unique() %>% as.character()
    shinyWidgets::pickerInput(inputId = "filter_data_columns",
                              label= glue::glue("Filter data by: "),
                              choices = choices,
                              multiple = TRUE,
                              options = pickerOptions(actionsBox = TRUE,
                                                      noneSelectedText = "No filter applied"),
                              selected = "Country") # Country is a mandatory metadata column
})

output$filter_data_options <- shiny::renderUI({
    shiny::req(input$filter_data_columns, dataset$metadata)
    # data
    d <- dataset$metadata %>% dplyr::select(-c(id, Day))
    # Create an empty list to store UI elements
    filter_data_inputs <- list()
    # create a picker element for each selected filter column 
    for (filter_col in input$filter_data_columns) {
        choices <- d %>%
            dplyr::pull(!!rlang::sym(filter_col)) %>%
            unique() %>%
            sort() %>%
            as.character()
        # Construct the pickerInput element for each filter column
        filter_data_input <- shinyWidgets::pickerInput(
            # dynamic input ID to use in accessing filter values
            inputId = paste0("filter_data_options_", filter_col),
            label = glue::glue("{filter_col} filter"),
            choices = choices,
            multiple = TRUE,
            options = pickerOptions(
                actionsBox = TRUE,
                noneSelectedText = glue::glue("No {filter_col} filter applied")
            ),
            selected = choices
        )
        # Append the UI element to the list
        filter_data_inputs[[filter_col]] <- filter_data_input
    }
    tagList(filter_data_inputs)
})


### FILTER DATA ------------------------------------
observeEvent(input$apply_filters,{
    shiny::req(dataset$metadata, input$filter_data_columns, input$apply_filters)
    final_data$snp_data <- dataset$snp_data
    final_data$metadata <- dataset$metadata
    final_data$kleborate_data <- dataset$kleborate_data
    for (filter_col in input$filter_data_columns){
        input_id <- paste0("filter_data_options_", filter_col)
        filter_values <- input[[input_id]]
        final_data$metadata <- filter_data(final_data$metadata, filter_col, filter_values)
    }
    final_data$snp_data %<>% 
        dplyr::filter(iso1 %in% final_data$metadata$id) %>% 
        dplyr::filter(iso2 %in% final_data$metadata$id)
    final_data$kleborate_data %<>% 
        dplyr::filter(`Genome Name` %in% final_data$metadata$id)

})


# final reactive data 
metadata <- shiny::reactive(final_data$metadata)
kleborate_data <- shiny::reactive(final_data$kleborate_data)
snp_data <- shiny::reactive(final_data$snp_data)



