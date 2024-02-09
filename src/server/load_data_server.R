library(shiny)
library(shinyjs)

### DEMO DATA ------------------------
# Load all demo data components
observeEvent(input$data_option, {
    
    if (input$data_option == "Demo dataset") {
        # Load demo data 
        # TODO: remove redundant loading functions
        dataset$snp_data <- read_snp_csv(DEMO_SNP_DATA)
        dataset$metadata <- read_metadata_csv(DEMO_METADATA, REQUIRED_METADATA_COLS)
        dataset$kleborate_data <- read_kleborate_data_csv(DEMO_KLEBORATE_DATA,
                                                          REQUIRED_KLEBORATE_COLS)
        
    } else if (input$data_option == "Upload dataset") {
        # Reset dataset values to NULL when 'Upload data' is selected
        dataset$snp_data <- NULL
        dataset$metadata <- NULL
        dataset$kleborate_data <- NULL
        shinyjs::reset('user_metadata')
        shinyjs::reset('user_distances')
        shinyjs::reset('user_kleborate_data')
    }
})

### USER DATA ------------------------
# Load metadata
observeEvent(
    input$user_metadata,
    {
        # Read in file and perform basic validation
        d <- read_file(input$user_metadata$datapath, 'Metadata')
        if (is.null(d)) {
            dataset$metadata <- NULL
            shinyjs::reset('user_metadata') 
            return()
        } 
        if (! all(tolower(REQUIRED_METADATA_COLS) %in% tolower(names(d))) ) {
            shiny::showNotification(paste0("The following columns are required in the metadata file: ", 
                                    paste(REQUIRED_METADATA_COLS, collapse = ", ")), 
                             type='error', duration=10)
            dataset$metadata <- NULL
            shinyjs::reset('user_metadata')
            return()
        } else {
            # Standardise (rename) mandatory column names
            col_inds <- match(tolower(REQUIRED_METADATA_COLS), tolower(names(d)))
            colnames(d)[col_inds] <- REQUIRED_METADATA_COLS
            # id column always character
            d %<>% dplyr::mutate(dplyr::across(.cols = c(id), as.character))
            # Remove kleborate columns from metadata
        }
        shiny::showNotification('Successfully uploaded metadata file', 
                                type = 'message', duration = 2)
        dataset$metadata <- d %>% 
            # remove required kleborate data cols from metadata if exists
            dplyr::select(-any_of(REQUIRED_KLEBORATE_COLS)) 
    }
)

# Load pathogenwatch distance data
observeEvent(
    input$user_distances,
    {
        # Read in file and perform basic validation
        d <- read_file(input$user_distances$datapath, 'Distance matrix')
        if (is.null(d)) {
            dataset$snp_data <- NULL
            shinyjs::reset('user_distances') 
            return()
        } 
        # check valid matrix
        if(nrow(d) + 1 != ncol(d)){
            shiny::showNotification('Number of rows and cols in snp data must be the same', 
                             type='error', duration=10)
            dataset$snp_data <- NULL
            shinyjs::reset('user_distances')
            return()
        } 
        if(!'Name' %in% names(d)){
            shiny::showNotification('Name column not in snp_data, is it from Pathogenwatch?', 
                             type='error', duration=NULL)
            dataset$snp_data <- NULL
            shinyjs::reset('user_distances')
            return()
        }
        shiny::showNotification('Successfully uploaded distances file', 
                                type = 'message', duration = 2)
        dataset$snp_data <- d %>% 
            tidyr::pivot_longer(cols = !Name, values_to = 'dist', names_to = 'iso2') %>%
            dplyr::rename(iso1=Name) %>% 
            dplyr::mutate(dplyr::across(.cols = c(iso1, iso2), as.character))
    }
)

# Load kleborate data
observeEvent(
    input$user_kleborate_data,
    {
        # Read in file and perform validation
        d <- read_file(input$user_kleborate_data$datapath, 'Kleborate')
        # Short circuit eval
        if (is.null(d) || ! kleborate_validate(d, REQUIRED_KLEBORATE_COLS)) {
            dataset$kleborate_data <- NULL
            missing_kleborate_cols <- setdiff(REQUIRED_KLEBORATE_COLS, colnames(d))
            showNotification(paste0('Input Kleborate file is invalid or did not contain required columns: ',
                                    paste(missing_kleborate_cols, collapse = ',')),
                             type='error', duration=10)
            shinyjs::reset('user_kleborate_data')
            return()
        }
        shiny::showNotification('Successfully uploaded kleborate data file', 
                                type = 'message', duration = 2)
        dataset$kleborate_data <- 
            # clean kleborate columns
            clean_kleborate(d) %>% 
            # remove required metadata cols from kleborate df if exists
            dplyr::select(-any_of(REQUIRED_METADATA_COLS)) 
    }
)

