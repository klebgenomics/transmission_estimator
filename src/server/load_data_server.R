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
    } else if (input$data_option == "Upload dataset") {
        # Reset dataset values to NULL when 'Upload data' is selected
        dataset$snp_data <- NULL
        dataset$metadata <- NULL
        # dataset$kleborate_data <- NULL
        shinyjs::reset('user_metadata')
        shinyjs::reset('user_distances')
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
        }
        shiny::showNotification('Successfully uploaded metadata file', 
                                type = 'message', duration = 2)
        # check rows with invalid dates 
        n_invalid_date <- invalid_date_rows(d)
        if (n_invalid_date > 0){
            shiny::showNotification(
                paste0(n_invalid_date, " rows with missing or invalid date columns 
                       ('Year', 'Month', and 'Day') detected in metadata file. 
                       Please check as this may affect clustering results"),
                type = 'warning', duration = 10)
        }
        # remove duplicate rows
        meta_dups <- sum(duplicated(d$id))
        if (meta_dups > 0){
            shiny::showNotification(
                paste0("Found and removed ", meta_dups, " metadata rows with duplicate ID"),
                type = 'warning', duration = 10)
            d %<>% dplyr::distinct(id, .keep_all = TRUE)
        }
        # set metadata
        dataset$metadata <- d 
    }
)

# Load distance data
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
        if(!names(d)[1] == "Name"){
            shiny::showNotification('First column of the distance matrix must be labelled "Name"', 
                                    type='error', duration=10)
            dataset$snp_data <- NULL
            shinyjs::reset('user_distances')
            return()
        }
        if(!all(names(d)[-1] %in% d$Name)){
            shiny::showNotification('Mismatched samples in rows and columns: 
                                    Please ensure that all samples in the rows have a 
                                    corresponding column in the distance matrix.', 
                                    type='error', duration=10)
            dataset$snp_data <- NULL
            shinyjs::reset('user_distances')
            return()
        }
        shiny::showNotification('Successfully uploaded distances file', 
                                type='message', duration=2)
        dataset$snp_data <- d %>% 
            tidyr::pivot_longer(cols = !Name, values_to = 'dist', names_to = 'iso2') %>%
            dplyr::rename(iso1=Name) %>% 
            dplyr::mutate(dplyr::across(.cols = c(iso1, iso2), as.character))
    }
)

