library(tidyverse)
library(magrittr)
library(lubridate)

# From Kleborate-Viz
# Determine file format and read data
read_file <- function(fp, input_name) {
    if (file.info(fp)$size < 1) {
        showNotification(
            paste('Input', input_name, 'file does not contain any data'),
            type='error',
            duration=NULL
        )
        return(NULL)
    }
    if (input_name=='Kleborate' & grepl('.txt$', fp)) {
        d <- readr::read_tsv(fp, show_col_types =FALSE)
    } else if (grepl('.csv$', fp)) {
        d <- readr::read_csv(fp, show_col_types=FALSE)
    } else if (grepl('.tsv$', fp)) {
        d <- readr::read_tsv(fp, show_col_types=FALSE)
    } else {
        showNotification(
            paste('Input', input_name, 'file must be in tsv or csv format and have the correct extension'),
            type='error', duration=NULL
        )
        return(NULL)
    }
    # Require some data, any data
    if (nrow(d) < 1) {
        showNotification(
            paste('Input', input_name, 'file did not have any rows'),
            type='error',
            duration=NULL
        )
        kleborate_reset()
        return(NULL)
    }
    if (ncol(d) < 1) {
        showNotification(
            paste('Input', input_name, 'file did not have any columns'),
            type='error',
            duration=NULL
        )
        return(NULL)
    }
    return(d)
}


# validate kleborate data
kleborate_validate <- function(d) {
    if (! all(REQUIRED_KLEBORATE_COLS %in% colnames(d)) ) {
        # First check presence of default columns
        missing_kleborate_cols <- setdiff(REQUIRED_KLEBORATE_COLS, colnames(d))
        showNotification(paste0('Input Kleborate file did not contain required columns: ',
                                paste(missing_kleborate_cols, collapse = ',')),
                         type='error', duration=NULL)
        return(FALSE)
    } else {
        return(TRUE)
    }
}


read_snp_csv <- function(distance_matrix_csv_path){
    tryCatch({
        snp_data <- read_csv(distance_matrix_csv_path, show_col_types = F)
    }, error = function(e) {
        stop("Error reading the SNP data file. Please check that it is a valid CSV file.")
    })
    # check valid matrix
    if(nrow(snp_data) + 1 != ncol(snp_data)){
        stop('Number of rows and cols in snp data must be the same')
    }
    if(!'Name' %in% names(snp_data)){
        stop('Name column not in snp_data, is it from Pathogenwatch?')
    }
    return(
        snp_data %>% 
            pivot_longer(cols = !Name, values_to = 'dist', names_to = 'iso2') %>%
            rename(iso1=Name)
    )
}

read_kleborate_data_csv <- function(kleborate_data_path){
    tryCatch({
        kleborate_data <- read_csv(kleborate_data_path, show_col_types = F)
    }, error = function(e) {
        stop("Error reading the kleborate data file. Please check that it is a valid CSV file.")
    })
    # TODO: validate kleborate data columns
    return(kleborate_data)
}

read_metadata_csv <- function(metadata_path){
    tryCatch({
        metadata <- read_csv(metadata_path, show_col_types = F)
    }, error = function(e) {
        stop("Error reading the metadata file. Please check that it is a valid CSV file.")
    })
    if (! all(REQUIRED_METADATA_COLS %in% names(metadata)) ) {
        stop(paste0("The following columns are required in the metadata file: ", 
                    paste(REQUIRED_METADATA_COLS, collapse = ", ")))
    }
    return(metadata)
}

filter_data <- function(d, filter_column, filter_values){
    if(is.null(filter_column)){return(d)}
    if(is.null(filter_values)){return(d)}
    if(!filter_column %in% names(d)){return(d)}
    d <- d %>% dplyr::filter(!!rlang::sym(filter_column) %in% filter_values)
    return(d)
}

format_sample_dates <- function(metadata){
    if(! all(c('id', 'Year', 'Month', 'Day') %in% names(metadata)) ) {
        stop(paste0("'id', 'Year', 'Month', and 'Day' columns are required."))
    }
    sample_dates <- metadata %>% dplyr::select(id, Year, Month, Day) %>% 
        dplyr::filter(!(is.na(Year) | is.na(Month) | is.na(Day))) %>% 
        dplyr::rowwise() %>%  
        dplyr::mutate(formatted_date = paste0(Year, "-", Month, "-", Day)) %>% 
        dplyr::mutate(formatted_date = lubridate::as_date(formatted_date)) %>% 
        # dplyr::mutate(decimal_date = lubridate::decimal_date(formatted_date)) %>% 
        dplyr::select(id, formatted_date) %>% 
        dplyr::ungroup()
    return(sample_dates)
}

get_snp_and_temporal_data <- function(snp_data, sample_dates){
    snp_and_temporal_data <- snp_data %>% 
        dplyr::left_join(sample_dates, by = c('iso1' = 'id')) %>% rename('iso1_date' = 'formatted_date') %>% 
        dplyr::left_join(sample_dates, by = c('iso2' = 'id')) %>% rename('iso2_date' = 'formatted_date') %>% 
        dplyr::mutate(days = abs(floor(as.numeric(
                    difftime(iso1_date, iso2_date, units = "days")
                )))
            ) %>% 
        dplyr::select(iso1, iso2, dist, days)
    return(snp_and_temporal_data)
}

get_snp_and_epi_data <- function(snp_data, sample_dates, metadata, 
                                 geo_column = "Country"){
    if (length(geo_column) > 1){stop("Can only supply one geo_column")}
    if(!geo_column %in% names(metadata)){
        stop(glue::glue("The specified '{geo_column}' column is missing from metadata"))
    }
    geo_data <- metadata %>% select(id, all_of(geo_column))
    colnames(geo_data) <- c("id", "geo")
    snp_and_epi_data <- snp_data %>% 
        dplyr::mutate(iso1 = as.character(iso1), 
                      iso2 = as.character(iso2)) %>%
        # remove duplicate pairs
        dplyr::filter( ! iso1 == iso2 ) %>%
        dplyr::mutate(L1 = if_else(iso1 < iso2, iso1, iso2)) %>% 
        dplyr::mutate(L2 = if_else(iso1 > iso2, iso1, iso2)) %>% 
        dplyr::mutate(pairs = paste0(L1, "_", L2)) %>% 
        dplyr::distinct(pairs, .keep_all = TRUE) %>% dplyr::select(-c(L1, L2, pairs)) %>%
        # days between isolation
        dplyr::left_join(sample_dates, by = c('iso1' = 'id')) %>% dplyr::rename('iso1_date' = 'formatted_date') %>% 
        dplyr::left_join(sample_dates, by = c('iso2' = 'id')) %>% dplyr::rename('iso2_date' = 'formatted_date') %>% 
        dplyr::mutate(days = abs(floor(as.numeric(
            difftime(iso1_date, iso2_date, units = "days")
        )))) %>% 
        dplyr::select(-c(iso1_date, iso2_date)) %>% 
        # check isolates with same or different location
        dplyr::left_join(geo_data, by = c('iso1' = 'id')) %>% dplyr::rename('geo1' = 'geo') %>% 
        dplyr::left_join(geo_data, by = c('iso2' = 'id')) %>% dplyr::rename('geo2' = 'geo') %>% 
        dplyr::mutate(pair_location = if_else(geo1 == geo2, "Same", "Different")) %>% 
        dplyr::select(-c(geo1, geo2))
    
    
    return(snp_and_epi_data)
}


    
    
    

