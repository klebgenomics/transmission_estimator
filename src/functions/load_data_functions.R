library(tidyverse)
library(magrittr)
library(lubridate)


# Determine file format and read data (from Kleborate-Viz)
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
kleborate_validate <- function(d, required_kleborate_cols) {
    if (! all(required_kleborate_cols %in% colnames(d)) ) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

# clean values in selected kleborate data columns
clean_kleborate <- function(kleborate_data){
    if ('K_locus' %in% names(kleborate_data)){
        kleborate_data %<>% 
            dplyr::mutate(K_locus_original = K_locus) %>%
            dplyr::mutate(K_locus = dplyr::if_else(K_locus == "unknown (KL107)", 
                                                   "nontypeable", K_locus)) %>%
            dplyr::mutate(K_locus = stringr::str_replace(K_locus, "unknown \\(", "")) %>%
            dplyr::mutate(K_locus = stringr::str_replace(K_locus, "\\)", ""))
        if ('K_locus_confidence' %in% names(kleborate_data)){
            kleborate_data %<>%
                dplyr::mutate(K_locus = dplyr::if_else(K_locus =="KL107" & K_locus_confidence=="None",
                                                       "nontypeable", K_locus))
        }
    }
    if ('O_type' %in% names(kleborate_data)){
        kleborate_data %<>% 
            dplyr::mutate(O_type = dplyr::if_else(startsWith(O_type,"unknown"), "unknown", O_type))
    }
    # Genome name column as character
    if ('Genome Name' %in% names(kleborate_data)){
        kleborate_data %<>% dplyr::mutate(dplyr::across(.cols = c(`Genome Name`), as.character))
    }
    return(kleborate_data)
}

# filter dataframe given column name (filter_column) and values to include (filter_values)
filter_data <- function(d, filter_column, filter_values){
    if(is.null(filter_column)){return(d)}
    if(is.null(filter_values)){return(d)}
    if(!filter_column %in% names(d)){return(d)}
    d <- d %>% dplyr::filter(!!rlang::sym(filter_column) %in% filter_values)
    return(d)
}


# Functions to load local data
read_snp_csv <- function(distance_matrix_csv_path){
    tryCatch({
        snp_data <- readr::read_csv(distance_matrix_csv_path, show_col_types = F)
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
            tidyr::pivot_longer(cols = !Name, values_to = 'dist', names_to = 'iso2') %>%
            dplyr::rename(iso1=Name) %>% 
            dplyr::mutate(dplyr::across(.cols = c(iso1, iso2), as.character))
    )
}

read_kleborate_data_csv <- function(kleborate_data_path, required_kleborate_cols, clean = T){
    tryCatch({
        d <- read_csv(kleborate_data_path, show_col_types = F)
    }, error = function(e) {
        stop("Error reading the kleborate data file. Please check that it is a valid CSV file.")
    })
    if (is.null(d) || ! kleborate_validate(d, required_kleborate_cols)) {
        missing_kleborate_cols <- setdiff(REQUIRED_KLEBORATE_COLS, colnames(d))
        stop(paste0('Input Kleborate file is invalid or did not contain required columns: ',
                    paste(missing_kleborate_cols, collapse = ',')))
    }
    if(clean){d <- clean_kleborate(d)}
    return(d)
}

read_metadata_csv <- function(metadata_path, required_cols = c('id')){
    tryCatch({
        d <- read_csv(metadata_path, show_col_types = F)
    }, error = function(e) {
        stop("Error reading the metadata file. Please check that it is a valid CSV file.")
    })
    if (! all(required_cols %in% names(d)) ) {
        stop(paste0("The following columns are required in the metadata file: ", 
                    paste(required_cols, collapse = ", ")))
    } else {
        # Standardise (rename) mandatory column names
        col_inds <- match(tolower(required_cols), tolower(names(d)))
        colnames(d)[col_inds] <- required_cols
        # id column always character
        d %<>% dplyr::mutate(dplyr::across(.cols = c(id), as.character))
    }
    return(d)
}

# format sample dates; filter out NA rows
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
        dplyr::mutate(weeks = abs(floor(as.numeric(
                    difftime(iso1_date, iso2_date, units = "weeks")
                )))
            ) %>% 
        dplyr::select(iso1, iso2, dist, weeks)
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
        # weeks between isolation
        dplyr::left_join(sample_dates, by = c('iso1' = 'id')) %>% dplyr::rename('iso1_date' = 'formatted_date') %>% 
        dplyr::left_join(sample_dates, by = c('iso2' = 'id')) %>% dplyr::rename('iso2_date' = 'formatted_date') %>% 
        dplyr::mutate(weeks = abs(floor(as.numeric(
            difftime(iso1_date, iso2_date, units = "weeks")
        )))) %>% 
        dplyr::select(-c(iso1_date, iso2_date)) %>% 
        # check isolates with same or different location
        dplyr::left_join(geo_data, by = c('iso1' = 'id')) %>% dplyr::rename('geo1' = 'geo') %>% 
        dplyr::left_join(geo_data, by = c('iso2' = 'id')) %>% dplyr::rename('geo2' = 'geo') %>% 
        dplyr::mutate(pair_location = if_else(geo1 == geo2, "Same", "Different")) %>% 
        dplyr::select(-c(geo1, geo2))
    return(snp_and_epi_data)
}

# Names to include in selectInput options (names from metadata and kleborate_data [optional]) 
select_metadata_and_kleborate_var_choices <- function(metadata, kleborate_data = NULL){
    EXCLUDE_VARS <- c('id', 'Day', 'Month', 'Year', 'Date', 'Cluster')
    INCLUDE_VARS <- c('resistance_score', 'virulence_score', 'species', 
                      'K_locus', 'O_locus', 'ST')
    # select character vars from metadata
    choices <-  metadata %>% dplyr::select(where(is.character)) %>% names()
    if (!is.null(kleborate_data)){
        # Only include selected vars from kleborate data
        choices <- c(choices, kleborate_data %>% 
                         dplyr::select(any_of(INCLUDE_VARS)) %>% names() 
                     )
    }
    # Exclude vars (case insensitive)
    choices <- choices[!tolower(choices) %in% tolower(EXCLUDE_VARS)] %>% 
        unique() %>% as.character()
    return(choices)
}

summarise_dataset <- function(metadata, kleborate_data, matching_ids){
    tibble::tribble(
        ~name, ~value,
        'N Samples', length(matching_ids) %>% as.character(),
        'N Countries', n_distinct(metadata$Country, na.rm = T) %>% as.character(),
        'N Sites', n_distinct(metadata$Site, na.rm = T) %>% as.character(),
        'N STs', n_distinct(kleborate_data$ST, na.rm = T) %>% as.character(),
        'Years', if (all(is.na(metadata$Year))){NA_character_} else {
            metadata %>% 
                reframe(years = paste0(range(Year, na.rm = T), 
                                       collapse = " - ")) %>% 
                pull(years)}
    )
}

    
    
    

