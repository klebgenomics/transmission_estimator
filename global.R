suppressPackageStartupMessages({
    # shiny app
    library(shiny)
    library(shinycssloaders)
    library(shinydashboard)
    library(shinyjs)
    library(shinyWidgets)
    # wrangling
    library(tidyverse)
    library(magrittr)
    library(tidyr)
    library(purrr)
    library(glue)
    # filesystem
    library(fs)
    # graph
    library(igraph)
    #plots and rendering
    library(ggplot2)
    library(ggpubr)
    library(ggExtra)
    library(ggnetwork)
    library(htmltools)
    library(DT)
    library(data.table)
    library(kableExtra)
    library(plotly)
    # multithreading
    library(future)
    library(furrr)
    library(parallel)
    library(parallelly)
})

### SHINY APP OPTIONS ----------------------------------------------
# Max file upload size
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB
# Spinner options
options(spinner.type = 5, spinner.color = "#2a77be")


### LOAD FUNCTIONS ------------------------------------------------
purrr::map(fs::dir_ls('src/functions/', glob = "*.R"), source)


### DEMO DATA -----------------------------------------------------
DEMO_SNP_DATA <- "data/demo_data/BARNARDS/BARNARDS_distance_matrix.csv"
DEMO_METADATA <- "data/demo_data/BARNARDS/BARNARDS_metadata.csv"
DEMO_KLEBORATE_DATA <- "data/demo_data/BARNARDS/BARNARDS_kleborate.csv"

### DATA SPECS ---------------------------------------------------
REQUIRED_METADATA_COLS <- c('id', 'Year', 'Month', 'Day', 'Country', 'Site') 
METADATA_ID_COL <- "id"
KLEBORATE_ID_COL <- "Genome Name"
REQUIRED_KLEBORATE_COLS <- c(
    'Genome Name',
    'species', 
    # Res / Vir columns
    'resistance_score', # 'virulence_score',
    # Genome quality columns
    # 'contig_count', 'N50', 'total_size', 
    # K/O columns
    # 'K_locus', 'K_type', 'K_locus_problems', 'K_locus_confidence', 
    # 'O_locus', 'O_type', 'O_locus_problems', 'O_locus_confidence', 
    'ST'
)

### INPUT DEFAULTS / VALIDATION ------------------------------------------
DEFAULT_SNP_DIST <- 10
DEFAULT_TEMP_DIST <- 4 # weeks
MAX_SNP_DIST <- 1000
MAX_TEMP_DIST <- 52 # weeks

### PRELOADED PUBLIC DATA FOR COMPARISONS --------------------------------
PUBLIC_COMP_SNP_AND_EPI_DATA <- dir_ls("data/public_comparison_data/", 
                                       recurse = T, glob = "*_snp_and_epi_data.csv") %>% 
    purrr::map_dfr(readr::read_csv, show_col_types = F)
# Study column identifies unique studies
PUBLIC_COMP_METADATA <- fs::dir_ls("data/public_comparison_data/", 
                                   recurse = T, glob = "*_metadata.csv") %>% 
    purrr::map_dfr(readr::read_csv, show_col_types = F)


