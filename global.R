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
    library(viridis)
    library(grid)  
    library(gtable)
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

### DATA SPECS ---------------------------------------------------
REQUIRED_METADATA_COLS <- c('id', 'Year', 'Month', 'Day', 'Country', 'Site', 'ST') 
METADATA_ID_COL <- "id"

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

### DEFINITIONS / CAPTIONS ---------------------------------------------------
CLUST_PROP_DEFINITION <- "Cluster proportion: calculated as the number of cases 
in clusters divided by the total number of cases"

TRANSMI_PROP_DEFINITION <- "Transmission proportion: a conservative estimate of 
the proportion of cases attributable to transmission. To calculate this, 
a hypothetical index case is excluded from each identified cluster and all other 
cases within clusters are assumed to be due to onward transmission events. 
The transmission proportion estimate is calculated as: 
(N cluster cases - N clusters) / total N cases"

CLUSTER_PLOT_CAPTION <- "Each point represents one or more cases isolated on specific dates. 
Clusters are represented as groups of cases (points) linked by horizontal lines. 
Clusters belonging to the same sequence type are jittered along the y-axis to 
allow visibility of overlapping clusters. Hover over points to view cluster/case details."
