suppressPackageStartupMessages({
    # shiny app
    library(shiny)
    library(shinycssloaders)
    library(shinyjs)
    library(shinyWidgets)
    # wrangling
    library(tidyverse)
    library(glue)
    # filesystem
    library(fs)
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
})

### Shiny app options ----------------------------------------------
# Max file upload size
options(shiny.maxRequestSize = 100*1024^2)  # 100 MB

### Load functions ----------------------------------------------
purrr::map(fs::dir_ls('src/functions/', glob = "*.R"), source)


### Demo data ----------------------------------------------
DEMO_SNP_DATA <- "data/demo_data/BARNARDS/BARNARDS_distance_matrix.csv"
DEMO_METADATA <- "data/demo_data/BARNARDS/BARNARDS_metadata.csv"
DEMO_KLEBORATE_DATA <- "data/demo_data/BARNARDS/BARNARDS_kleborate.csv"

### Specs ---------------------------------------------------
REQUIRED_METADATA_COLS <- c('id', 'Year', 'Month', 'Day', 'Country') # Require Site instead?

METADATA_ID_COL <- "id"
KLEBORATE_ID_COL <- "Genome Name"

REQUIRED_KLEBORATE_COLS <- c(
    'Genome ID',
    'Genome Name',
    # 'Version',	'Kleborate version',
    'species', 
    # Genome quality columns
    # 'species_match', 'contig_count', 'N50', 'largest_contig', 
    # 'total_size', 'ambiguous_bases', 'QC_warnings', 
    # K/O columns
    # 'wzi', 'K_locus', 'K_type', 'K_locus_problems', 'K_locus_confidence', 
    # 'K_locus_identity', 'K_locus_missing_genes', # K
    # 'O_locus',	'O_type',	'O_locus_problems',	'O_locus_confidence', 
    # 'O_locus_identity', 'O_locus_missing_genes' # O
    'ST'
)

KLEBORATE_FILTER_COLS <- c(
    'species',
    'ST',
    'K_locus',
    'O_locus'
)

GENOME_FILTER_COLS <- c(
    "species_match", "contig_count", "N50", "largest_contig", "total_size"
)





