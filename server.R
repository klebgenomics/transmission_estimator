library(shiny)

# Server logic
function(input, output, session) {
    
    dataset <- reactiveValues(
        snp_data = NULL,
        metadata = NULL,
    )
    final_data <- reactiveValues(
        snp_data = NULL,
        metadata = NULL,
    )
    
    ### LOAD DATA ----------------
    source('src/server/load_data_server.R', local = TRUE)
    source('src/server/data_misc_server.R', local = TRUE)
    # Load other server logic
    source('src/server/cluster_server.R', local = TRUE)
    source('src/server/sensitivity_server.R', local = TRUE)
    source('src/server/comparison_server.R', local = TRUE)
    source('src/server/downloads_server.R', local = TRUE)
}
