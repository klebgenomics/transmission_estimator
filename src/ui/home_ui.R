library(shiny)
library(shinycssloaders)
library(DT)

home_ui <- shiny::tabPanel(
    title = "Home",
    h2("Transmission Estimator", align = "center", style = "margin-top: 60px"),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            div(
                align = "center",
                HTML("This tool is designed to identify transmission clusters among neonatal"), 
                br(),
                HTML("sepsis bacterial isolates using genomic (genetic distance)"), 
                br(),
                HTML("and epidemiological (spatiotemporal) data."),
                br(), br(), br(), br(),
                HTML("A demo dataset from the <a href='https://pubmed.ncbi.nlm.nih.gov/33782558/' target='_blank'>
                 BARNARDS study</a> is preloaded for exploration"), 
                br(),
                HTML("of the app's features. Users can also upload their dataset by clicking on"),
                br(),
                HTML("'Upload dataset' on the left panel."),
                br(),
                HTML("Two input files (TSV or CSV format) are required."),
            ),
        )
    ),
    br(), 
    br(), 
    fluidRow(
      column( 
          width = 12,
          div(class = 'centered-items-row',
              align = "center",
              box(width = 4, class = 'shadow-border-box',
                  h4("Genetic distance data"),
                  HTML("SNP distance matrix file. This can be generated using 
                       <a href='https://pathogen.watch/' target='_blank'>Pathogenwatch</a>"),
                  div(style='margin-top: 6px;',
                      br(),
                      HTML("<a href='https://tinyurl.com/TE-demo-data' style='text-decorations:none; color:inherit;'>
                           <b>Required format <i class='fa fa-external-link'></i></b></a>"),
                      br(),
                      HTML("Square matrix format with the first column labelled 'Name'."),
                  ),
              ),
              box(width = 4, class = 'shadow-border-box',
                  h4("Metadata"),
                  HTML("Metadata file containing epidemiological and other information on the isolates."),
                  div(style='margin-top: 6px;',
                      br(),
                      HTML("<a href='https://tinyurl.com/TE-demo-data' style='text-decorations:none; color:inherit;'>
                           <b>Required columns <i class='fa fa-external-link'></i></b></a>"),
                      br(),
                      HTML(paste(REQUIRED_METADATA_COLS, collapse = ", "))
                  ),
              ),
          ),
      )  
    ),
    br(), br(), br(), br(), br(),
    fluidRow(
        align="center",
        column(width=12,
            HTML("For more information, comments and suggestions, 
                 visit the <a href='https://github.com/klebgenomics/transmission_estimator/' target='_blank' alt='GitHub repo'>GitHub repo</a>."),
            br(),
            HTML("If you use this tool, please cite:"),
            br(),
            HTML("1. The GitHub repository: <a href='https://doi.org/10.5281/zenodo.17593948' target='_blank'>
            <img src='https://zenodo.org/badge/DOI/10.5281/zenodo.17593948.svg' alt='DOI'></a>"),
            br(),
            HTML("2. The preprint: Odih et al (2025). <i>MedRxiv</i> <a href='https://www.medrxiv.org/content/10.1101/2025.11.15.25340095v1' target='_blank'>10.1101/2025.11.15.25340095v1
            </a>")
        )
    ),
)
