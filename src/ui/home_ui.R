library(shiny)
library(shinycssloaders)
library(DT)

home_ui <- shiny::tabPanel(
    title = "Home",
    h2("Transmission Estimator", align = "center", style = "margin-top: 60px"),
    shiny::fluidRow(
        # style = "margin-top: 40px;",
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
                HTML("A demo dataset from the <a href='https://pubmed.ncbi.nlm.nih.gov/33782558/'>
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
                  h4("Distance data"),
                  HTML("SNP distance matrix file. This can be generated using 
                       <a href='https://pathogen.watch/'>Pathogenwatch</a>"),
                  div(style='margin-top: 6px;',
                      HTML("<b>Required format:</b>"),
                      br(),
                      HTML("Square matrix format with the first column labelled 'Name'."),
                  ),
              ),
              box(width = 4, class = 'shadow-border-box',
                  h4("Metadata"),
                  HTML("Metadata file containing epidemiological and other information on the isolates."),
                  div(style='margin-top: 6px;',
                      HTML("<b>Required columns:</b>"), 
                      br(),
                      HTML(paste(REQUIRED_METADATA_COLS, collapse = ", "))
                  ),
              ),
          ),
      )  
    ),
    br(), br(),
    # fluidRow(
    #     align = "center",
    #     column(width = 12, 
    #            HTML("Estimates of cases attributable to transmission are reported using interactive plots."),
    #            br(),
    #            HTML("An exploratory sensitivity analysis feature ('Sensitivity' tab) is also included to assist users"),
    #            br(),
    #            HTML("in determining appropriate temporal and genetic distance thresholds"),
    #            br(),
    #     )
    # )
)
