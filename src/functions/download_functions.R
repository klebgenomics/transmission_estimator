### Helper functions from Kleborate-viz ------------------
# Icon (download) button 
IconButton <- function(outputId, type, ...) {
    if (type == 'data_dl') {
        s.class <- 'shiny-download-link'
        icon <- icon('table')
    } else if (type == 'graph_modal') {
        s.class <- 'action-button'
        icon <- icon('chart-area')
    } else {
        stop('Got bad IconButton type')
    }
    aTag <- tags$a(
        id=outputId,
        class=paste('btn btn-default', s.class),
        style='padding: 2px 4px;',
        href='',
        target='_blank',
        download=NA,
        icon,
        '',
        ...
    )
}
# Plot download UI/code
DownloadModal <- function(download_button, m.title = 'Download plot (PNG)') {
    shiny::showModal(
        shiny::modalDialog(
            title=m.title,
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::numericInput('plot_dl_width', 'Width (px)', 1000, step = 10),
                ),
                shiny::column(
                    6,
                    shiny::numericInput('plot_dl_height', 'Height (px)', 600, step = 10),
                ),
            ),
            size='s',
            easyClose=TRUE,
            footer=div(
                style='margin-top: 15px',
                div(style='float: left', modalButton('Close')),
                div(style='float: right', download_button),
            ),
        )
    )
}
# download plot
download_plot <- function(plot, s.filename, width, height, scale = 5) {
    reticulate::py_run_string("import sys") # needed to fix kaleido bug
    plotly::save_image(plot, s.filename, width=width,
                       height=height, scale = scale)
}