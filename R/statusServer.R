statusServer <- function(id, email, biocver) {
    moduleServer(
        id,
        function(input, output, session) {
            output$status_out <- plotly::renderPlotly(
                BiocPkgDash::pkgStatusPlot(
                    version = biocver(),
                    main = email()
                )
            )
        }
    )
}

