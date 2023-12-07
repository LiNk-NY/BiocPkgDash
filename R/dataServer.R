dataServer <- function(id, email, biocver) {
    moduleServer(
        id,
        function(input, output, session) {
            colsOfInterest <- c(
                "Package", "Version", "License", "NeedsCompilation", "Title",
                "hasREADME", "hasNEWS", "hasINSTALL", "hasLICENSE",
                "dependencyCount"
            )
            output$data_out <- DT::renderDataTable({
                DT::datatable(
                    BiocPkgDash:::renderMaintained(
                        email = email(), version = biocver()
                    )[, colsOfInterest],
                    rownames = FALSE,
                    options = list(
                        dom = "ftp",
                        pageLength = 20,
                        paging = TRUE
                    )
                )
            })
        }
    )
}
