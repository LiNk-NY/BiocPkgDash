badgesServer <- function(id, email, biocver) {
    moduleServer(
        id,
        function(input, output, session) {
            output$badge_out <- DT::renderDataTable({
                DT::datatable(
                    renderDF(
                        email = email(),
                        version = biocver()
                    ),
                    escape = FALSE,
                    rownames = FALSE,
                    options = list(
                        dom = "ftp",
                        pageLength = 20,
                        lengthChange = FALSE,
                        paging = TRUE
                    )
                )
            })
        }
    )
}
