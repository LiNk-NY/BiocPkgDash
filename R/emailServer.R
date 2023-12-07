emailServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {

            emailValue <- reactiveVal("maintainer@bioconductor.org")
            observeEvent(input$submit, {
                emailValue(input[["email"]])
            })

            output$dash_out <- DT::renderDataTable({
                DT::datatable(
                    BiocPkgDash:::renderDF(
                        email = emailValue(),
                        version = input[["biocver"]]
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

            output$btnSend <- downloadHandler(
                filename = function() {
                    em <- gsub("@", "_at_", emailValue())
                    em <- gsub("\\.", "_dot_", em)
                    paste0(em, ".html")
                },
                content = function(file) {
                    BiocPkgDash:::renderDoc(
                        email = emailValue(),
                        version = input[["biocver"]],
                        file = file
                    )
                }
            )

            output$pkgStatus <- plotly::renderPlotly(
                BiocPkgDash::pkgStatusPlot(
                    main = emailValue(),
                    version = input$biocver
                )
            )
        }
    )
}
