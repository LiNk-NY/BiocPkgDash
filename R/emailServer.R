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
                    renderDF( email = emailValue() ),
                    escape = FALSE,
                    rownames = FALSE,
                    options = list(
                        pageLength = 100,
                        lengthChange = FALSE
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
                    renderDoc(email = emailValue(), file = file)
                }
            )
        }
    )
}
