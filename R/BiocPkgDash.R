#' @export
BiocPkgDash <- function(...) {
    emailField <- function(id, label = "email") {
        ns <- NS(id)
        tagList(
            textInput(
                inputId = ns("email"),
                label = "Maintainer E-mail",
                placeholder = "maintainer@bioconductor.org"
            ),
            actionButton(
                inputId = ns("submit"),
                label = "Submit",
                class = "btn-primary"
            ),
            DT::dataTableOutput(ns("dash_out"))
        )
    }

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
            }
        )
    }

    ui <- fluidPage(
        emailField("email1")
    )

    server <- function(input, output, session) {
        emailServer("email1")
    }

    shinyApp(ui, server)
}
