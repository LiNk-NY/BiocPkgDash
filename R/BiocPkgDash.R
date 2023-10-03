#' @importFrom BiocPkgTools biocMaintained
#' @export
BiocPkgDash <- function(...) {
    emailField <- function(id, label = "email") {
        ns <- NS(id)
        tagList(
            textInput(
                inputId = ns("email"),
                label = "Enter maintainer e-mail",
                placeholder = "maintainer@bioconductor.org"
            ),
            actionButton(
                inputId = ns("submit"),
                label = "Submit",
                class = "btn-primary"
            ),
            downloadButton(
                outputId = ns("btnSend"),
                label = "Download HTML"
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

    ui <- fluidPage(
        titlePanel(
            windowTitle = "BiocPkgDash",
            title = div(
                img(
                    src = "images/bioconductor_logo_rgb_small.png",
                    align = "right",
                    style = "margin-right:10px"
                ),
                h1(id = "big-heading", "Bioconductor Package Dashboard")
            )
        ),
        emailField("email1")
    )

    server <- function(input, output, session) {
        emailServer("email1")
    }

    shinyApp(ui, server)
}
