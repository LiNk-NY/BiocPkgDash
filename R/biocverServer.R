biocverServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            reactive(input$biocver)
        }
    )
}
