emailServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            emailValue <- reactiveVal("maintainer@bioconductor.org")
            observeEvent(input$submit, {
                emailValue(input$email)
            })
            return(emailValue)
        }
    )
}
