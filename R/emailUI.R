emailUI <- function(id, label = "email") {
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
        )
    )
}
