emailField <- function(id, label = "email") {
    ns <- NS(id)
    tagList(
        radioButtons(
            inputId = ns("biocver"),
            label = "Bioconductor version:",
            choices = c("release", "devel"),
            selected = "release"
        ),
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
