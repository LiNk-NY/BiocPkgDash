biocverUI <- function(id, label = "biocver") {
    ns <- NS(id)
    tagList(
        radioButtons(
            inputId = ns("biocver"),
            label = "Bioconductor version:",
            choices = c("release", "devel"),
            selected = "release"
        )
    )
}
