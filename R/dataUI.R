dataUI <- function(id, label = "data") {
    ns <- NS(id)
    tagList(
        DT::dataTableOutput(ns("data_out"))
    )
}
