badgesUI <- function(id, label = "badges") {
    ns <- NS(id)
    tagList(
        DT::dataTableOutput(ns("badge_out"))
    )
}
