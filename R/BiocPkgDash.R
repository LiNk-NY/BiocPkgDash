#' @importFrom BiocPkgTools biocMaintained
#' @export
BiocPkgDash <- function(...) {
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
