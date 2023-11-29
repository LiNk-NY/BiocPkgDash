#' @title The Bioconductor Package Dashboard
#'
#' @description A dashboard for Bioconductor package maintainers to monitor the
#'   status of their packages. The key input to the dashboard is the email the
#'   maintainer uses in their package's `DESCRIPTION` file. The dashboard
#'   displays the badge statuses of the package in both the `release` or `devel`
#'   branches of Bioconductor. The status is determined by the results of the
#'   Bioconductor nightly builds. The dashboard also provides a visualization of
#'   the status of the package checks on either the `release` or `devel`
#'   branches of Bioconductor.
#'
#' @param ... Additional parameters to pass to the `shinyApp()` function.
#'
#' @importFrom BiocPkgTools biocMaintained
#' @import shiny
#'
#' @return called for the side effect of initializing a shiny app
#'
#' @examples
#' if (interactive()) {
#'    BiocPkgDash()
#' }
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
