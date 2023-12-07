.PACKAGE_NAME <- "BiocPkgDash"

aboutPanel <- function() {
    sessionText <-
        if (requireNamespace("sessioninfo", quietly = TRUE))
            "sessioninfo::session_info()"
    else
        "utils::sessionInfo()"
    bioc_version <-
        if (requireNamespace("BiocManager", quietly = TRUE))
            as.character(BiocManager::version())
    else
        "version not available"
    pkgVer <- as.character(utils::packageVersion(.PACKAGE_NAME))
    HTML(paste0(
        h4(.PACKAGE_NAME),
        p("Package version: ", strong(pkgVer)),
        p("Bioconductor version: ", strong(bioc_version)),
        p("Last updated: ", strong("2023-12-07")),
        span("Source: ", a(
            paste0("https://github.com/Bioconductor/", .PACKAGE_NAME),
            href=paste0(
                "https://github.com/Bioconductor/", .PACKAGE_NAME
            )
        )),
        hr(),
        "<details style='margin-bottom:10px;'>", "<summary>",
        "&#9654; Session Info",
        "</summary>",
        "<pre class='r'><code>", sessionText,
        verbatimTextOutput("sessioninfo"),
        "</code></pre></details>"
    ))
}
