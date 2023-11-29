.SHIELDS_URL <- "http://bioconductor.org/shields/build/"
.CHECK_RESULTS_URL <- "http://bioconductor.org/checkResults/"

renderMaintained <- function(email, version) {
    ## annotation badges not supported
    maindf <- biocMaintained(
        main = email,
        version = version,
        pkgType = c("software", "data-experiment", "workflows")
    )
    maindf[["dependencyCount"]] <- as.integer(maindf[["dependencyCount"]])
    maindf
}

renderDF <- function(email, version) {
    version <- BiocManager:::.version_bioc(type = version)
    maindf <- renderMaintained(email = email, version = version)
    sourceType <- vapply(maindf[["biocViews"]], `[[`, character(1L), 1L)
    sourceType <- gsub("AnnotationData", "data-annotation", sourceType)
    sourceType <- gsub("ExperimentData", "data-experiment", sourceType)
    sourceType <- gsub("Workflow", "workflows", sourceType)
    rsn <- BiocPkgTools:::repo_short_names
    shortType <- rsn[
        match(tolower(sourceType), rsn[["repository"]]), "stat.url"
    ]
    version <- c("release", "devel")

    templates <- c(
        paste0(.SHIELDS_URL, version, "/{{shortType}}/{{package}}.svg"),
        paste0(
            .CHECK_RESULTS_URL, version, "/{{shortType}}-LATEST/{{package}}"
        )
    )
    names(templates) <- c("rshield", "dshield", "rresult", "dresult")

    urldf <- .build_urls_temp(
        packages = maindf[["Package"]],
        shortType = shortType,
        templates = templates
    )
    rellink <- .build_html_link(
        urldf, "rshield", "rresult", "release"
    )
    devlink <- .build_html_link(
        urldf, "dshield", "dresult", "devel"
    )

    data.frame(
        Package = maindf[["Package"]],
        `Bioc-release` = rellink,
        `Bioc-devel` = devlink,
        row.names = NULL,
        check.names = FALSE
    )
}

#' @importFrom whisker whisker.render
.build_urls_temp <- function(packages, shortType, templates) {
    .data <- data.frame(
        package = packages,
        shortType = shortType
    )
    result <- lapply(templates, function(template, tdata) {
        apply(tdata, 1L, function(x) {
            whisker::whisker.render(
                data = x,
                template = template
            )
        })
    }, tdata = .data)
    cbind.data.frame(package = .data[["package"]], result)
}

renderDoc <- function(email, version, file) {
    version <- BiocManager:::.version_bioc(type = version)
    maindf <- biocMaintained(
        main = email,
        version = version,
        pkgType = c("software", "data-experiment", "workflows")
    )
    sourceType <- vapply(maindf[["biocViews"]], `[[`, character(1L), 1L)
    sourceType <- gsub("AnnotationData", "data-annotation", sourceType)
    sourceType <- gsub("ExperimentData", "data-experiment", sourceType)
    sourceType <- gsub("Workflow", "workflows", sourceType)
    rsn <- BiocPkgTools:::repo_short_names
    shortType <- rsn[
        match(tolower(sourceType), rsn[["repository"]]), "stat.url"
    ]
    version <- c("release", "devel")
    templates <- c(
        paste0("https://bioconductor.org/packages/{{package}}"),
        paste0(.SHIELDS_URL, version, "/{{shortType}}/{{package}}.svg"),
        paste0(
            .CHECK_RESULTS_URL, version, "/{{shortType}}-LATEST/{{package}}"
        )
    )
    names(templates) <- c("pkgurl", "rshield", "dshield", "rresult", "dresult")
    urldf <- .build_urls_temp(
        packages = maindf[["Package"]],
        shortType = shortType,
        templates = templates
    )

    datalist <- unname(split(urldf, urldf[["package"]]))
    tableTemplate <- c(
        "---",
        "title: Bioconductor Packages",
        "output: html_fragment",
        "---",
        "| Name | Bioc-release | Bioc-devel |",
        "|:-----:|:-----:|:-----:|",
        "{{#packages}}",
        paste0("| [{{{package}}}]({{{pkgurl}}}) |",
            " [![Bioconductor-release Build Status]({{{rshield}}})]({{{rresult}}}) |",
            " [![Bioconductor-devel Build Status]({{{dshield}}})]({{{dresult}}}) |"
        ),
        "{{/packages}}"
    )
    rtext <- whisker::whisker.render(
        template = tableTemplate,
        data = list(packages = datalist)
    )
    mdfile <- tempfile(fileext = ".md")
    writeLines(text = rtext, con = mdfile)
    rmarkdown::render(input = mdfile, output_file = file)
}

.build_html_link <- function(.data, shieldCol, resultCol, version) {
    paste0(
        '<a href=', dQuote(.data[[resultCol]]), ' target="_blank">',
        '<img src=', dQuote(.data[[shieldCol]]),
        ' alt="Bioconductor-', version, ' Build Status"></a>'
    )
}
