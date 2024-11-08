.SHIELDS_URL <- "http://bioconductor.org/shields/build/"
.CHECK_RESULTS_URL <- "http://bioconductor.org/checkResults/"

renderMaintained <- function(
    email,
    version,
    pkgType = c("software", "data-experiment", "workflows", "data-annotation")
) {
    ## annotation badges not supported
    pkgType <- pkgType[pkgType != "data-annotation"]
    maindf <- biocMaintained(
        main = email, version = version, pkgType = pkgType
    )
    maindf[["dependencyCount"]] <- as.integer(maindf[["dependencyCount"]])
    if (!nrow(maindf))
        stop("No packages found with maintainer: ", email)
    maindf
}

badgesDF <- function(email, data = NULL) {
    version <- BiocManager:::.version_bioc(type = "devel")
    if (is.null(data)) {
        maindf <- renderMaintained(email = email, version = version)
    } else {
        maindf <- data
    }
    pkgType <- .get_pkgType_from_URL(maindf[["Package"]], version)
    version <- c("release", "devel")

    templates <- c(
        paste0(.SHIELDS_URL, version, "/{{pkgType}}/{{package}}.svg"),
        paste0(
            .CHECK_RESULTS_URL, version, "/{{pkgType}}-LATEST/{{package}}"
        )
    )
    names(templates) <- c("rshield", "dshield", "rresult", "dresult")

    ## adjust for missing package types
    maindf <- maindf[match(names(pkgType), maindf[["Package"]]), ]
    urldf <- .build_urls_temp(
        packages = maindf[["Package"]],
        pkgType = pkgType,
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
.build_urls_temp <- function(packages, pkgType, templates) {
    .data <- data.frame(
        package = packages,
        pkgType = pkgType
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

renderHTMLfrag <- function(email, file, data = NULL) {
    version <- BiocManager:::.version_bioc(type = "devel")

    if (is.null(data)) {
        maindf <- renderMaintained(email = email, version = version)
    } else {
        maindf <- data
    }
    pkgType <- .get_pkgType_from_URL(maindf[["Package"]], version)

    version <- c("release", "devel")
    templates <- c(
        paste0("https://bioconductor.org/packages/{{package}}"),
        paste0(.SHIELDS_URL, version, "/{{pkgType}}/{{package}}.svg"),
        paste0(
            .CHECK_RESULTS_URL, version, "/{{pkgType}}-LATEST/{{package}}"
        )
    )
    names(templates) <- c("pkgurl", "rshield", "dshield", "rresult", "dresult")
    ## adjust for missing package types
    maindf <- maindf[match(names(pkgType), maindf[["Package"]]), ]
    urldf <- .build_urls_temp(
        packages = maindf[["Package"]],
        pkgType = pkgType,
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
