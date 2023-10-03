renderDF <- function(email) {
    maindf <- biocMaintained(main = email)
    packages <- maindf[["Package"]]
    release_badges <-
        vapply(packages, .buildHTMLBadge, character(1L), version = "release")
    devel_badges <-
        vapply(packages, .buildHTMLBadge, character(1L), version = "devel")

    data.frame(
        Package = packages,
        `Bioc-release` = unname(release_badges),
        `Bioc-devel` = unname(devel_badges),
        row.names = NULL,
        check.names = FALSE
    )
}

.SHIELDS_URL <- "http://bioconductor.org/shields/build/"
.CHECK_RESULTS_URL <- "http://bioconductor.org/checkResults/"

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

renderDoc <- function(email) {
    maindf <- biocMaintained(main = email)
    sourceType <- vapply(maindf[["biocViews"]], `[[`, character(1L), 1L)
    rsn <- BiocPkgTools:::repo_short_names
    shortType <- rsn[
        match(tolower(sourceType), rsn[["repository"]]), "stat.url"
    ]

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
    outhtml <- tempfile(fileext = ".html")
    writeLines(text = rtext, con = mdfile)
    rmarkdown::render(input = mdfile, output_file = outhtml)
}

.buildHTMLBadge <- function(package, version) {
    datdf <- data.frame(package = package, version = version)
    shieldtemp <- paste0(
        "https://bioconductor.org/shields/build",
        "/{{version}}/bioc/{{package}}.svg"
    )
    shieldurl <- whisker::whisker.render(shieldtemp, data = datdf)
    alttemp <- ' alt="Bioconductor-{{version}} Build Status"></a>'
    alttxt <- whisker::whisker.render(
        alttemp,
        data = list(version = version)
    )
    landtemp <- paste0(
        "https://bioconductor.org/checkResults/",
        "{{version}}/bioc-LATEST/{{package}}/"
    )
    landing <- whisker::whisker.render(landtemp, data = datdf)
    paste0(
        '<a href=', dQuote(landing), ' target="_blank">',
        '<img src=', dQuote(shieldurl), alttxt
    )
}

