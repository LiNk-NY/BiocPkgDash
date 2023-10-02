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

renderDoc <- function(email) {
    maindf <- biocMaintained(main = email)
    sourceType <- vapply(maindf[["biocViews"]], `[[`, character(1L), 1L)
    rsn <- BiocPkgTools:::repo_short_names
    badgeKey <- rsn[rsn[["repository"]] == tolower(sourceType), "stat.url"]
    rshields <- paste0(
        .SHIELDS_URL, "release/", badgeKey, maindf[["Package"]], ".svg"
    )
    dshields <- paste0(
        .SHIELDS_URL, "devel/", badgeKey, maindf[["Package"]], ".svg"
    )
    mdf <- cbind.data.frame(
        maindf,
        pkgurl = paste0("https://bioconductor.org/packages/", mdf[["Package"]])
    )
    datalist <- unname(split(adf, adf[["package"]]))
    tableTemplate <- c(
        "| Name | Bioc-release | Bioc-devel |",
        "|:-----:|:-----:|:-----:|",
        "{{#packages}}",
        paste0("| [{{{package}}}]({{{pkgurl}}}) |",
            " [![Bioconductor-release Build Status]({{{rshield}}})] |",
            " [![Bioconductor-devel Build Status]({{{dshield}}})]"
        ),
        "{{/packages}}"
    )
    whisker.render(
        template = tableTemplate,
        data = list(packages = datalist)
    )
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

