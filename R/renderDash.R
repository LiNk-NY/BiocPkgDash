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

.buildHTMLBadge <- function(package, version) {
    datdf <- data.frame(package = package, version = version)
    shieldtemp <-
        "https://bioconductor.org/shields/build/{{version}}/bioc/{{package}}.svg"
    shieldurl <- whisker::whisker.render(shieldtemp, data = datdf)
    alttemp <- ' alt="Bioconductor-{{version}} Build Status"></a>'
    alttxt <- whisker::whisker.render(
        alttemp,
        data = list(version = version)
    )
    landtemp <-
        "https://bioconductor.org/checkResults/{{version}}/bioc-LATEST/{{package}}/"
    landing <- whisker::whisker.render(landtemp, data = datdf)
    paste0(
        '<a href=', dQuote(landing), ' target="_blank" rel = "noopener">',
        '<img src=', dQuote(shieldurl), alttxt
    )
}

