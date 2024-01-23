
#' @export
pkgStatusTable <-
    function(
        main = "maintainer@bioconductor\\.org",
        version = BiocManager::version(),
        status = c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped"),
        stage = c("install", "buildsrc", "checksrc", "buildbin"),
        pkgType = c(
            "software", "data-experiment",
            "workflows", "data-annotation"
        )
    )
{
    status <- match.arg(status, several.ok = TRUE)
    stage <- match.arg(stage, several.ok = TRUE)
    pkgType <- match.arg(pkgType, several.ok = TRUE)

    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(type = version)

    build_status_db <- BiocPkgTools:::get_build_status_db_url(version)
    status_file <- BiocPkgTools:::.cache_url_file(build_status_db)
    dat <- readLines(status_file)
    sdat <- strsplit(dat, "#|:\\s")
    sdat <- do.call(
        function(...) {
            rbind.data.frame(..., row.names = NULL)
        },
        sdat
    )
    names(sdat) <- c("Package", "Hostname", "Stage", "Status")

    mainPkgs <- biocMaintained(
        version = version, main = main, pkgType = pkgType
    )
    avail_pkgs <- available.packages(repos = BiocManager::repositories())
    repo_urls <- avail_pkgs[
        rownames(avail_pkgs) %in% mainPkgs[["Package"]], "Repository"
    ]

    pkgURL <- basename(gsub("/src/contrib", "", repo_urls))
    mainPkgs <- dplyr::bind_cols(mainPkgs, pkgType = pkgURL)

    lmain <- sdat[["Package"]] %in% mainPkgs[["Package"]]
    lstage <- sdat[["Stage"]] %in% stage
    lstatus <- sdat[["Status"]] %in% status
    statusPkgs <- sdat[lmain & lstage & lstatus, ]
    if (!length(statusPkgs))
        stop("No packages found with maintainer: ", main)

    statusPkgs[["Stage"]] <- factor(
        statusPkgs[["Stage"]],
        levels = c("install", "buildsrc", "checksrc", "buildbin"),
        ordered = TRUE
    )
    statusPkgs[["Status"]] <- factor(
        statusPkgs[["Status"]],
        levels = .BIOC_PKG_STATUSES,
        ordered = TRUE
    )
    statusPkgs <- tidyr::complete(
        statusPkgs,
        .data[["Package"]], .data[["Hostname"]], .data[["Stage"]]
    )

    statusPkgs <- dplyr::left_join(
        statusPkgs,
        mainPkgs[, c("Package", "pkgType")],
        by = c("Package" = "Package")
    )

    statusPkgs[["StageLabel"]] <- factor(
        statusPkgs[["Stage"]],
        levels = c("install", "buildsrc", "checksrc", "buildbin"),
        labels = c("Install", "Build Source", "Check Source", "Build Binary"),
        ordered = TRUE
    )
    glyphSuffix <- factor(
        statusPkgs$Status,
        levels = c("ERROR", "WARNINGS", "TIMEOUT", "OK", "skipped", "question-sign"),
        labels = c(
            "remove-circle", "warning-sign", "time",
            "ok-circle", "minus-sign", "question-sign"
        ),
        ordered = FALSE
    )
    glyphSuffix [is.na(glyphSuffix )] <- "question-sign"
    statusPkgs <- dplyr::bind_cols(statusPkgs, glyphSuffix = glyphSuffix)

    build_urls <- apply(
        statusPkgs,
        1L,
        function(x, ver) {
            whisker::whisker.render(
                template = .build_html_status(),
                data = c(as.list(x), version = ver)
            )
        }, ver = as.character(version)
    )
    statusPkgs[["build_url"]] <- build_urls

    dplyr::select(
        statusPkgs, "Package", "Hostname", "StageLabel", "build_url"
    ) |>
    tidyr::pivot_wider(
        names_from = "StageLabel", values_from = "build_url"
    )
}

.build_html_status <- function() {
    builder_url <- paste0(
        "https://bioconductor.org/checkResults/",
        "{{version}}/{{pkgType}}-LATEST/{{Package}}/",
        "{{Hostname}}-{{Stage}}.html"
    )
    paste0(
        '<a href=', dQuote(builder_url), ' target="_blank">',
        '<i class="glyphicon glyphicon-{{glyphSuffix}}">{{Status}}</i>',
        '</a>'
    )
}
