.get_pkgType_from_URL <-
    function(packages, version)
{
    repos <- BiocManager:::.repositories_bioc(version)
    pkgsdb <- available.packages(repos = repos)
    repo_urls <- pkgsdb[rownames(pkgsdb) %in% packages, "Repository"]
    tail_urls <- vapply(
        strsplit(repo_urls, paste0(version, "/")), "[", character(1L), 2L
    )
    biocType <- gsub("/src/contrib", "", tail_urls)
    unname(gsub("/", "-", biocType, fixed = TRUE))
}

.build_html_status <- function() {
    builder_url <- paste0(
        "https://bioconductor.org/checkResults/",
        "{{version}}/{{pkgType}}-LATEST/{{Package}}/",
        "{{Hostname}}-{{Stage}}.html"
    )
    paste0(
        '<a href=', dQuote(builder_url), ' target="_blank">',
        '<span class="icon-status-{{Status}}">{{{svgIcon}}} {{Status}}</span>',
        '</a>'
    )
}

#' Build a table of package build statuses
#'
#' @description This function builds a table of package build statuses for a
#'  given Bioconductor version and email combination. It is mainly used for the
#'  Bioconductor Package Dashboard.
#'
#' @details Note that binary build stages for the Linux builders are marked as
#'   `skipped` in the table. This is because the binaries are built on GitHub
#'   Actions and their result are not included in the Bioconductor Build System
#'   (BBS) database. Provide the `data` argument to avoid recomputing the
#'   list of maintained packages for a given email and Bioconductor version.
#'   Annotation packages are not included in the table because they are not
#'   built regularly by the BBS.
#'
#' @inheritParams BiocPkgTools::biocMaintained
#'
#' @param status `character()` The status of the builders to include in the
#'   table. These values are obtained from the `result` column in
#'   [BiocPkgTools::biocBuildReport()]. The default is all:
#'   `c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped")`.
#'
#' @param stage `character()` A vector of the Bioconductor Build System (BBS)
#'   stages to include in the plot. These values are obtained from the `stage`
#'   [BiocPkgTools::biocBuildReport()]. The default is all stages:
#'   `c("install", "buildsrc", "checksrc", "buildbin")`.
#'
#' @param pkgType `character()` A vector of package types to include in the
#'   table. These values are passed to [BiocPkgTools::biocMaintained()].
#'   The default is all:
#'   `c("software", "data-experiment", "workflows", "data-annotation")`.
#'
#' @param data `tibble()` / `data.frame()` A data frame of maintained packages.
#'   This is used internally to avoid repeated calls to the
#'   [BiocPkgTools::biocMaintained()] function.
#'
#' @export
pkgStatusTable <-
    function(
        main = "maintainer@bioconductor\\.org",
        version = BiocManager::version(),
        status = c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped"),
        stage = c("install", "buildsrc", "checksrc", "buildbin"),
        pkgType = c(
            "software", "data-experiment", "workflows", "data-annotation"
        ),
        data = NULL
    )
{
    status <- match.arg(status, several.ok = TRUE)
    stage <- match.arg(stage, several.ok = TRUE)
    pkgType <- match.arg(pkgType, several.ok = TRUE)

    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(type = version)

    if (is.null(data)) {
        mainPkgs <- renderMaintained(
            version = version, email = main, pkgType = pkgType
        )
    } else {
        mainPkgs <- data
    }

    biocType <- .get_pkgType_from_URL(mainPkgs[["Package"]], version)
    mainPkgs <- dplyr::bind_cols(mainPkgs, pkgType = biocType)
    sdat <-
        BiocPkgTools::biocBuildStatusDB(version = version, pkgType = pkgType)
    names(sdat) <- c("Package", "Hostname", "Stage", "Status")

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
        statusPkgs[["Status"]],
        levels = c("ERROR", "WARNINGS", "TIMEOUT", "OK", "skipped", "NA"),
        labels = c(
            "x-circle", "exclamation-circle", "clock",
            "check2-circle", "dash-circle", "question-circle"
        ),
        ordered = FALSE
    )
    glyphSuffix[is.na(glyphSuffix )] <- "question-circle"
    statusPkgs <- dplyr::bind_cols(statusPkgs, glyphSuffix = glyphSuffix)
    statusPkgs[["Status"]][is.na(statusPkgs[["Status"]])] <- "NA"
    statusPkgs[["svgIcon"]] <- vapply(
        statusPkgs[["glyphSuffix"]],
        function(x) as.character(bsicons::bs_icon(x)),
        character(1L)
    )

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

