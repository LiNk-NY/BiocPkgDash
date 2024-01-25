.BIOC_PKG_STATUSES <- c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped", "NA")

#' A Summary Plot for Package Statuses
#'
#' This function generates a stacked bar plot of package statuses for
#' a given Bioconductor version and email combination. It is mainly used
#' for the Bioconductor Package Dashboard.
#'
#' @details Note that binary build stages for the Linux builders are not
#'   included in the plot. This is because the binaries are built on GitHub
#'   Actions and their result are not included in the Bioconductor Build System
#'   (BBS) database.
#'
#' @inheritParams BiocPkgTools::biocMaintained
#'
#' @param status `character()` A vector of `INSTALL`, `build` and `check`
#'   statuses to include in the plot. These values are obtained from the
#'   `result` column in `BiocPkgTools::biocBuildReport()`. The default is all:
#'   `c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped")`.
#'
#' @param stage `character()` A vector of the Bioconductor Build System (BBS)
#'   stages to include in the plot. These values are obtained from the `stage`
#'   `BiocPkgTools::biocBuildReport()`. The default is all stages:
#'   `c("install", "buildsrc", "checksrc", "buildbin")`.
#'
#' @importFrom BiocPkgTools biocMaintained
#' @importFrom ggplot2 ggplot aes geom_col facet_grid coord_flip
#'   scale_fill_manual ggtitle theme element_blank
#' @importFrom dplyr full_join mutate count .data
#' @importFrom tidyr complete
#' @importFrom plotly ggplotly
#'
#' @examples
#' pkgStatusPlot()
#' @export
pkgStatusPlot <-
    function(
        version = BiocManager::version(),
        main = "maintainer@bioconductor\\.org",
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
    names(sdat) <- c("Package", "Builder", "Stage", "Status")

    mainPkgs <- renderMaintained(
        version = version, main = main, pkgType = pkgType
    )
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
    statusPkgs <- complete(
        statusPkgs, .data[["Package"]], .data[["Builder"]], .data[["Stage"]]
    )
    statusPkgs <- full_join(
        statusPkgs,
        count(
            statusPkgs, .data[["Builder"]], .data[["Stage"]], .data[["Status"]]
        ),
        by = c("Builder", "Stage", "Status")
    )
    statusPkgs <- mutate(statusPkgs, Packages = 1)

    cat_colors <-
        c('darkgreen', 'darkorange', 'darkred', 'purple', 'black', 'grey')
    names(cat_colors) <- .BIOC_PKG_STATUSES

    p <- ggplot(
            statusPkgs,
            aes(
                x = .data[["Builder"]], y = .data[["Packages"]],
                label = .data[["Package"]], tooltip = .data[["n"]]
            )
        ) +
        geom_col(aes(fill = .data[["Status"]])) +
        facet_grid(. ~  .data[["Stage"]]) +
        coord_flip() +
        scale_fill_manual(values = cat_colors) +
        ggtitle(paste0("Bioconductor version ", as.character(version))) +
        theme(
            axis.text.x = element_blank()
        )
    ggplotly(p, tooltip = c("label", "n", "Status", "Stage", "Builder"))
}
