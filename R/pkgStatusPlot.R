.BIOC_PKG_STATUSES <- c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped")

#' @title A Summary Plot for Package Statuses
#'
#' @importFrom BiocPkgTools biocMaintained
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_grid coord_flip
#'   scale_fill_manual ggtitle theme element_blank
#' @importFrom dplyr full_join mutate count
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

    mainPkgs <- biocMaintained(
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
    statusPkgs <- full_join(
        statusPkgs,
        count(
            statusPkgs, Builder, Stage, Status
        ),
        by = c("Builder", "Stage", "Status")
    )
    statusPkgs <- mutate(statusPkgs, Packages = 1)

    p <- ggplot(statusPkgs, aes(x = Builder, y = Packages, label = Package, tooltip = n)) +
        geom_col(aes(fill = Status)) +
        facet_grid(. ~  Stage) +
        coord_flip() +
        scale_fill_manual(
            values = setNames(
                c('darkgreen', 'darkorange', 'darkred', 'purple', 'black'),
                .BIOC_PKG_STATUSES
            )
        ) +
        ggtitle(paste0("Bioconductor version ", as.character(version))) +
        theme(
            axis.text.x = element_blank()
        )
    ggplotly(p, tooltip = c("label", "n", "Status", "Stage", "Builder"))
}
