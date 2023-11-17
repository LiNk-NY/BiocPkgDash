.BIOC_PKG_STATUSES <- c("OK", "WARNINGS", "ERROR", "TIMEOUT", "skipped")

#' @importFrom BiocPkgTools biocMaintained
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_grid coord_flip
#'   scale_fill_manual
#'
#' @importFrom plotly ggplotly
#'
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
    names(sdat) <- c("package", "builder", "stage", "status")

    mainPkgs <- biocMaintained(
        version = version, main = main, pkgType = pkgType
    )
    lmain <- sdat[["package"]] %in% mainPkgs[["Package"]]
    lstage <- sdat[["stage"]] %in% stage
    lstatus <- sdat[["status"]] %in% status
    statusPkgs <- sdat[lmain & lstage & lstatus, ]
    if (!length(statusPkgs))
        stop("No packages found with maintainer: ", main)
    longSummary <- statusPkgs |> dplyr::count(builder, stage, status)
    longSummary[["stage"]] <- factor(
        longSummary[["stage"]],
        levels = c("install", "buildsrc", "checksrc", "buildbin"),
        ordered = TRUE
    )
    longSummary[["status"]] <- factor(
        longSummary[["status"]],
        levels = .BIOC_PKG_STATUSES,
        ordered = TRUE
    )

    p <- ggplot(longSummary, aes(builder, y = n)) +
        geom_col(aes(fill = status)) +
        facet_grid(. ~  stage) +
        coord_flip() +
        scale_fill_manual(
            values = setNames(
                c('darkgreen', 'darkorange', 'darkred', 'purple', 'black'),
                .BIOC_PKG_STATUSES
            )
        )

    ggplotly(p)
}
