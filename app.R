# Bioconductor Package Dashboard Shiny App
webr::install(
    c(
        "glue", "shiny", "shinydashboard", "DT", "bslib", "bsicons",
        "BiocManager", "whisker"
    ),
    repos = c(
        "https://repo.r-wasm.org/",
        "https://webr.bioconductor.org/3.21",
        "https://link-ny.r-universe.dev/"
    )
)

# Load required libraries
library(glue)
library(shiny)
library(shinydashboard)
library(DT)
library(bslib)
library(bsicons)
library(BiocManager)

.stats_table <- function(pkg) {
    url <- glue::glue(
        "https://bioconductor.org/packages/stats/bioc/{pkg}/{pkg}_2025_stats.tab"
    )
    res <- read.table(url, header = TRUE)
    subset(
        res,
        res$Month != "all" &
            (res$Nb_of_distinct_IPs !=  0 & res$Nb_of_downloads != 0)
    )
}

.pkgbiocdeps <- function(pkg, version) {
    all_db <- utils::available.packages(repos = BiocManager::repositories())
    repo <- BiocManager:::.repositories_bioc(version)["BioCsoft"]
    biocdb <- utils::available.packages(repos = repo)
    res <- tools::package_dependencies(
        pkg, db = all_db, which = "all", recursive = FALSE
    )
    lapply(res, function(pkglist) pkglist[pkglist %in% rownames(biocdb)])
}

.read_views <- function(version) {
    bioc_repos <- BiocManager:::.repositories_bioc(version = version)
    views_url <- paste0(bioc_repos["BioCsoft"], "/VIEWS")
    con <- url(views_url)
    on.exit(close(con))
    res <- suppressWarnings({
        try(read.dcf(con), silent = TRUE)
    })
    if (inherits(res, "try-error"))
        stop("Unable to read VIEWS file URL: ", url)
    else
        tibble::as_tibble(res, stringsAsFactors = FALSE)
}

.renderMaintained <- function(email, version) {
    views <- .read_views(version)
    if (is.null(views))
        stop("No views found for the specified Bioconductor version.")
    views <- views[grep(email, fixed = TRUE, x = views$Maintainer), ]
    if (!nrow(views))
        stop("No packages found for the specified maintainer email.")
    views
}

.get_pkgType_from_URL <- function (packages, version)
{
    repos <- BiocManager:::.repositories_bioc(version)
    pkgsdb <- available.packages(repos = repos)
    repo_urls <- pkgsdb[rownames(pkgsdb) %in% packages, "Repository"]
    tail_urls <- vapply(strsplit(repo_urls, paste0(version, "/")),
                        "[", character(1L), 2L)
    biocType <- gsub("/src/contrib", "", tail_urls)
    pkgsnot <- !packages %in% names(biocType)
    npkgs <- paste(packages[pkgsnot], collapse = ", ")
    if (any(pkgsnot))
        warning("Bioconductor package category not found for: ", npkgs)
    gsub("/", "-", biocType, fixed = TRUE)
}

.SHIELDS_URL <- "https://bioconductor.org/shields/build/"
.CHECK_RESULTS_URL <- "http://bioconductor.org/checkResults/"

.build_urls_temp <- function(packages, pkgType, templates)
{
    .data <- data.frame(
        package = packages, pkgType = pkgType
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

.build_html_link <- function(.data, shieldCol, resultCol, version) {
    paste0(
        '<a href=', dQuote(.data[[resultCol]], q = FALSE), ' target="_blank">',
        '<img src=', dQuote(.data[[shieldCol]], q = FALSE),
        ' alt="Bioconductor-', version, ' Build Status"></a>'
    )
}

.badgesDF <- function(email, data = NULL)
{
    version <- BiocManager:::.version_bioc(type = "devel")
    if (is.null(data))
        maindf <- .renderMaintained(email = email, version = version)
    else
        maindf <- data
    pkgType <- .get_pkgType_from_URL(maindf[["Package"]], version)
    version <- c("release", "devel")
    templates <- c(
        paste0(.SHIELDS_URL, version, "/{{pkgType}}/{{package}}.svg"),
        paste0(.CHECK_RESULTS_URL, version, "/{{pkgType}}-LATEST/{{package}}")
    )
    names(templates) <- c("rshield", "dshield", "rresult", "dresult")
    maindf <- maindf[match(names(pkgType), maindf[["Package"]]),
    ]
    urldf <- .build_urls_temp(
        packages = maindf[["Package"]],
        pkgType = pkgType,
        templates = templates
    )
    rellink <- .build_html_link(urldf, "rshield", "rresult", "release")
    devlink <- .build_html_link(urldf, "dshield", "dresult", "devel")
    data.frame(
        Package = maindf[["Package"]],
        `Bioc-release` = rellink,
        `Bioc-devel` = devlink,
        row.names = NULL,
        check.names = FALSE
    )
}

# Define UI
ui <- page_navbar(
    title = div(
        img(
            src = "https://bioconductor.org/images/logo_bioconductor.gif",
            height = "30px"
        ),
        "Bioconductor Package Dashboard"
    ),
    theme = bs_theme(bootswatch = "minty"),

    # Sidebar panel
    sidebar = sidebar(
        title = "Version",
        width = 300,

        radioButtons(
            inputId = "biocver",
            label = "Bioconductor version:",
            choices = c("release", "devel"),
            selected = "release"
        ),

        textInput(
            inputId = "email",
            label = "Enter maintainer e-mail",
            placeholder = "maintainer@bioconductor.org"
        ),

        actionButton(
            inputId = "submit",
            label = "Submit",
            class = "btn-primary"
        )
    ),

    # Main content tabs
    nav_panel(
        title = "Badges",
        layout_columns(
            col_widths = c(8, 4),

            # Badges table
            card(
                card_header("Wall of Badges"),
                DT::dataTableOutput("badge_out")
            ),

            # Value boxes
            layout_columns(
                col_widths = 12,

                value_box(
                    title = "Total Packages",
                    value = textOutput("pkg_count"),
                    showcase = bs_icon("hash"),
                    theme = "primary"
                ),

                value_box(
                    title = "Year-To-Date Downloads",
                    value = textOutput("dl_count"),
                    showcase = bs_icon("cloud-download"),
                    theme = "secondary"
                ),

                value_box(
                    title = "Total No. of Dependencies",
                    value = textOutput("dep_count"),
                    showcase = bs_icon("collection"),
                    theme = "warning"
                )
            )
        )
    )
)

# Define Server
server <- function(input, output, session) {

    # Initialize data
    version <- BiocManager:::.version_bioc(type = "devel")
    initial_pkgs <- .renderMaintained(
        email = "maintainer@bioconductor.org",
        version = version
    )

    # Reactive values
    emailValue <- reactiveVal("maintainer@bioconductor.org")

    # Main data reactive
    main_data <- reactive({
        if (
            emailValue() == "maintainer@bioconductor.org"
        ) {
            initial_pkgs
        } else {
            .renderMaintained(
                email = emailValue(),
                version = input$biocver
            )
        }
    })

    # Update email when submit is clicked
    observeEvent(input$submit, {
        emailValue(input[["email"]])
    })

    # Package count output
    output$pkg_count <- renderText({
        nrow(main_data())
    })

    # Badges table output
    output$badge_out <- DT::renderDataTable({
        DT::datatable(
            .badgesDF(
                data = main_data()
            ),
            escape = FALSE,
            rownames = FALSE,
            options = list(
                dom = "ftp",
                pageLength = 20,
                lengthChange = FALSE,
                paging = TRUE
            )
        )
    })

    # Download count output
    output$dl_count <- renderText({
        all_dls <- vapply(
            main_data()$Package,
            function(pkg) {
                dl_pkg <- suppressWarnings({
                    .stats_table(pkg)
                })
                dls <- sum(dl_pkg[["Nb_of_distinct_IPs"]])
                if (!length(dls))
                    0
                else
                    dls
            },
            numeric(1L)
        )
        prettyNum(
            sum(all_dls),
            big.mark = ",",
            scientific = FALSE
        )
    })

    # Dependency count output
    output$dep_count <- renderText({
        total_deps <- vapply(
            main_data()$Package,
            function(pkg) {
                deps <- .pkgbiocdeps(pkg, version = version)
                length(unlist(deps))
            },
            numeric(1L)
        )
        prettyNum(
            sum(total_deps),
            big.mark = ",",
            scientific = FALSE
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
