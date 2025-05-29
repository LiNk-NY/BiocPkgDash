# Bioconductor Package Dashboard Shiny App
webr::install(
    c(
        "glue", "shiny", "shinydashboard", "DT", "bslib", "bsicons",
        "BiocManager"
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

.pkgbiocdeps <- function(pkg) {
    all_db <- utils::available.packages(repos = BiocManager::repositories())
    repo <- BiocManager:::.repositories_bioc(version)["BioCsoft"]
    biocdb <- utils::available.packages(repos = repo)
    res <- tools::package_dependencies(
        pkg, db = all_db, which = "all", recursive = recursive
    )
    if (only.bioc)
        lapply(res, function(pkglist) pkglist[pkglist %in% rownames(biocdb)])
    else
        res
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

# Define UI
ui <- page_navbar(
    title = div(
        img(src = "https://bioconductor.org/images/logo_bioconductor.gif", height = "30px"),
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
            BiocPkgDash:::badgesDF(
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
                deps <- .pkgbiocdeps(pkg)
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
