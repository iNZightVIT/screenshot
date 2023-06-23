# crawl a directory looking for 'screenshot.R' files

crawl <- function(dir) {
    cli::cli_h1("Crawling for screenshot files")
    cli::cli_inform("Directory: {dir}")
    screenshot_files <- list.files(dir, pattern = "screenshot\\.R",
        recursive = TRUE, full.names = TRUE)

    cli::cli_inform("Found {length(screenshot_files)} files")

    sapply(screenshot_files, function(file) {
        cli::cli_h2("Entering {dirname(file)}")
        o <- setwd(dirname(file))
        on.exit(setwd(o))
        source(basename(file))
    })

    cli::cli_h1("Complete")
    invisible()
}
