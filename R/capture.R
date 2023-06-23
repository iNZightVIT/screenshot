#' Capture Screenshot
#'
#' @param file The name of the file
#' @param grid optional, if TRUE then a grid is overlaid to help with
#'             positioning
#' @return NULL, generates a file called 'file.png'
#' @export
capture <- function(file, grid = getOption("screenshot.dev.grid", FALSE)) {
    dir <- dirname(file)

    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    cli::cli_h3("{basename(name)}")

    cli::cli_progress_step("Capturing")
    Sys.sleep(0.5)
    s <- system(
        sprintf("%s savescreenshotwin %s", 
            system.file("nircmd.exe", package = "screenshot"),
            name
        )
    )
    rm(s)
    Sys.sleep(0.01)

    img <- magick::image_read(name)
    img_info <- magick::image_info(img)
    crop(file,
        x0 = 7,
        x1 = img_info$width[1] - 8,
        y0 = 0,
        y1 = img_info$height[1] - 8
    )
    cli::cli_progress_done()

    if (grid) add_grid(file)

    options("screenshot.last.file" = file)

    invisible(NULL)
}
