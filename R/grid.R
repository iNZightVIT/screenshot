# add a grid to help with development
add_grid <- function(file) {
    if (missing(file))
        file <- getOption("screenshot.last.file")

    if (is.null(file))
        stop("No file specified")

    dir <- dirname(file)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    grid_size <- getOption("screenshot.grid.size", c(50, 500))

    cli::cli_progress_step("Add grid")

    img <- magick::image_read(name)
    img_info <- magick::image_info(img)

    img_e <- magick::image_draw(img)
    ab_y <- seq(0, img_info$height[1], by = grid_size[1])
    abline(h = ab_y, col = "white",
        lwd = ifelse(ab_y %% grid_size[2] == 0, 5, 1))
    ab_x <- seq(0, img_info$width[1], by = grid_size[1])
    abline(v = ab_x, col = "white",
        lwd = ifelse(ab_x %% grid_size[2] == 0, 5, 1))
    dev.off()
    magick::image_write(img_e, name)
}
