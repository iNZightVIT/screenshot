crop <- function(file,
                 x0 = 0, x1 = img_info$width[1],
                 y0 = 0, y1 = img_info$height[1]
                 ) {
    if (missing(file))
        file <- getOption("screenshot.last.file")

    if (is.null(file))
        stop("No file specified")

    cli::cli_progress_step("Cropping")
    dir <- dirname(file)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    img <- magick::image_read(name)
    img_info <- magick::image_info(img)

    img_cropped <- magick::image_crop(
        img,
        magick::geometry_area(x1 - x0, y1 - y0, x0, y0)
    )

    magick::image_write(img_cropped, name)
    invisible(NULL)
}
