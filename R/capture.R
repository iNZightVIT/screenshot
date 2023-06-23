capture <- function(file) {
    dir <- dirname(file)

    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )

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
    invisible(NULL)
}
