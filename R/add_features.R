
add_rect <- function(file, x0, y0, x1, y1,
                     border_col = "red", border_width = 2,
                     fill_col = NA) {
    if (missing(file)) stop("You need to specify a path")
    dir <- dirname(file)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    img <- magick::image_read(name)

    img_e <- magick::image_draw(img)
    rect(x0, y0, x1, y1,
        border = border_col, lwd = border_width,
        col = fill_col
    )
    dev.off()
    magick::image_write(img_e, name)
}

add_arrow <- function(file, x0, y0, x1 = x0, y1 = y0,
                      arrow_col = "red",
                      arrow_width = 2L,
                      head_angle = 30,
                      head_length = 0.2
                      ) {
    if (missing(file)) stop("You need to specify a path")
    dir <- dirname(file)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    img <- magick::image_read(name)

    img_e <- magick::image_draw(img)
    arrows(x0, y0, x1, y1,
        length = head_length,
        angle = head_angle,
        lwd = arrow_width,
        col = arrow_col
    )
    dev.off()
    magick::image_write(img_e, name)
}

add_circle <- function(file, x0, y0, radius,
                       col = "red",
                       width = 2L,
                       fill_col = NA) {
    if (missing(file)) stop("You need to specify a path")
    dir <- dirname(dir)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    img <- magick::image_read(name)

    img_e <- magick::image_draw(img)
    points(x0, y0, 
        pch = 21,
        lwd = width,
        col = col,
        cex = radius,
        bg = fill_col
    )
    dev.off()
    magick::image_write(img_e, name)
}

add_text <- function(file, x0, y0, text, 
                     font_col = "red",
                     font_size = 1,
                     font_style = 1L,
                     justify_x = 0.5,
                     justify_y = NA) {
    if (missing(file)) stop("You need to specify a path")
    dir <- dirname(file)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    img <- magick::image_read(name)

    img_e <- magick::image_draw(img)
    text(x0, y0, labels = text,
        col = font_col,
        cex = font_size,
        font = font_style,
        adj = c(justify_x, justify_y)
    )
    dev.off()
    magick::image_write(img_e, name)
}