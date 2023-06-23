#' Add rectangle
#'
#' @param file The name of a file (without extension) to add to
#' @param x0 left side of the rectangle
#' @param y0 bottom of the rectangle
#' @param x1 right side of the rectangle
#' @param y1 top of the rectangle
#' @param border_col colour of the border
#' @param border_width width of the border
#' @param fill_col colour to will the rectangle
#' @return NULL, saves image
#' @export
add_rect <- function(file, x0, y0, x1, y1,
                     border_col = "red", border_width = 2,
                     fill_col = NA) {
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

    cli::cli_progress_step("Add rectangle")

    img <- magick::image_read(name)

    img_e <- magick::image_draw(img)
    rect(x0, y0, x1, y1,
        border = border_col, lwd = border_width,
        col = fill_col
    )
    dev.off()
    magick::image_write(img_e, name)
}

#' Add line
#' @param file The name of a file (without extension) to add to
#' @param x0 starting x coordinate
#' @param y0 starting y coordinate
#' @param x1 end x coordinate
#' @param x2 end y coordinate
#' @param line_col colour of the line
#' @param line_width width of the line
#' @return NULL, saves image
#' @export
add_line <- function(file, x0, y0, x1 = x0, y1 = y0,
                     line_col = "red",
                     line_width = 2L
                     ) {
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

    cli::cli_progress_step("Add line")

    img <- magick::image_read(name)

    img_e <- magick::image_draw(img)
    lines(x0, y0, x1, y1,
        lwd = line_width,
        col = line_col
    )
    dev.off()
    magick::image_write(img_e, name)
}

#' Add arrow
#' @param file The name of a file (without extension) to add to
#' @param x0 starting x coordinate
#' @param y0 starting y coordinate
#' @param x1 end x coordinate
#' @param x2 end y coordinate
#' @param arrow_col colour of the line
#' @param arrow_width width of the line
#' @param head_angle angle of the arrow head
#' @param head_length length of the arrow head
#' @return NULL, saves image
#' @export
add_arrow <- function(file, x0, y0, x1 = x0, y1 = y0,
                      arrow_col = "red",
                      arrow_width = 2L,
                      head_angle = 30,
                      head_length = 0.2
                      ) {
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

    cli::cli_progress_step("Add arrow")

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

#' Add circle
#' @param file The name of a file (without extension) to add to
#' @param x0 center x coordinate
#' @param y0 center y coordinate
#' @param radius size of the circle
#' @param col colour of the border
#' @param width width of the border
#' @param fill_col fill colour
#' @return NULL, saves image
#' @export
add_circle <- function(file, x0, y0, radius,
                       col = "red",
                       width = 2L,
                       fill_col = NA) {
    if (missing(file))
        file <- getOption("screenshot.last.file")

    if (is.null(file))
        stop("No file specified")

    dir <- dirname(dir)
    name <- sprintf(
        "%s.png",
        file.path(dir, file)
    )
    if (!file.exists(name)) stop("File does not exist")

    cli::cli_progress_step("Add circle")

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

#' Add text
#' @param file The name of a file (without extension) to add to
#' @param x0 x position
#' @param y0 y position
#' @param text the text to add
#' @param font_col the colour of the text
#' @param font_size the size of the text
#' @param font_style the style of the text (see ?text)
#' @param justify_x the x justification of the text (see ?text)
#' @param justify_y the y justification of the text (see ?text)
#' @return NULL, saves image
#' @export
add_text <- function(file, x0, y0, text, 
                     font_col = "red",
                     font_size = 1,
                     font_style = 1L,
                     justify_x = 0.5,
                     justify_y = NA) {
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

    cli::cli_progress_step("Add text")

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
