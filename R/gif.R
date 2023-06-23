#' Create a GIF
#' @param files list of files to convert into a GIF
#' @param cleanup if TRUE files will be deleted
#' @param ... additional arguments passed to `magick::image_animate()`
#' @return NULL, creates a GIF
#' @export
create_gif <- function(files, cleanup = FALSE, ...) {
    img_list <- lapply(files, magick::image_read)
    img_joined <- magick::image_join(img_list)
    img_animated <- magick::image_animate(img_joined, ...)
    magick::image_write(
        image = img_animated,
        path = sprintf("%s.gif", dir)
    )
    if (cleanup) unlink(files)
}
