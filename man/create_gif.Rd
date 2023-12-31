% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gif.R
\name{create_gif}
\alias{create_gif}
\title{Create a GIF}
\usage{
create_gif(files, cleanup = FALSE, ...)
}
\arguments{
\item{files}{list of files to convert into a GIF}

\item{cleanup}{if TRUE files will be deleted}

\item{...}{additional arguments passed to \code{magick::image_animate()}}
}
\value{
NULL, creates a GIF
}
\description{
Create a GIF
}
