% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak_finding.R
\name{iso_find_peaks}
\alias{iso_find_peaks}
\alias{iso_find_peaks.iso_file_list}
\alias{iso_find_peaks.data.frame}
\title{Find chromatographic peaks}
\usage{
iso_find_peaks(...)

\method{iso_find_peaks}{iso_file_list}(iso_files, ..., quiet = default(quiet))

\method{iso_find_peaks}{data.frame}(df, quiet = default(quiet))
}
\arguments{
\item{...}{S3 method placeholder parameters, see class specific functions for details on parameters}

\item{iso_files}{collection of continuous flow iso_file objects}

\item{quiet}{whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages.}

\item{df}{a data frame of raw continuouus flow data, must have at minimum the columns 'file_id'}
}
\description{
NOT YET IMPLEMENTED.
}
