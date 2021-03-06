% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{iso_calculate_ratios}
\alias{iso_calculate_ratios}
\alias{iso_calculate_ratios.iso_file_list}
\alias{iso_calculate_ratios.data.frame}
\title{Calculate ratios}
\usage{
iso_calculate_ratios(...)

\method{iso_calculate_ratios}{iso_file_list}(iso_files, ratios, quiet = default(quiet))

\method{iso_calculate_ratios}{data.frame}(df, ratios, quiet = default(quiet))
}
\arguments{
\item{...}{S3 method placeholder parameters, see class specific functions for details on parameters}

\item{iso_files}{collection of iso_file objects}

\item{ratios}{which ratios to calculate (e.g. c("45/44", "46/44"))}

\item{quiet}{whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages.}

\item{df}{a data frame of raw continuous flow or dual inlet data, must have at minimum the columns 'file_id' and the referenced mass columns required to calculate the \code{ratios}. Assumes that mass columns have either 'v' (for voltages) or 'i' (for currents) as the column name prefix and a ".<unit>" suffix, but otherwise makes no safety checks and will let you calculate any two ratios.}
}
\value{
the passed in iso_file(s) with ratios added
}
\description{
Calculate ratios from raw data. Note that these are raw ratios and are not normalized against any standards yet. The ratios are calculated straight from the raw data without any unit conversions, i.e. if the raw data is in mV, the ratio is mV/mV, if in nA the ratio is nA/nA, etc. These raw ratios are subsequently NOT identical to absolute ratios, in fact they are usually not even close (especially if raw data recorded as voltages with different resistors). If raw data is first converted to identical current units (\code{\link{iso_convert_signals}}), the ratios may be close to their true values (+instrument fractionation), however, isotope ratios should always be calibrated against reference ratios measured in the same data file.
}
