% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{iso_prepare_dual_inlet_plot_data}
\alias{iso_prepare_dual_inlet_plot_data}
\title{Prepare plotting data from dual inlet files}
\usage{
iso_prepare_dual_inlet_plot_data(
  iso_files,
  data = character(),
  include_file_info = NULL,
  filter = NULL
)
}
\arguments{
\item{iso_files}{collection of iso_file objects}

\item{data}{which masses, ratios and deltas to plot (e.g. \code{c("44", "45", "45/44", "d45/44")} - without the units), if omitted, all available masses, ratios, and delta values are plotted. Note that ratios should be calculated using \code{\link{iso_calculate_ratios}} and delta values should be calculated using \code{\link{iso_calculate_deltas}} prior to plotting.}

\item{include_file_info}{which file information to include (see \link[isoreader]{iso_get_file_info}). Use c(...) to select multiple, supports all \link[dplyr]{select} syntax including renaming columns.}

\item{filter}{any filter condition to apply to the data beyond the masses/ratio/delta selection (param \code{data}). For details on the available data columns see \link[isoreader]{iso_get_raw_data} with parameters \code{gather = TRUE} and \code{include_file_info = everything()}.}
}
\description{
This function helps with the preparation of plotting data from dual inlet files. Call either explicity and pass the result to \code{\link{iso_plot_dual_inlet_data}} or let \code{\link{iso_plot_dual_inlet_data}} take care of preparing the plotting data directly from the \code{iso_files}.
}
\seealso{
Other plot functions: 
\code{\link{iso_mark_calibration_range}()},
\code{\link{iso_mark_outliers}()},
\code{\link{iso_mark_value_range}()},
\code{\link{iso_mark_x_range}()},
\code{\link{iso_plot_continuous_flow_data}()},
\code{\link{iso_plot_data}()},
\code{\link{iso_plot_dual_inlet_data}()},
\code{\link{iso_plot_raw_data}()},
\code{\link{iso_plot_ref_peaks}()},
\code{\link{iso_plot_residuals}()},
\code{\link{iso_plot_scan_data}()},
\code{\link{iso_prepare_continuous_flow_plot_data}()},
\code{\link{iso_prepare_scan_plot_data}()}
}
\concept{plot functions}
