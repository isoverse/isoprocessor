% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{unnest_select_data}
\alias{unnest_select_data}
\title{unnest parts of a data frame without loosing the rest}
\usage{
unnest_select_data(
  dt,
  select = everything(),
  nested_data = nested_data,
  keep_remaining_nested_data = TRUE,
  keep_other_list_data = TRUE,
  keep_only_unique = TRUE
)
}
\arguments{
\item{select}{which columns to unnest - use \code{c(...)} to select multiple, supports all \link[dplyr]{select} syntax including renaming columns. Includes all columns by default (i.e. unnests an entire nested data frame).}

\item{nested_data}{which column to unnest the \code{select} from}

\item{keep_remaining_nested_data}{whether to keep any remaining parts of the partially unnested data (irrelevant if \code{select = everything()})}

\item{keep_other_list_data}{keep other list data columns (e.g. other data or model columns)}
}
\description{
note that this will lead to row duplication if the unnested variables have multiple entries per row of the \code{dt} data frame
also note that this will remove rows that have NULL in the nested_data column
}
