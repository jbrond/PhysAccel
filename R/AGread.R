#' Read Data Files from ActiGraph Monitors
#'
#' This provides support for reading ActiGraph files of various modes into R.
#' For more information see:
#' \url{https://actigraph.desk.com/customer/en/portal/articles/2515800-what-do-the-different-mode-numbers-mean-in-a-csv-or-dat-file-}.
#' Functions are provided to read and minimally pre-process raw data from
#' primary accelerometer and inertial measurement unit files. Reading binary
#' .gt3x files is now supported as well. See
#' \url{https://github.com/actigraph/GT3X-File-Format} for more information.
#'
#' @section Core functions:
#' \code{\link{read_AG_counts}}
#'
#'
#' @importFrom magrittr %>% %T>% %<>% %$%
#' @importFrom rlang := .data
NULL
