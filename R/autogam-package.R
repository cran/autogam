#' Automate the Creation of Generalized Additive Models (GAMs)
#'
#'This wrapper package for 'mgcv' makes it easier to create high-performing Generalized Additive Models (GAMs). With its central function autogam(), by entering just a dataset and the name of the outcome column as inputs, 'AutoGAM' tries to automate the procedure of configuring a highly accurate GAM which performs at reasonably high speed, even for large datasets.
#'
#' @author Chitu Okoli \email{Chitu.Okoli@skema.edu}
#' @docType package
#'
#' @keywords internal
#' @aliases autogam-package NULL
#'
#'
#' @import dplyr
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_abort
#' @importFrom cli cli_inform
#' @importFrom cli cli_warn
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom rlang .data
#'
'_PACKAGE'




