#' @name Toxboot configuration functions
#' @rdname toxboot_config_funcs
#' @title Functions for configuring the toxboot package
#'
#' @description This set of functions are used to configure the settings used to
#'   interact with the mongoDB database.
#'
#' @param mongo_host Character of length 1, database IP address
#' @param DBNS Character of length 1, the collection on the database to read
#'   from and write to
#' @param user Character of length 1, username to authenticate
#' @param pass Character of length 1, password that corresponds to username
#' @param db Character of length 1, database where the username and pass are
#'   authenicated. Can be an IP address or a url.
#' @param show.pass Logical, should the password be returned
#'
#' @details Parameter settings are stored in two places. Long term storage on
#'   disk is in a configuration file, toxboot.config, located in the toxboot
#'   package directory within the library. This allows default settings to be
#'   maintained between sessions without a need for including IP addresses and
#'   login credentials within user scripts. During runtime, parameter settings
#'   are set in \code{options} to be used by various \code{toxboot} functions.
#'   The \code{toxboot} configuration functions are used to read and/or write
#'   parameters from/to the toxboot.config configuration files at the settings
#'   in \code{options}.
#'
#'   \code{toxbootConf} changes \code{options} to set the toxboot options used
#'   to interact with mongoDB when reading or writing results. This includes the
#'   location of the database, which collection to use, and a username and
#'   password to authenticate against the database. \code{toxbootConf} will only
#'   change non-null values, and can be used to change a single value if needed.
#'
#'   \code{toxbootConfSave} modifies the toxboot.config file. Current global
#'   parameters are ready from \code{options} using \code{toxbootConfList} and
#'   the values are stored in toxboot.config within the toxboot directory within
#'   the library.
#'
#'   \code{toxbootConfList} returns the toxboot global parameters from
#'   \code{options}.
#'
#'   \code{toxbootConfLoad} reads the toxboot parameters from toxboot.config and
#'   updates \code{options} with these settings.
#'
#'   \code{toxbootConfReset} will overwrite toxboot.config setting all
#'   parameters to NA. This is used to remove any user information from the
#'   file.
NULL
