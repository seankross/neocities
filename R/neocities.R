#' Upload a file to your Neocities site. 
#'
#' @param path The path to the file you want to upload.
#' @param ... Arguments that will be passed to the underlying httr POST request.
#' @param user The username for your neocities account. See ?neocities_user for more details.
#' @param pass The password for your neocities account. See ?neocities_pass for more details.
#' @param filename The name of the file as it will appear on neocities.org. If \code{NULL}, the base name of the file will be used. The default value is \code{NULL}.
#' @export
#' @importFrom httr POST upload_file
#' @examples
#' \dontrun{
#' 
#' ## Username and password have not been set as environmental variables, so
#' ## the user will be prompted to enter their username and password if in
#' ## interactive mode. This file will appear on neocities.org at 
#' ## http://username.neocities.org/report.html
#' 
#' neocities_upload("~/Desktop/report.html")
#' 
#' ## Setting username and password as environmental variables is wise when not
#' ## in interactive mode, but be sure not to distribute them with your code.
#' 
#' Sys.setenv(NEOCITIES_USERNAME = "username")
#' Sys.setenv(NEOCITIES_PASSWORD = "password")
#' neocities_upload("~/Desktop/report.html")
#' 
#' ## You can also pass your username and password as function arguments.
#' 
#' neocities_upload("~/Desktop/report.html", user="username", pass="password")
#' 
#' ## You can change what the filename will be on your neocities domain. This
#' ## file will appear at http://username.neocities.org/october.html
#' 
#' neocities_upload("~/Desktop/report.html", filename="october.html")
#' 
#' }
neocities_upload <- function(path, ..., user = neocities_user(), pass = neocities_pass(), filename=NULL){
  # Get authorization
  auth <- neocities_auth(user, pass)
  
  # Get file name as it will appear on neocities
  if(is.null(filename)){
    filename <- basename(path)
  }
  
  # Create and post the body of the http request
  upload_body <- list()
  upload_body[[filename]] <- upload_file(path)
  req <- POST("https://neocities.org/api/upload", 
              auth, 
              body=upload_body, ...)
  req
}

#' Delete a file on your Neocities site.
#' 
#' @param filename The name of the file as it appears on neocities.org.
#' @param ... Arguments that will be passed to the underlying httr POST request.
#' @param user The username for your neocities account. See ?neocities_user for more details.
#' @param pass The password for your neocities account. See ?neocities_pass for more details.
#' @export
#' @importFrom httr POST
#' @examples
#' \dontrun{
#' 
#' ## Username and password have not been set as environmental variables, so
#' ## the user will be prompted to enter their username and password if in
#' ## interactive mode. The file will be deleted from
#' ## http://username.neocities.org/report.html
#' 
#' neocities_delete("report.html")
#' 
#' ## Setting username and password as environmental variables is wise when not
#' ## in interactive mode, but be sure not to distribute them with your code.
#' 
#' Sys.setenv(NEOCITIES_USERNAME = "username")
#' Sys.setenv(NEOCITIES_PASSWORD = "password")
#' neocities_delete("report.html")
#' 
#' ## You can also pass your username and password as function arguments.
#' 
#' neocities_delete("report.html", user="username", pass="password")
#'
#' }
neocities_delete <- function(filename, ..., user = neocities_user(), pass = neocities_pass()){
  # Get authorization
  auth <- neocities_auth(user, pass)
  
  # Create and post the body of the http request
  delete_body <- list()
  delete_body[["filenames[]"]] <- filename
  
  req <- POST("https://neocities.org/api/delete",
              auth,
              body=delete_body, ...)
  req
}

#' Get information on a Neocities site.
#' 
#' @param user The username for your neocities account. See ?neocities_user for more details.
#' @param ... Arguments that will be passed to the underlying httr GET request.
#' 
#' @export
#' @importFrom httr GET
#' @examples
#' \dontrun{
#' 
#' ## Username has not been set as an environmental variable, so
#' ## the user will be prompted to enter their username if in
#' ## interactive mode. 
#' 
#' neocities_info()
#' 
#' ## Setting the username as an environmental variable is wise when not
#' ## in interactive mode.
#' 
#' Sys.setenv(NEOCITIES_USERNAME = "username")
#' neocities_info()
#' 
#' ## You can also pass your username as a function argument.
#' 
#' neocities_info(user="username")
#'
#' }
neocities_info <- function(user = neocities_user(), ...){
  GET("https://neocities.org/api/info", query=paste0("sitename=", user), ...)
}

#' @importFrom httr authenticate
neocities_auth <- function(user=neocities_user(), password=neocities_pass()){
  authenticate(user, password, type = "basic")
}

#' Interactively set your neocities username. 
#'
#' @param force If set to \code{TRUE}, this function will attempt to return the neocities username variable, even if it does not exist. The default value is \code{FALSE}.
#' 
#' @export
neocities_user <- function(force=FALSE){
  env <- Sys.getenv("NEOCITIES_USERNAME")
  if (!identical(env, "") && !force) 
    return(env)
  
  if (!interactive()) {
    stop("Please set env var NEOCITIES_USERNAME to your Neocities username.", 
         call. = FALSE)
  }
  
  message("Couldn't find env var NEOCITIES_USERNAME. See ?neocities_user for more details.")
  message("Please enter your Neocities username and press enter:")
  uname <- readline(": ")
  
  if (identical(uname, "")) {
    stop("Neocities username entry failed", call. = FALSE)
  }
  
  message("Updating NEOCITIES_USERNAME env var to your username")
  Sys.setenv(NEOCITIES_USERNAME = uname)
  
  uname
}

#' Interactively set your neocities password. 
#'
#' @param force If set to \code{TRUE}, this function will attempt to return the neocities password variable, even if it does not exist. The default value is \code{FALSE}.
#' 
#' @export
neocities_pass <- function(force=FALSE){
  env <- Sys.getenv("NEOCITIES_PASSWORD")
  if (!identical(env, "") && !force) 
    return(env)
  
  if (!interactive()) {
    stop("Please set env var NEOCITIES_PASSWORD to your Neocities password.", 
         call. = FALSE)
  }
  
  message("Couldn't find env var NEOCITIES_PASSWORD. See ?neocities_pass for more details.")
  message("Please enter your Neocities password and press enter:")
  pass <- readline(": ")
  
  if (identical(pass, "")) {
    stop("Neocities password entry failed", call. = FALSE)
  }
  
  message("Updating NEOCITIES_PASSWORD env var to your username")
  Sys.setenv(NEOCITIES_PASSWORD = pass)
  
  pass
}