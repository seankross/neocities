neocities_upload <- function(path, ..., user = neocities_user(), pass = neocities_pass(), filename=NULL){
  auth <- neocities_auth(user, pass)
  if(is.null(filename)){
    filename <- basename(path)
  }
  upload_body <- list()
  upload_body[[filename]] <- upload_file(path)
  req <- POST("https://neocities.org/api/upload", 
              auth, 
              body=upload_body, ...)
  req
}

neocities_delete <- function(filename, ..., user = neocities_user(), pass = neocities_pass()){
  auth <- neocities_auth(user, pass)
  delete_body <- list()
  delete_body[["filenames[]"]] <- filename
  
  req <- POST("https://neocities.org/api/delete",
              auth,
              body=delete_body, ...)
  req
}

neocities_info <- function(user = neocities_user(), ...){
  GET("https://neocities.org/api/info", query=paste0("sitename=", user), ...)
}

neocities_auth <- function(user=neocities_user(), password=neocities_pass()){
  authenticate(user, password, type = "basic")
}

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