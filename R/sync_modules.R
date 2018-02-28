# check the modules are up to date and fetch them if not
#' @noRd
#' @importFrom git2r remote_ls
latest_sha <- function() {
  shas <- git2r::remote_ls(paste0("https://github.com/",
                                  getOption("zoonModulesRepo"),
                                  ".git"))
  idx <- grep(getOption("zoonModulesBranch"), names(shas))
  shas[[idx]]
}

#' @noRd
#' @importFrom utils packageDescription
current_sha <- function () {
  suppressWarnings(desc <- utils::packageDescription("zoon.modules"))
  sha <- ""
  if (inherits(desc, "packageDescription")) {
    sha <- desc$RemoteSha
    if (is.null(sha))
      sha <- ""
  } 
  sha
}

sync_modules <- function () {
  
  # see if the modules are out of date
  if (latest_sha() != current_sha()) {
    message("\nyour zoon modules are out of date")
    
    # give the user the option to update
    if (interactive()) {
      
      message("would you like to update them?")
      installChoice <- utils::menu(c("yes", "no"))
      
      if (installChoice == 1) {
        remotes::install_github(getOption("zoonModulesRepo"), ref = getOption("zoonModulesBranch"),
                                dependencies = FALSE, quiet = TRUE)
        message("zoon modules updated")
      }
    }
  }
  
  # try to load them
  suppressWarnings(require("zoon.modules", quietly = TRUE))
}
