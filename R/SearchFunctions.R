#'GetModuleList
#'
#'Get a list of all the modules available on the github repo.
#'
#'@return A list with all module names.
#'@name GetModuleList
#'@param renew Download from github even if we already have a module list.
#'
#'@export
#'@examples \dontrun{GetModuleList()}

GetModuleList <- function(renew = FALSE){

  # Check if we've made an environment called zoonHidden yet
  # If not make one.
  if(!exists('zoonHidden', mode = 'environment')){
    .zoonHidden <- new.env(parent = .GlobalEnv)
  }
  
  # If we've already downloaded a module list, print that.
  # Otherwise download a list from github
  # If renew is TRUE download from github even if we have a list already.
  if(exists('moduleList', envir = .zoonHidden) & !renew){
    moduleNames <- .zoonHidden$moduleList
    return(moduleNames)
  } else {

    # Get list of files in github repo
    files <- gh_list_files('zoonproject', 'modules')

    # Find files in directory 'R/'. Then remove unneeded characters.
    mods <- files[grep('^R/', files)]
    names <- gsub('^R/|.R$', '', mods)

    # Remove ModulesDocumentation which is not a module.
    moduleNames <- names[-grep('ModulesDocumentation', names)]
    .zoonHidden$moduleList <- moduleNames
    return(moduleNames)
  }
}

# Code largely taken from 

github_auth <- function(appname = getOption("gh_appname"), key = getOption("gh_id"), 
                        secret = getOption("gh_secret")) {
  if (is.null(getOption("gh_token"))) {
    myapp <- oauth_app(appname, key, secret)
    token <- oauth2.0_token(oauth_endpoints("github"), myapp)
    options(gh_token = token)
  } else {
    token <- getOption("gh_token")
  }
  return(token)
}

make_url <- function(owner, repo, sha) {
  sprintf("https://api.github.com/repos/%s/%s/git/trees/%s?recursive=1", owner, repo, sha)
}

#probably should keep the secret in here, but can't work out how to fix it right now.
gh_list_files <- function(owner, repo, ...) {
  token <- github_auth(appname = "zoonproject", key = "945195231bd7edd336c9", secret = 'd43d121439b081db59e5f0852e25712294128e2f')

  # if this is a dev version, find the most recent version of modules
  if (.sha == 'master') {

    sha <- FindNewestCommit(owner, rep, token)

  } else {

    # otherwise use the target sha for modules
    sha <- .sha
  
  }    
  
  req <- GET(make_url(owner, repo, sha), config = list(token = token))
  out <- content(req)
  sapply(out$tree, "[[", "path")
}

FindNewestCommit <- function(owner, repo, token){
  r <- GET('https://api.github.com/repos/zoonproject/modules/commits', config = list(token = token))
  outSha <- content(r)

  commits <- data.frame(t(unlist(sapply(outSha, function(l) c(l$commit$committer$date, l$sha)))), stringsAsFactors=FALSE)
  commits$posixDate <- as.POSIXct(gsub('T|Z',' ', commits[,1]))

  sha <- (commits[order(commits$posixDate, decreasing=TRUE),])[1,2]
  return(sha)
}



