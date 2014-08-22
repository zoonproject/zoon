
#'Turns named options into list ready to be used by workflow()
#'
#'@param module The module name or URL.
#'@param ... Any other parameters or options needed by that  module.
#'           All extra options must be named i.e. ModuleOptions('x', x=1)
#'           Not ModuleOptions('x', 1).
#'
#'
#'@return A list with all module options and the module name/URL in.
#'@name ModuleOptions
#'
#'@export
#'@examples print('No examples yet')

GetModuleList <- function(){

  library("httr")

  files <- gh_list_files('zoonproject', 'modules')
  mods <- files[grep('^R/', files)]
  names <- gsub('^R/|.R$', '', mods)
  return(names)
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

gh_list_files <- function(owner, repo, ...) {
  token <- github_auth(appname = "zoonproject", key = "945195231bd7edd336c9")

  sha <- FindNewestCommit(owner, rep, token)
  
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



