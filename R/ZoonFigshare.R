#' @title ZoonFigshare
#'
#' @description This function uploads a zoon workflow object to Figshare. To share
#' your workflow with the community please set your workflow to public on figshare
#' after using this function. If your workflow is made public it will automatically
#' appear on the Zoon website.
#' 
#' @param zoonWorkflow A zoonWorkflow object as returned by the function \code{Workflow}
#'
#' @param title String giving the title of hte workflow
#'
#' @param description String describing the workflow 
#'
#' @param authors Charactor vector of full authors names 
#'
#' @param categories Character vector of figshare categories e.g. ecology. 
#'
#' @param tags Character vector of searchable tags. 
#'
#' @importFrom rfigshare fs_new_article
#' @export

ZoonFigshare <- function(zoonWorkflow, title = 'My Zoon Workflow',
                         description = 'zoon workflow',
                         authors = 'zoon', categories = 'SDM', tags = 'zoon'){
  
  temp_save <- file.path(tempdir(), 'zoonTMPfigshare.RData')
  
  save(zoonWorkflow, file = temp_save)
  
  
  id <- fs_new_article(title = title,
                       description = description, 
                       type = "dataset",
                       authors = authors,
                       tags = c(tags, 'zoonWorkflow'),
                       categories = 'ecology',
                       files = temp_save,
                       visibility = "private")
  
  message('To share your workflow with the ZOON community remember to make it public on Figshare')
  
  if(is.numeric(id)) browseURL(paste('http://figshare.com/preview/_preview/',
                                     id, sep = '')) 
  
  file.remove(temp_save)
  
  return(invisible(NULL))
  
}