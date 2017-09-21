#' @title ZoonFigshare
#'
#' @description This function uploads a zoon workflow object to Figshare. To
#'   share your workflow with the community please set your workflow to public
#'   on figshare after using this function. If your workflow is made public it
#'   will automatically appear on the Zoon website.
#'
#' @param zoonWorkflow A zoonWorkflow object as returned by the function
#'   \code{Workflow}
#'
#' @param title String giving the title of the workflow
#'
#' @param description String describing the workflow
#'
#' @param authors Character vector of full authors names
#'
#' @param categories Character vector of figshare categories e.g. ecology.
#'
#' @param tags Character vector of searchable tags.
#'
#' @importFrom rfigshare fs_new_article
#' @importFrom utils browseURL
#' @export

ZoonFigshare <- function(zoonWorkflow,
                         title = "My Zoon Workflow",
                         description = "zoon workflow",
                         authors = "zoon",
                         categories = "SDM",
                         tags = "zoon") {

  # Create the filename
  tx <- gsub(" ", "_", title)
  tx <- substr(tx, 1, min(20, nchar(tx)))
  datasave <- file.path(tempdir(), paste0(tx, ".RData"))
  metasave <- file.path(tempdir(), paste0(tx, "_metadata.txt"))
  # Write the metadata to file
  WriteWorkflowMetadata(
    zoonWorkflow, title, description,
    authors, categories, tags,
    filename = metasave
  )

  save(zoonWorkflow, file = datasave)


  id <- fs_new_article(
    title = title,
    description = description,
    type = "fileset",
    authors = authors,
    tags = c(tags, "zoonWorkflow"),
    categories = "ecology",
    files = c(
      datasave,
      metasave
    ),
    visibility = "private"
  )

  message("To share your workflow with the ZOON community remember ",
          "to make it public on Figshare")

  if (is.numeric(id))
    browseURL(paste0("http://figshare.com/preview/_preview/", id))

  file.remove(metasave, datasave)

  return (invisible(NULL))
}
