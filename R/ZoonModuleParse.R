#' Parse a module file to read roxygen tags
#'
#' This is a stand in for the parse_file function in roxygen which
#' is not exported.
#'
#' @param modulePath The path to a zoon module .R script
#'
#' @return Named list of roxygen tags
#' @name ZoonModuleParse
#' @export
#' @import roxygen2
#' @importFrom utils packageVersion


ZoonModuleParse <- function(modulePath) {

  # Read in the file
  lines <- readLines(modulePath, -1)

  # Get the tags out
  roc <- roxygen2::rd_roclet()
  rd <- roxygen2::roc_proc_text(roc, lines)[[1]]

  # Behaviour varies on the version of
  # Roxygen2 being used
  if(packageVersion("roxygen2") >= "7.0.0"){
    rds <- rd$sections
    res <- lapply(
      names(rds),
      function(field, x) x[[field]]$value,
      x = rds
    )
    names(res) <- names(rd$sections)
    
    if ("section" %in% names(res)) {
      sections <- paste0(res$section$title, ":", res$section$content)
      
      # name sections
      names(sections) <- rep("section", length(sections))
      
      # replace old section with new sections
      res[["section"]] <- NULL
      res <- append(res, sections)
      
    }
  } else {
    
    if ("fields" %in% names(rd)) {
      
      field_fun <- function (field, x) {
        
        if ("values" %in% names(x[[field]])) {
          
          named_tag <- list(x[[field]]$values)
          names(named_tag) <- field
          return (unlist(named_tag))
          
        } else {
          
          list_tag <- list()
          
          for (i in seq_along(x[[field]]$title)) {
            list_tag <- c(list_tag,
                          list(list(name = x[[field]]$title[i],
                                    content = x[[field]]$content[i])))
          }
          return (list_tag)
          
        }
      }
    
      res <- lapply(names(rd$fields),
                    field_fun,
                    rd$fields)
      
      names(res) <- names(rd$fields)
      
      if ("param" %in% names(res))
        names(res$param) <- gsub("^param.", "", names(res$param))
      
    } else if(length(rd)==1){
      res <- lapply(
        names(rd),
        function(field, x) x[[1]][[field]]$values,
        rd
      )
    } else {
      res <- lapply(
        names(rd),
        function(field, x) x[field]$values,
        x = rd
      )
    }
  
    # flatten 'sections'
    if ("section" %in% names(res)) {
      
      sections <- lapply(
        res$section,
        function (x) {
          paste0(x$name, ":", x$content)
        }
      )
      
      # name sections
      names(sections) <- rep("section", length(sections))
  
      # replace old section with new sections
      res[["section"]] <- NULL
      res <- append(res, sections)
      
    }
  }
  
  # format params
  if ("param" %in% names(res)) {
    
    params <- lapply(
      names(res$param),
      function (field, x) {
        list(name = field,
             description = as.character(x[field]))
      },
      res$param
    )

    names(params) <- rep("param", length(params))
    res[["param"]] <- NULL
    res <- append(params, res)
    
  }

  return(res)
  
}
