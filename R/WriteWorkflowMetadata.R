WriteWorkflowMetadata <- function (zoonWorkflow,
                                   title,
                                   description,
                                   authors,
                                   categories,
                                   tags,
                                   filename) {
  
  # Write out the basic metadata
  write(paste("Title:", title), file = filename, append = TRUE)
  write(paste("Created:", Sys.time()), file = filename, append = TRUE)
  write(paste("Description:", description), file = filename, append = TRUE)
  write(paste("Authors:", authors), file = filename, append = TRUE)
  write(paste("Categories:", categories), file = filename, append = TRUE)
  write(paste("Tags:", tags), file = filename, append = TRUE)
  
  # Write out the module call
  write(paste("Call:", zoonWorkflow$call), file = filename, append = TRUE)
  
  versions <- do.call(cbind, zoonWorkflow$module.versions)
  version_text <- paste(versions[1, ],
                        versions[2, ],
                        sep = ", ",
                        collapse = "; ")
  
  # write out the module versions
  write(paste("Module versions:", version_text),
        file = filename,
        append = TRUE)
  
  return(filename)
}
