# This is a custom helper function for building the zoon vignettes
# Edit the vignettes in the vignettes_prebuild folder
# then run this script to build the new md>Rmd>HTML>png
# and place them correctly in the file structure
quiet <- FALSE

# Build the vignettes
to_knit <- list.files('vignette_prebuild/', pattern = '.Rmd$', full.names = TRUE)

for(i in to_knit){
 
  cat('Building ', i, '...\n') 
  md <- knitr::knit(i, quiet = quiet)
  
  # convert md to Rmd and move to vignettes folder
  file.rename(from = md,
              to = file.path('vignettes', gsub('.md$', '.Rmd', md)))
  
  #Move images across
  cat('Moving images...\n')
  for(image in list.files(pattern = '.png$')){
    
    file.rename(from = image,
                to = file.path('vignettes', image))
    
  }
  
  for(image in list.files('vignette_prebuild', pattern = '.svg$', full.names = TRUE)){
    
    file.copy(from = image,
              to = file.path('vignettes', basename(image)))
    
  }
  
  # Create html version and put in inst/doc
  cat('Building HTML...\n\n')
  rmarkdown::render(input = file.path('vignettes', gsub('.md$', '.Rmd', md)),
                    output_format = 'html_document',
                    clean = TRUE,
                    output_dir = 'inst/doc', output_options = list(keep_md = FALSE),
                    quiet = quiet)
}