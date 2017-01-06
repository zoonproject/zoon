zoon 0.6 (2017-01-06)
=========================
  
### MAJOR CHANGES

* Allow covariate rasters to be at different resolutions (they are combined)
* Allow Occurrence data and covariate rasters to be in a range of projections. They will be converted to a common CRS and combined.
* Failing workflows save partial workflow and message is returned to console
* summary.zoonWorkflow update to produce an HTML report
* Better debugging in BuildModule when a module fails

### MINOR CHANGES
  
* Test updates and coverage increased
* Vignettes updated
* Fix bug when module not found
* Improve manual pages
* Improve formatting resulting from BuildModule
* Acceptable module testing time increased to 5 minutes
* Fuzzy search added to LoadModule
