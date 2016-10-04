#' Change the CRS of occurrence or covariate data
#'
#' Takes a dataframe returned by an occurrence module or a raster object from a covariate module and converts the CRS to lat/long so tht everything works together. 
#'
#'@param data The object returned by an occurrence or covariate module.
#'@param module The module the object came from, one of 'occurrence' or 'covariate'
#'@return The same object as in data, but with CRS changed
#'
#'@name TransformCRS
#'@import rgdal
#'@examples \dontrun{
#' w <- workflow(UKAnophelesPlumbeus,
#'               UKAir,
#'               Background(n = 70), 
#'               LogisticRegression,
#'               PrintMap)
#'
#' w2 <- ChangeWorkflow(w,
#'                      output = PrintMap)
#'}

TransformCRS <- function(data, module){

  if(!module %in% c('occurrence', 'covariate')) stop('In TransformCRS: module must be occurrence or covariate')
  
  if(module == 'occurrence'){
    
    if('crs' %in% tolower(colnames(data))){
      
      crs_current <- as.character(unique(data[,grep('crs', tolower(names(data)))]))
      
      if(length(crs_current) > 1){
        stop('In occurrence module: There is more than one CRS type specified in the CRS column currently we only support one')
      }
      
      LL <- cbind(X = as.numeric(data$longitude),
                  Y = as.numeric(data$latitude))
      
      LL_omit <- na.omit(LL)
      
      LL_points <- SpatialPoints(coords = LL_omit, proj4string = CRS(crs_current))
      
      LL_new <- spTransform(LL_points,
                            CRS("+init=epsg:4326"))
      
      XY_new <- coordinates(LL_new)
      
      # merge back to orignals (containing NAs)
      XY_NAs <- LL
      XY_NAs[-attr(LL_omit, "na.action"), ] <- XY_new
      XY_NAs[attr(LL_omit, "na.action"), ] <- NA
      
      data$longitude <- XY_NAs[,1]
      data$latitude <- XY_NAs[,2]
      
      return(data)
      
    } else {
      
      # The data should be lat long so check a few things and error 
      # if not lat long
      t1 <- data$longitude <= 180 & data$longitude >= -180
      t2 <- data$latitude <= 90 & data$latitude >= -90

      if(any(!c(t1, t2))){
       
        stop(paste('Your occurrence data lat/long positions dont appear',
                   'to be in lat/long, some fall out of range of possible',
                   'values (lat -90-90, long -180-180). If your data are',
                   'not lat/long add a column named "crs" giving the proj4string',
                   ' for the coordinate system you are using (for example',
                   '"+init=epsg:27700" for easting/northing data)')) 
        
      }
      
      return(data)
      
    }
    
  } else if(module == 'covariate'){
    
    
    
    return(data)
    
  }
}