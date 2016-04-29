#' @title Plot the path on a leaflet map
#' 
#' @description
#' 
#' @param d Data.frame containing all data (must have Lat/Lon/T_Depth columns)
#' @param colvar Variable to color path points (defaults to T_Depth)
#' 
#' @import leaflet
#' 
#' @examples 
#' 
#' data(greenlake)
#' 
#' plot_map(greenlake)
#' 
#' #the map can be colored by different variables
#' plot_map(greenlake, colvar='pH')
#' 
#' 
#' @export 
plot_map = function(d, colvar='T_Depth'){
  
  
  if(!all(c('Lat', 'Long', colvar) %in% names(d))){
    stop('Lat, Long and ', colvar, ' must be columns in supplied data.frame')
  }
  
  d = clean_common_issues(d)
  
  
  pal = colorNumeric(
    palette = "RdYlBu",
    domain = d[, colvar]
  )
  
  
  l = leaflet(d)  %>% addProviderTiles('Esri.WorldImagery') %>% addCircleMarkers(lng=~Long, lat=~Lat, radius = 1, color=~pal(d[,colvar]))
  
  l = l %>% addLegend("bottomright", pal = pal, values = d[,colvar],
                      title = colvar,
                      opacity = 1)
  
  return(l)
}
