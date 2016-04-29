#' @title Plot heat map of overall profile
#' 
#' @description Creates single heat map of a chosen variable
#' 
#' @param d Data.frame containing all data (must have Lat/Lon/T_Depth columns)
#' @param colvar Variable to color path points (defaults to T_Depth)
#' @param xvar Select a different variable to define the x-axis
#' @param yvar Select a different variable to define the y-axis
#' @param col Select different colormap (passed in as vector of colors, for example, try \code{rainbow(12)})
#' @param tau 
#' Rudimentary sensor lag correction. Zero (no correction) is default. Larger value applies 
#' larger lag correction, though it adds noise at the same time (not working well right now)
#' 
#' 
#' @import reshape2
#' 
#' @examples 
#' data(greenlake)
#' 
#' plot_profile(greenlake)
#' 
#' 
#' #Can change the heatmap variable if you want
#' plot_profile(greenlake, colvar='Sled_chl_A')
#' 
#' 
#' @export
plot_profile = function(d, colvar='Sled_YSI_Temp', xvar='RECORD', yvar='Sled_Depth', col=rev(heat.colors(24)), fig.out=NULL, tau=0){
  
  z = tau_correct(d[,colvar], tau)
  
  plotdata = custom_2d_interp(d[,xvar], d[,yvar], z)
  
  
  if(!is.null(fig.out)){
    png(fig.out, res=450, width=3200, height=1800)
  }
  
  image(plotdata$x, plotdata$y, plotdata$z, ylim=rev(range(plotdata$y)), col=col, ylab=yvar, xlab=xvar)
  lines(d[,xvar], d[,yvar], lwd=0.5)
  
  lines(d[,xvar], d[,'T_Depth'], lwd=2)
  
  if(!is.null(fig.out)){
    dev.off() 
  }
}
