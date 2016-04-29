library(leaflet)
library(reshape2)

gl = read.table('data/Green_Lake_Location_WQ.csv', sep=',', header=TRUE, as.is=TRUE)
sep = read.table('data/Sept2015_P5.csv', sep=',', header=TRUE, as.is=TRUE,comment.char = '', na.strings = '#VALUE!')


clean_common_issues = function(d){
  
  #common issue I've noticed
  
  ## NAs in T_Depth
  d$T_Depth = approx(d$RECORD[!is.na(d$T_Depth)], d$T_Depth[!is.na(d$T_Depth)], xout=d$RECORD)$y
  
  return(d)
}


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





custom_2d_interp = function(x, y, z, by='x'){
  
  d = data.frame(x, y, z)
  
  newx = seq(min(x), max(x), length.out = 1000)
  newy = seq(min(y), max(y), length.out=100)
  
  d$xi = cut(x, breaks=newx, labels=FALSE)
  d$yi = cut(y, breaks=newy, labels=FALSE)
  
  zmat = acast(formula = xi~yi, data=d, value.var = 'z', fun.aggregate = mean)
  
  for(i in 1:length(zmat[1,])){
    
    dz = zmat[,i]
    notna = !is.na(dz)
    if(sum(notna) < 2){
      next
    }
    di = seq_along(dz)
    
    zmat[,i] = approx(di[notna], dz[notna], xout=di, rule=1)$y
  }

  return(list(x=newx, y=newy, z=zmat))
}


tau_correct = function(z, tau=2){
  
  x = seq_along(z)
  
  correction = c(tau * diff(z), 0)
  
  lines(z + correction, col='red')
  
  return(z)
}

#defaults to temp
plot_profile = function(d, colvar='Sled_YSI_Temp', fname=NULL, xvar='RECORD', yvar='Sled_Depth', col=rev(heat.colors(24)), fig.out=NULL, tau=0){
  
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
