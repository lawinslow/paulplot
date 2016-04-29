
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
