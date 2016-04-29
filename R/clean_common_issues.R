clean_common_issues = function(d){
  
  #common issue I've noticed
  
  ## NAs in T_Depth
  d$T_Depth = approx(d$RECORD[!is.na(d$T_Depth)], d$T_Depth[!is.na(d$T_Depth)], xout=d$RECORD)$y
  
  return(d)
}
