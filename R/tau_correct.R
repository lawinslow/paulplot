tau_correct = function(z, tau=2){
  
  x = seq_along(z)
  
  correction = c(tau * diff(z), 0)
  
  z = z + correction
  
  return(z)
}