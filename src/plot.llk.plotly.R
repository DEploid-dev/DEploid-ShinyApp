
dEploidPrefix = ""
fun.dEploidPrefix <- function ( prefix ){
  if ( prefix == "" ){
    stop ("dEprefix ungiven!!!")
  }
  
  return ( list ( propFileName = paste(prefix, ".prop",   sep = ""),
                  hapFileName  = paste(prefix, ".hap",    sep = ""),
                  llkFileName  = paste(prefix, ".llk",    sep = ""),
                  dicLogFileName  = paste(prefix, "dic.log", sep = "") ) )
}
dEploidOutput = fun.dEploidPrefix(dEploidPrefix)
llkTable = read.table( dEploidOutput$llkFileName, header=F)
############################
fun.llk <- function(cov.ref, cov.alt, f.samp, err=0.01, fac=100) {
  f.samp<-f.samp+err*(1-2*f.samp);
  llk<-lbeta(cov.alt+f.samp*fac, cov.ref+(1-f.samp)*fac)-lbeta(f.samp*fac,(1-f.samp)*fac);
  #  llk<-lgamma(fac*f.samp+cov.alt)+lgamma(fac*(1-f.samp)+cov.ref)-lgamma(fac*f.samp)-lgamma(fac*(1-f.samp));
  if (sum(is.nan(llk))>1){
    print("f.samp = ")
  }
  return(llk);
}


#############################
fun.dic.by.llk.var <- function ( tmpllk ){
  return (  mean(-2*tmpllk) + var(-2*tmpllk)/2 )# D_bar + 1/2 var (D_theta), where D_theta = -2*tmpllk, and D_bar = mean(D_theta)
}

#############################
fun.dic.by.theta <- function ( tmpllk, thetallk ){
  DIC.WSAF.bar = -2 * sum(thetallk)
  return (  mean(-2*tmpllk) + (mean(-2*tmpllk) - DIC.WSAF.bar) ) # D_bar + pD, where pD = D_bar - D_theta, and D_bar = mean(D_theta)
}

#############################
plot.llk <- function (llk, ref, alt, expWSAF, title = "", cex.lab = 1, cex.main = 1, cex.axis = 1 ){
  llk = PG0390CoverageTxt.deconv$llks
  range(llk)
  table(PG0390CoverageTxt.deconv$Haps)
  llkEvent = llkTable$V1
  #    llk_sd = sd(llk)
  #    llk_range = range(llk)
  
  #    dic.by.var = fun.dic.by.llk.var (llk)
  #    dic.by.theta = fun.dic.by.theta ( llk, fun.llk(ref, alt, expWSAF))
  
  plot(llk, lty=2, type="l", col="black", xlab="Iteration", ylab="LLK", main=title,
       cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
  updateSingleAt = which(llkEvent == 1)
  updateBothAt = which(llkEvent == 2)
  updatePropAt = which(llkEvent == 0)
  index = c(1:length(llk))
  points(index[updateSingleAt], llk[updateSingleAt], cex = 0.6, col="red")
  points(index[updateBothAt], llk[updateBothAt], cex = 0.6, col="blue")
  points(index[updatePropAt], llk[updatePropAt], cex = 0.6, col="green")
  
}
