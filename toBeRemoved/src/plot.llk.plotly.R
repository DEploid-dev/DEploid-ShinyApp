# 
# ############################
# fun.llk <- function(cov.ref, cov.alt, f.samp, err=0.01, fac=100) {
#   f.samp<-f.samp+err*(1-2*f.samp);
#   llk<-lbeta(cov.alt+f.samp*fac, cov.ref+(1-f.samp)*fac)-lbeta(f.samp*fac,(1-f.samp)*fac);
#   #  llk<-lgamma(fac*f.samp+cov.alt)+lgamma(fac*(1-f.samp)+cov.ref)-lgamma(fac*f.samp)-lgamma(fac*(1-f.samp));
#   if (sum(is.nan(llk))>1){
#     print("f.samp = ")
#   }
#   return(llk);
# }
# 
# #############################
# fun.dic.by.llk.var <- function ( tmpllk ){
#   return (  mean(-2*tmpllk) + var(-2*tmpllk)/2 )# D_bar + 1/2 var (D_theta), where D_theta = -2*tmpllk, and D_bar = mean(D_theta)
# }
# 
# #############################
# fun.dic.by.theta <- function ( tmpllk, thetallk ){
#   DIC.WSAF.bar = -2 * sum(thetallk)
#   return (  mean(-2*tmpllk) + (mean(-2*tmpllk) - DIC.WSAF.bar) ) # D_bar + pD, where pD = D_bar - D_theta, and D_bar = mean(D_theta)
# }

#############################
plotLLKPlotly <- function (llk, llkEvent){

  #    llk_sd = sd(llk)
  #    llk_range = range(llk)
  
  #    dic.by.var = fun.dic.by.llk.var(llk)
  #    dic.by.theta = fun.dic.by.theta(llk, fun.llk(ref, alt, expWSAF))
  
  x = c(1:length(llk))
  llks = data.frame(x, llk, llkEvent)
  llks$single = ifelse(llkEvent == 1, llk, NA)
  llks$both = ifelse(llkEvent == 2, llk, NA)
  llks$prop = ifelse(llkEvent == 0, llk, NA)
  
  # plot_ly(llks, x = ~x, y = ~single, type = "scatter", mode = "markers",
  #         marker = list(size = 3, color = "#ff0054"), name = "Single") %>%
  #   add_trace(y = ~both, mode = "markers", 
  #             marker = list(size = 3, color = "#005dff"), name = "Both") %>%
  #   add_trace(y = ~prop, mode = "markers", 
  #             marker = list(size = 3, color = "#00ff87"), name = "Prop") %>%
  #   
  #   add_trace(y = ~llk, mode = "lines+markers", name = "llk",
  #             line = list(color = "black", width = 1, dash = "dot"))
    
    
  
  plot_ly(llks, x = ~x, y = ~llk, type = "scatter", mode = "lines+markers", 
          line = list(color = "black", width = 1, dash = "dot"), name = "llk") %>%
    add_trace(y = ~single, mode = "markers", 
              marker = list(size = 3, color = "#ff0054"), name = "Single") %>%
    add_trace(y = ~both, mode = "markers", 
              marker = list(size = 3, color = "#005dff"), name = "Both") %>%
    add_trace(y = ~prop, mode = "markers", 
              marker = list(size = 3, color = "#00ff87"), name = "Prop") %>%
    layout(margin = list(l = 95, r = 25, b = 50, t = 80, pad = 0),
           title = "LLK", font = list(size = 18, colot = "black"),
           xaxis = list(title = "x", 
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           yaxis = list(title = "value",
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")))
}
