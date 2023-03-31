plot_chm_hgt_vs_sampled_hgt <- function(tree_hgt, CHM_hgt, resultsDir, fullSite, fn_suffix, sub_text, screen = FALSE) {
  ## A function to plot the Lidar measured tree heights (CHM_hgt) vs the ground measured tree heights (tree_hgt)
  ## from NEON data
  
  ## x and y plot labels
  xlab = "Ground Measured Canopy Height (m)"
  ylab = "Lidar Canopy Height Model (m)"
  
  ## Open the plot file
  if(!screen) png(filename = paste0(resultsDir, fullSite,fn_suffix,".png"), width = 800, height = 600)
  
  plim = c(0, max(c(tree_hgt, CHM_hgt))) #max of x and y
  plot.window(xlim = plim, ylim = plim)

  plot(CHM_hgt ~ 0 + tree_hgt, pch=20, 
       xlim = plim, ylim = plim,
       xlab = xlab, ylab = ylab,
       main = c(fullSite, sub_text))
  
  ## Linear least squares fit
  regress = lm(CHM_hgt ~ 0 + tree_hgt)
  lines(plim, plim*regress$coefficients, col="black", lwd = 3)
  
  ## Correlation coefficient
  corr_coef = cor(tree_hgt, CHM_hgt)
  print(sprintf("Correlation Coefficient: %.3f", cor(tree_hgt, CHM_hgt) ))
  
  plotText = c(sprintf("Slope: %.3f", regress$coefficients), 
               sprintf("R Squared: %.3f", summary(regress)[9]), 
               sprintf("N points: %d", length(CHM_hgt)), 
               sprintf("Correlation: Coeff: %.3f", corr_coef))
  text(c(0, 0, 0, 0), plim[2]-array(1:4), plotText, adj = 0)
  if(!screen) dev.off()
  
}