
f_plot_rasterMap <- function(r_in, colorpal,
                                   titleLabel, N = 12){
  
  mybreaks <- terra::global(r_in, quantile, probs=seq(0,1,1/N), na.rm=TRUE) %>% 
    as.numeric() %>% 
    round(digits = 2)
    
  r_projected = terra::project(x = r_in, y = "+proj=robin", method = "near")
  
  map =  
    
    tm_shape(r_projected, raster.warp = FALSE) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks =  mybreaks,
              palette = colorpal,
              labels = as.character(mybreaks),
              showNA = F,
              colorNA = NULL,
              title = titleLabel, 
              legend.is.portrait = FALSE)+
    tm_shape(sf_adm0) + 
    tm_borders(col = "grey", lwd = 0.1) +
    tm_layout(legend.outside.position = "bottom",
              legend.outside.size = 0.1,
              legend.width = .75,
              legend.height = .05,
              legend.bg.color = F,
              legend.outside = TRUE,
              legend.position = c(0.35, 0.75),
              main.title.position = "centre",
              frame = FALSE)
  map
  
  
  return(map)
  
  
}