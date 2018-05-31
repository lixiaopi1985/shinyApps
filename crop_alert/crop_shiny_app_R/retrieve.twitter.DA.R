retrieve.twitter.DA = function(mx.aggre.custom){

  # load states
  states = ggplot2::map_data('state')
  basemap = ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", col = 'white') + coord_fixed(1.3) + guides(fill = FALSE)
  
  outmap = basemap + 
    geom_point(data = mx.aggre.custom, aes(x = avg_long, y = avg_lat, size = total)) +
    geom_text_repel(data = mx.aggre.custom, aes(x = avg_long, y = avg_lat, label = pathogen_reported)) +
    ggtitle(paste0("Plant pathogen reported on twitter ", "(Year: ", mx.aggre.custom$year, " Month:", mx.aggre.custom$month, ")")) + theme(plot.title = element_text(hjust = 0.5))
  
  print(outmap)
}