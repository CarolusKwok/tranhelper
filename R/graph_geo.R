#' Plots lon, lat coordinate in ggplot
#'
#' @param df dataframe with Type, S_lon and S_lat
#' @param geo draws Natural Earth map
#'
#' @return
#' @export
#'
#' @examples
#' graph_geo(trs_geo(cre_tran(), SP = c(0,0), BRNG = 0))
graph_geo = function(df, geo = F){
  trh_lib()
  if(is.list(df)){
    detail = df$detail
    df = df$data
  }

  if(geo == T){
    data_land  = fortify(ne_download(scale = 10, category = 'physical', type = 'land'))
    data_minor = fortify(ne_download(scale = 10, category = 'physical', type = 'minor_islands'))
    data_river = fortify(ne_download(scale = 10, category = 'physical', type = 'rivers_lake_centerlines'))
    data_lake  = fortify(ne_download(scale = 10, category = 'physical', type = 'lakes'))


    graph = ggplot(data_land)+
      geom_polygon(data =  data_land, aes(group = group, x = long, y = lat), fill = "#ffeebb", color = "#ffeebb")+
      geom_polygon(data = data_minor, aes(group = group, x = long, y = lat), fill = "#ffeebb", color = "#ffeebb")+
         geom_path(data = data_river, aes(group = group, x = long, y = lat),                   color = "#86d2f5")+
      geom_polygon(data =  data_lake, aes(group = group, x = long, y = lat), fill = "#86d2f5", color = "#86d2f5")+
      theme_bw () +
      theme(panel.background = element_rect(fill = "#86d2f5"),
            panel.grid.major = element_line(size = 0.25, color = "#FFFFFF", linetype = "solid"),
            panel.grid.minor = element_line(size = 0.25, color = "#FFFFFF", linetype = "solid"),
            axis.text.y = element_text(angle = 90, hjust = 0.5))+
      coord_cartesian(xlim = c(min(df$S_lon), max(df$S_lon)), ylim = c(min(df$S_lat),max(df$S_lat)))
  }
  if(geo == F){
    graph = ggplot(df, aes(x = S_lon, y = S_lat, color = Type, shape = Type, fill = Type))+
      theme_bw()+
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  }

  graph = graph +
    geom_point(data = df, aes(x = S_lon, y = S_lat, color = Type, shape = Type, fill = Type), size = 2)+
    labs(x = "Longitude (°)", y = "Latitude (°)")+
    scale_x_continuous(labels = comma)+
    scale_y_continuous(labels = comma)+
    scale_shape_manual(breaks = c("Ref_SP", "Site", "Ref_EP"),
                       values = c(      23,     19,      23))+
    scale_color_manual(breaks = c( "Ref_SP",    "Site",  "Ref_EP"),
                       values = c("#3AD900", "#75AADB", "#E85C85"))+
    scale_fill_manual (breaks = c( "Ref_SP",    "Site",  "Ref_EP"),
                       values = c("#3AD900", "#75AADB", "#E85C85"))

  if(exists("detail")){
    df = list(graph = graph,
              data = df,
              detail = detail)
  }
  remove(graph)
  remove(detail)
  df
}
