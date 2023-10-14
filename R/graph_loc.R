#' Plots x, y coordinate in ggplot
#'
#' @param df dataframe with Type, S_x and S_y
#'
#' @return
#' @export
#'
#' @examples graph_loc(cre_tran())
graph_loc = function(df){
  trh_lib()
  if(is.list(df)){
    detail = df$detail
    df = df$data
  }
  graph = ggplot(df, aes(x = S_x, y = S_y, color = Type, shape = Type, fill = Type))+
    geom_point(size = 2)+
    labs(x = "X-coordinate", y = "Y-coordinate")+
    scale_shape_manual(breaks = c("Ref_SP", "Site", "Ref_EP"),
                       values = c(      23,     19,      23))+
    scale_color_manual(breaks = c( "Ref_SP",    "Site",  "Ref_EP"),
                       values = c("#3AD900", "#75AADB", "#E85C85"))+
    scale_fill_manual (breaks = c( "Ref_SP",    "Site",  "Ref_EP"),
                       values = c("#3AD900", "#75AADB", "#E85C85"))+
    theme_bw()
  list(graph = graph, data = df, detail = detail)
}
