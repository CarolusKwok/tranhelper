#' HI
#'
#' @param obj1 hi
#' @param obj2 hi
#' @param shiftx hi
#' @param shifty hi
#'
#' @return
#' @export
#'
#' @examples trs_join(cre_twid(), cre_twid(), shiftx = 1, shifty = 1)
trs_join = function(obj1, obj2, shiftx = 0, shifty = 0){
  detail1 = obj1$detail
  detail2 = obj2$detail

  data1 = obj1$data
  data2 = obj2$data

  service1 = detail1$service
  service2 = detail2$service

  if(grepl("geo", service1, fixed = T) | grepl("geo", service2, fixed = T)){
    message("Note: This function removes geo from data!")
    message("Confirm? (Y/N)")
    Confirm = readline(prompt="Confirm: ")
    if(Confirm == "Y" & grepl("geo", service1, fixed = T)){
      detail1$Service = gsub("+ geo", "", detail1$Service)
      service1 = detail1$Service

      data1 = data1 %>%
        select(-S_lat, -S_lon)
    }
    if(Confirm == "Y" & grepl("geo", service2, fixed = T)){
      detail2$Service = gsub("+ geo", "", detail2$Service)
      service2 = detail2$Service

      data2 = data2 %>%
        select(-S_lat, -S_lon)
    }
    if(Confirm != "Y"){
      return(message("ABORTED!"))
    }
  }

  if(!("Trans" %in% colnames(data1))){
    data1 = data1 %>%
      mutate(Trans = 1) %>%
      relocate(Trans, .before = Type)
  }
  data2 = data2 %>%
    mutate(Trans = 1 + max(data1$Trans)) %>%
    mutate(S_x = S_x + shiftx, S_y = S_y + shifty)

  detail = bind_rows(detail1, detail2)

  data = bind_rows(data1, data2) %>%
    mutate(S_dis = sqrt(S_x^2 + S_y^2)) %>%
    mutate(S_brn = rad2deg(atan(abs(S_y)/abs(S_x)))) %>%
    mutate(S_brn = ifelse(S_x < 0, S_brn + 270, ifelse(S_x > 0, 90 - S_brn, ifelse(S_x == 0, 0, NA))))
  obj = list(data, detail)
  obj
}
