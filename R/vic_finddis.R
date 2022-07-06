#' Finds distance between 2 points
#'
#' Via Vincenty's inverse formula
#' A direct translation of https://nathanrooy.github.io/posts/2016-12-18/vincenty-formula-with-python/ from python to R
#' Thanks ;D
#'
#' @param p1 Location of point 1, in the format of c(Lon, Lat). Both in degrees
#' @param p2 Location of point 2, in the format of c(Lon, Lat). Both in degrees
#'
#' @return
#' @export
#'
#' @examples vic_finddis((10, 10), (20, 20))
#'
vic_finddis = function(p1,p2){
  #prevent loop failure
  if(p1[1] == p2[1] & p1[2] == p2[2]){
    return(0)
  }

  #Constants
  a = 6378137.0
  f = 1/298.257223563
  b = (1-f)*a

  #p1 = c(Longitude, Latitude)

  Lat1 = p1[2]
  Lon1 = p1[1]

  #p2 = c(Longitude, Latitude)

  Lat2 = p2[2]
  Lon2 = p2[1]

  u1 = atan((1-f)*tan(deg2rad(Lon1)))
  u2 = atan((1-f)*tan(deg2rad(Lon2)))

  L = deg2rad(Lat2 - Lat1)

  Lambda = L

  sin_u1 = sin(u1)
  cos_u1 = cos(u1)
  sin_u2 = sin(u2)
  cos_u2 = cos(u2)

  maxIter = 200
  tol = 10**-12

  #Interation
  for(i in 0:maxIter){
    cos_lambda   = cos(Lambda)
    sin_lambda   = sin(Lambda)
    sin_sigma    = sqrt((cos_u2*sin(Lambda))**2 + (cos_u1*sin_u2-sin_u1*cos_u2*cos_lambda)**2)

    cos_sigma    = sin_u1*sin_u2 + cos_u1*cos_u2*cos_lambda
    sigma        = atan2(sin_sigma, cos_sigma)
    sin_alpha    = (cos_u1*cos_u2*sin_lambda) / sin_sigma
    cos_sq_alpha = 1 - sin_alpha**2
    cos2_sigma_m = cos_sigma - ((2*sin_u1*sin_u2)/cos_sq_alpha)
    C            = (f/16) * cos_sq_alpha * (4+f*(4-3*cos_sq_alpha))
    Lambda_prev  = Lambda
    Lambda       = L + (1-C)*f*sin_alpha*(sigma+C*sin_sigma*(cos2_sigma_m+C*cos_sigma*(-1+2*cos2_sigma_m**2)))

    diff = abs(Lambda_prev - Lambda)
    if(diff <= tol){
      break
    }
  }

  u_sq      = cos_sq_alpha*((a**2-b**2)/b**2)
  A         = 1 + (u_sq/16384) * (4096+u_sq*(-768+u_sq*(320-175*u_sq)))
  B         = (u_sq/1024) * (256+u_sq*(-128+u_sq*(74-47*u_sq)))
  delta_sig = B * sin_sigma *
    (cos2_sigma_m + 0.25*B*(cos_sigma*(-1+2*cos2_sigma_m**2) -
                            (1/6) * B * cos2_sigma_m * (-3+4*sin_sigma**2) * (-3+4*cos2_sigma_m**2)))

  self.m = b*A*(sigma-delta_sig)
  self.m
}
