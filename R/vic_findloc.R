#' Title
#'
#' @param p1 location in the format of c(lat, lon)
#' @param brng initial bearing
#' @param dis distance in m
#'
#' @return
#' @export
#'
#' @examples vic_finddis(c(10,10), 30, 18000)
vic_findloc = function(p1, brng, dis, a = 6378137.0, f = 1/298.257223563){
  #Constants
  b = (1-f)*a

  #Input!
  #note: φ = latitude
  #p1 = c(lat, lon)
  lat_1 = deg2rad(p1[1])
  lon_1 = deg2rad(p1[2])
  s = dis

  #Calculation
  sin_a1 = sin(deg2rad(brng))
  cos_a1 = cos(deg2rad(brng))

  tan_u1 = (1-f) * tan(lat_1)
  cos_u1 = 1/sqrt((1+ tan_u1^2))
  sin_u1 = tan_u1 * cos_u1

  sigma_1 = atan2(tan_u1, cos_a1) #σ1 = angular distance on the sphere from the equator to P1
  sin_a = cos_u1 * sin_a1 #α = azimuth of the geodesic at the equator
  cos_sqa = 1-sin_a^2
  uSq = cos_sqa * (a^2 - b^2) / (b^2)

  A = 1 + uSq/ 16384*(4096 + uSq * (-768 + uSq*(320 - 175 * uSq)))
  B = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))

  sigma = s / (b*A)
  sin_sigma = 0
  cos_sigma = 0
  cos_2signmam = 0

  sigma_2 = 0
  while(abs(sigma - sigma_2) > (10^-12)){
    cos_2signmam = cos(2*sigma_1 + sigma)
    sin_sigma = sin(sigma)
    cos_sigma = cos(sigma)
    delta_sigma = B * sin_sigma * (cos_2signmam + B / 4 *(cos_sigma*(-1 + 2 * cos_2signmam^2)
                                                          - B/6*cos_2signmam*(-3+4*sin_sigma^2)*(-3+4*cos_2signmam^2)))
    sigma_2 = sigma
    sigma = s/(b*A) + delta_sigma
  }

  x = sin_u1 * sin_sigma - cos_u1 * cos_sigma * cos_a1
  lat_2 = atan2(sin_u1 * cos_sigma + cos_u1 * sin_sigma*cos_a1,
                (1-f)*sqrt(sin_a^2 + x^2))

  lambda = atan2(sin_sigma * sin_a1, cos_u1*cos_sigma - sin_u1 * sin_sigma * cos_a1)
  C = f/16*cos_sqa * (4+f*4-3*cos_sqa)
  L = lambda - (1-C) * f * sin_a * (sigma + C * sin_sigma * (cos_2signmam + C * cos_sigma * (-1 + 2 * cos_2signmam^2)))
  lon_2 = lon_1 + L
  a_2 = atan2(sin_a, -x)
  c(rad2deg(lat_2), rad2deg(lon_2), rad2deg(a_2))
}
