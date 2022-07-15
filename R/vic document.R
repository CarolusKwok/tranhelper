#https://www.movable-type.co.uk/scripts/latlong-vincenty.html

#For reading purpose only

#inverse(point) {
#if (!(point instanceof LatLonEllipsoidal)) throw new TypeError(`invalid point ‘${point}’`);
#if (this.height!=0 || point.height!=0) throw new RangeError('point must be on the surface of the ellipsoid');
#
#const p1 = this, p2 = point;
#const φ1 = p1.lat.toRadians(), λ1 = p1.lon.toRadians();
#const φ2 = p2.lat.toRadians(), λ2 = p2.lon.toRadians();
#
#// allow alternative ellipsoid to be specified
#const ellipsoid = this.datum ? this.datum.ellipsoid : LatLonEllipsoidal.ellipsoids.WGS84;
#const { a, b, f } = ellipsoid;

#const L = λ2 - λ1; // L = difference in longitude, U = reduced latitude, defined by tan U = (1-f)·tanφ.
#const tanU1 = (1-f) * Math.tan(φ1), cosU1 = 1 / Math.sqrt((1 + tanU1*tanU1)), sinU1 = tanU1 * cosU1;
#const tanU2 = (1-f) * Math.tan(φ2), cosU2 = 1 / Math.sqrt((1 + tanU2*tanU2)), sinU2 = tanU2 * cosU2;

#const antipodal = Math.abs(L) > π/2 || Math.abs(φ2-φ1) > π/2;

#let λ = L, sinλ = null, cosλ = null; // λ = difference in longitude on an auxiliary sphere
#let σ = antipodal ? π : 0, sinσ = 0, cosσ = antipodal ? -1 : 1, sinSqσ = null; // σ = angular distance P₁ P₂ on the sphere
#let cos2σₘ = 1;                      // σₘ = angular distance on the sphere from the equator to the midpoint of the line
#let cosSqα = 1;                      // α = azimuth of the geodesic at the equator

#let λʹ = null, iterations = 0;
#do {
#  sinλ = Math.sin(λ);
#  cosλ = Math.cos(λ);
#  sinSqσ = (cosU2*sinλ)**2 + (cosU1*sinU2-sinU1*cosU2*cosλ)**2;
#  if (Math.abs(sinSqσ) < 1e-24) break;  // co-incident/antipodal points (σ < ≈0.006mm)
#  sinσ = Math.sqrt(sinSqσ);
#  cosσ = sinU1*sinU2 + cosU1*cosU2*cosλ;
#  σ = Math.atan2(sinσ, cosσ);
#  const sinα = cosU1 * cosU2 * sinλ / sinσ;
#  cosSqα = 1 - sinα*sinα;
#  cos2σₘ = (cosSqα != 0) ? (cosσ - 2*sinU1*sinU2/cosSqα) : 0; // on equatorial line cos²α = 0 (§6)
#  const C = f/16*cosSqα*(4+f*(4-3*cosSqα));
#  λʹ = λ;
#  λ = L + (1-C) * f * sinα * (σ + C*sinσ*(cos2σₘ+C*cosσ*(-1+2*cos2σₘ*cos2σₘ)));
#  const iterationCheck = antipodal ? Math.abs(λ)-π : Math.abs(λ);
#  if (iterationCheck > π) throw new EvalError('λ > π');
#} while (Math.abs(λ-λʹ) > 1e-12 && ++iterations<1000); // TV: 'iterate until negligible change in λ' (≈0.006mm)
#if (iterations >= 1000) throw new EvalError('Vincenty formula failed to converge');

#const uSq = cosSqα * (a*a - b*b) / (b*b);
#const A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)));
#const B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)));
#const Δσ = B*sinσ*(cos2σₘ+B/4*(cosσ*(-1+2*cos2σₘ*cos2σₘ)-B/6*cos2σₘ*(-3+4*sinσ*sinσ)*(-3+4*cos2σₘ*cos2σₘ)));

#const s = b*A*(σ-Δσ); // s = length of the geodesic

#// note special handling of exactly antipodal points where sin²σ = 0 (due to discontinuity
#                                                                      // atan2(0, 0) = 0 but atan2(ε, 0) = π/2 / 90°) - in which case bearing is always meridional,
#// due north (or due south!)
#// α = azimuths of the geodesic; α2 the direction P₁ P₂ produced
#const α1 = Math.abs(sinSqσ) < ε ? 0 : Math.atan2(cosU2*sinλ,  cosU1*sinU2-sinU1*cosU2*cosλ);
#const α2 = Math.abs(sinSqσ) < ε ? π : Math.atan2(cosU1*sinλ, -sinU1*cosU2+cosU1*sinU2*cosλ);

#return {
#  distance:       s,
#  initialBearing: Math.abs(s) < ε ? NaN : Dms.wrap360(α1.toDegrees()),
#  finalBearing:   Math.abs(s) < ε ? NaN : Dms.wrap360(α2.toDegrees()),
#  iterations:     iterations,
#};
#}

#}





#direct(distance, initialBearing) {
#  if (isNaN(distance)) throw new TypeError(`invalid distance ${distance}`);
#  if (distance == 0) return { point: this, finalBearing: NaN, iterations: 0 };
#  if (isNaN(initialBearing)) throw new TypeError(`invalid bearing ${initialBearing}`);
#  if (this.height != 0) throw new RangeError('point must be on the surface of the ellipsoid');

#  const φ1 = this.lat.toRadians(), λ1 = this.lon.toRadians();
#  const α1 = Number(initialBearing).toRadians();
#  const s = Number(distance);

#  // allow alternative ellipsoid to be specified
#  const ellipsoid = this.datum ? this.datum.ellipsoid : LatLonEllipsoidal.ellipsoids.WGS84;
#  const { a, b, f } = ellipsoid;

#  const sinα1 = Math.sin(α1);
#  const cosα1 = Math.cos(α1);

#  const tanU1 = (1-f) * Math.tan(φ1), cosU1 = 1 / Math.sqrt((1 + tanU1*tanU1)), sinU1 = tanU1 * cosU1;
#  const σ1 = Math.atan2(tanU1, cosα1); // σ1 = angular distance on the sphere from the equator to P1
#  const sinα = cosU1 * sinα1;          // α = azimuth of the geodesic at the equator
#  const cosSqα = 1 - sinα*sinα;
#  const uSq = cosSqα * (a*a - b*b) / (b*b);
#  const A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)));
#  const B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)));

#  let σ = s / (b*A), sinσ = null, cosσ = null; // σ = angular distance P₁ P₂ on the sphere
#  let cos2σₘ = null; // σₘ = angular distance on the sphere from the equator to the midpoint of the line

#  let σʹ = null, iterations = 0;
#  do {
#   cos2σₘ = Math.cos(2*σ1 + σ);
#   sinσ = Math.sin(σ);
#   cosσ = Math.cos(σ);
#   const Δσ = B*sinσ*(cos2σₘ+B/4*(cosσ*(-1+2*cos2σₘ*cos2σₘ)-B/6*cos2σₘ*(-3+4*sinσ*sinσ)*(-3+4*cos2σₘ*cos2σₘ)));
#   σʹ = σ;
#   σ = s / (b*A) + Δσ;
#} while (Math.abs(σ-σʹ) > 1e-12 && ++iterations<100); // TV: 'iterate until negligible change in λ' (≈0.006mm)
#  if (iterations >= 100) throw new EvalError('Vincenty formula failed to converge'); // not possible?
#
#    const x = sinU1*sinσ - cosU1*cosσ*cosα1;
#  const φ2 = Math.atan2(sinU1*cosσ + cosU1*sinσ*cosα1, (1-f)*Math.sqrt(sinα*sinα + x*x));
#  const λ = Math.atan2(sinσ*sinα1, cosU1*cosσ - sinU1*sinσ*cosα1);
#  const C = f/16*cosSqα*(4+f*(4-3*cosSqα));
#  const L = λ - (1-C) * f * sinα * (σ + C*sinσ*(cos2σₘ+C*cosσ*(-1+2*cos2σₘ*cos2σₘ)));
#  const λ2 = λ1 + L;
#
#  const α2 = Math.atan2(sinα, -x);
#
#  const destinationPoint = new LatLonEllipsoidal_Vincenty(φ2.toDegrees(), λ2.toDegrees(), 0, this.datum);
#
#  return {
#    point:        destinationPoint,
#    finalBearing: Dms.wrap360(α2.toDegrees()),
#    iterations:   iterations,
#  };
