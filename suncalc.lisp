(defpackage :suncalc
  (:use #:common-lisp)
  (:import-from :local-time
                #:encode-universal-time
                #:now
                #:timestamp-to-universal))

(in-package :suncalc)

#|
The javascript implementation defined these:
var PI   = Math.PI,
    sin  = Math.sin,
    cos  = Math.cos,
    tan  = Math.tan,
    asin = Math.asin,
    atan = Math.atan2,
    acos = Math.acos,
but since they are available in the CLOS standard i have not.
|#

(defconstant +rad+ (/ pi 180))

; sun calculations are based on http://aa.quae.nl/en/reken/zonpositie.html formulas
; date/time constants and conversions

(defconstant +day-ms+ (* (* (* 1000 60) 60) 24))
(defconstant +j1970+ 2440588)
(defconstant +j2000+ 2451545)

; Helper functions. JavaScript uses unix epoch while Lisp does whatever it wants
; so these are to standardize the dates

(defconstant +suncalc-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun to-julian (date)
  `(/ (- (local-time:timestamp-to-universal ,date) +suncalc-epoch+)
    (- +day-ms+ (+ 0.5 +j1970+))))

(defun from-julian (j)
  `(local-time:timestamp-to-universal (* (- (+ ,j 0.5) +j1970+) +day-ms+)))

(defun to-days (date)
  `(- (to-julian ,date) +j2000+))

; general calculations for position
(defconstant +e+ (* +rad+ 23.4397))

(defun right-ascension (l b)
  (atan (- (* (sin l) (cos +e+))
    (* (tan b) (sin +e+))) (cos l)))

(defun declination (l b)
  (asin (+ (* (sin b) (cos +e+))
    (* (cos b) (sin +e+) (sin l)))))

(defun azimuth (h phi dec)
  (atan (sin h) (- (* (cos h)
    (sin phi)) (* (tan dec) (cos phi)))))

(defun altitude (h phi dec)
  (asin (+ (* (sin phi) (sin dec))
    (* (cos phi) (cos dec) (cos h)))))

(defun sidereal-time (d lw)
  (- (* +rad+ (+ 280.16 (* 360.9856235 d))) lw))

(defun astro-refraction (h)
  (if (< h 0) ; the following formula works for positive altitudes only.
    (setq h 0) ; if h = -0.08901179 a div/0 would occur.
      ; formula 16.4 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.
      ; 1.02 / tan(h + 10.26 / (h + 5.10)) h in degrees, result in arc minutes -> converted to rad:
  (/ 0.002967 (tan (/ (+ h 0.00312536) (+ h 0.08901179))))))

(defun solar-mean-anomaly (d)
  (* +rad+ (+ 357.5291 (* 0.98560028 d))))

(defun ecliptic-longitude (m)
  (let* ((c (* +rad+ (+ (* 1.9148 (sin m)) (* 0.02 (sin (* 2 m))) (* 0.0003 (sin (* 3 m))))))
        (p (* +rad+ 102.9372)))
        (+ (+ m c) (+ p pi))))

(defstruct sun-coordinates
  ra
  dec)

(defun sun-coords (d)
  (let* ((m (solar-mean-anomaly d))
        (l (ecliptic-longitude m)))
        (make-sun-coordinates :ra (right-ascension l 0)
                              :dec (declination l 0))))

(defvar *sun-calc* (make-hash-table))

(defstruct sun-position
  azi
  alt)

(defun get-position (date lat lng)
  `(let* ((lw (* +rad+ (* -1 ,lng)))
        (phi (* +rad+ ,lat))
        (d (to-days ,date))

        (c (sun-coords d))
        (h (- (sidereal-time d lw) (coordinates-right-ascension c)))
        (make-sun-position :azi (azimuth h phi (dec c))
                           :alt (altitude h phi (dec c))))))

; sun times configuration (angle, morning name, evening name)
(defvar *times*
  '((-0.833 "sunrise" "sunset")
    (-0.3 "sunrise-end" "sunset-start")
    (-6 "dawn" "dusk")
    (-12 "nautical-dawn" "nautical-dusk")
    (-18 "night-end" "night")
    (6 "golden-hour-end" "golden-hour")))

; adds a custom time to the times config
(defun add-time (angle rise-name set-name)
  (push `(,angle ,rise-name ,set-name) *times*))

; calculations for sun times
(defconstant +j0+ 0.0009)

(defun julian-cycle (d lw)
  (round (/ (- d +j0+ lw) (* 2 pi))))

(defun approx-transit (ht lw n)
  (/ (+ +j0+ (+ ht lw)) (+ (* 2 pi) n)))

(defun solar-transit-j (ds m l)
  (+ +j2000+ ds (* 0.0053 (sin m) (* -0.0069 (sin (* 2 L))))))

(defun hour-angle (h phi d)
  (acos (/ (- (sin h) (* (sin phi) (sin d))) (* (cos phi) (cos d)))))

(defun observer-angle (height)
  (/ (* -2.076 (sqrt height)) 60))

(defun get-set-j (h lw phi dec n m l)
  (let* ((w (hour-angle h phi dec))
         (a (approx-transit w lw n)))
         (return-from get-set-j (solar-transit-j a m l))))

; calculates sun times for a given date, latitude/longitude, and, optionally,
; the observer height (in meters) relative to the horizon

(defun get-times (date lat lng &optional (height 0))
  `(let* ((lw (* +rad+ (* -1 ,lng))))
          (phi (* rad lat))

          (dh (observer-angle ,height))

          (d (to-days ,date))
          (n (julian-cycle ,d ,lw))
          (ds (approx-transit 0 ,lw n))

          (m (solar-mean-anomaly ds))
          (l (ecliptic-longitude m))
          (dec (declination l 0))

          (jnoon (solar-transit-j ds m l))

;;;;;;;;;;FINISH HERE!!!;;;;;;;;;;
          ))

; moon calculations, based on http://aa.quae.nl/en/reken/hemelpositie.html formulas
(defstruct moon-coordinates
  ra
  dec
  dist)

(defun moon-coords (d)
  `(let* ((l (* +rad+ (+ 218.316 (* 13.176396 ,d)))) ; ecliptic longitude
          (m (* +rad+ (+ 134.963 (* 13.064993 ,d)))) ; mean anomaly
          (f (* +rad+ (+ 93.272  (* 13.229350 ,d))))

          (l (+ l (* +rad+ 6.289 (sin m))))
          (b (* +rad+ 5.127 (sin f)))
          (dt (- 385001 (* 20905 (cos m))))
          (make-moon-coordinates :ra (right-ascension l b)
                                 :dec (declination l b)
                                 :dist dt))))

(defstruct moon-position
  azi
  alt
  dist
  par-ang)

(defun get-moon-position (date lat lng)
  `(let* ((lw (* +rad+ (* -1 ,lng)))
          (phi (* +rad+ ,lat))
          (d (to-days ,date))

          (c (moon-coords d))
          (h (- (side-realtime d lw) (dec c)))

          (pa (atan ((sin h) (- (* (tan phi) (cos (dec c))) (* (sin (dec c)) (cos h))))))

          (rh (+ astro-refraction h)))

          (make-moon-position :azi (azimuth h phi (dec c))
                              :alt h
                              :dist (dist c)
                              :par-ang pa)))

; calculations for illumination parameters of the moon,
; based on http://idlastro.gsfc.nasa.gov/ftp/pro/astro/mphase.pro formulas and
; Chapter 48 of "Astronomical Algorithms" 2nd edition by Jean Meeus (Willmann-Bell, Richmond) 1998.

(defstruct moon-illumination
  fraction
  phase
  angle)

(defun get-moon-illumination (&optional (date (local-time:now)))
  `(let* ((d (to-days ,date)))
          (s (sun-coords d))
          (m (moon-coords d))

          (sdist 149598000)

          (phi (acos (+ (* (sin (dec s)) (sin (dec m)))
            (* (cos (dec s)) (cos (dec m)) (cos (- (ra s) (ra m)))))))
          (inc (atan (* sdist (sin phi)) (- (distance m) (* sdist (cos phi)))))
          (angle (atan (* (cos (dec s)) (sin (- (ra s) (ra m))))
            (- (* (sin (dec s) (dec m))) (* (cos (dec s)) (sin (dec m) (cos (- (ra s) (ra m))))))))

          (make-moon-illumination :fraction (/ (+ 1 (cos inc)) 2)
                                  :phase (+ 0.5 (/ (* 0.5 inc ((if (< angle 0) -1 1))) pi))
                                  :angle angle)))


(defun hours-later (date h)
  `(local-time:timestamp-to-universal (+ ,date (/ (* ,h +day-ms+) 24))))

(defun get-moon-times (date lat lng in-utc)

)
