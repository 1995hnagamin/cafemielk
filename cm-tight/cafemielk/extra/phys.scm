;;;
;;; cafemielk.extra.phys
;;; Physical constants
;;;

(define-module cafemielk.extra.phys
  (export
   avogadro-const-mol^-1
   electr-const-F/m
   elem-charge-C
   light-speed-m/s
   magn-const-N/A^2
   planck-const-J.s
   )
  )

(select-module cafemielk.extra.phys)

;; Speed of light in vacuum
;; c = 299792458 m s^{-1}
(define light-speed-m/s
  299_792_458)

;; Elementary charge
;; e = 1.602176634 \times 10^{-19} C
(define elem-charge-C
  1.602_176_634e-19)

;; Planck constant
;; h = 6.62607015 \times 10^{-34} J s
(define planck-const-J.s
  6.626_070_15e-34)

;; Avogadro constant
;; N_A = 6.02214076 \times 10^{23} mol^{-1}
(define avogadro-const-mol^-1
  6.022_140_76e23)

;; Vacuum magnetic permeability (magnetic constant)
;; \mu_0 = 1.25663706127(20) \times 10^{-6} N.A^{-2}
(define magn-const-N/A^2
  1.256_637_061_27e-6)

;; Vacuum permittivity (electric constant)
;; \epsilon_0 = 1/(c^2 \mu_0)
;;            = 8.8541878188(14) \times 10^{-12} F m^{-1}
(define electr-const-F/m
  (/. 1
      (expt light-speed-m/s 2)
      magn-const-N/A^2))
