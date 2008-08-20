
; See http://en.wikipedia.org/wiki/Aspect_ratio_(image)
;
; The aspect ratios below are listed in ascending wideness.

; 1.33:1 aka 4:3 formats
; Used in old TVs.
(define image-size-dvd-4:3 '(768 576))
(define image-size-vga '(640 480))

; 1.37:1 formats
; Between the 1920s and the early 1950s, all films were photographed 
; with a 1.37:1 aspect ratio. This is why you won't see older films 
; like The Wizard of Oz or Casablanca in widescreen. They were not 
; filmed that way. 

; 1.5:1 aka 3:2 formats
; Common still-photography aspect. 

; 1.5:1 aka 15:10 formats
(define image-size-ntsc-sd '(720 480))

; 1.6:1 aka 16:10 formats
; Used in many widescreen monitors
(define image-size-30-inch-apple-cinema '(2560 1600))
(define image-size-23-inch-apple-cinema '(1920 1200))
(define image-size-23-inch-dell-monitor '(1920 1200))    ; Min 23" Dell monitor ude på UNI-C
(define image-size-24-inch-intel-imac   '(1920 1200))
(define image-size-20-inch-apple-cinema '(1680 1050))
(define image-size-20-inch-intel-imac   '(1680 1050))
(define image-size-17-inch-macbook-pro  '(1680 1050))     ; 17" Macbook Pro
(define image-size-17-inch-g4-powerbook '(1440 900))     ; 17" G4 Powerbook (Toves)
(define image-size-15-inch-macbook-pro  '(1440 900))     ; 15" Macbook Pro
(define image-size-13-inch-macbook      '(1280 800))     ; 13.3" Macbook
(define image-size-13-inch-macbook-air  '(1280 800))     ; 13.3" Macbook Air

; 1.78:1 aka 16:9 formats
; Used in new widescreen TVs
(define image-size-1080-hd '(1920 1080)) 
(define image-size-720-hd  '(1280 720))
(define image-size-360-hd  '(640 360))
(define image-size-180-hd  '(320 180))

; 1.85:1 formats
; A popular aspect for many movies 
(define image-size-1.85 '(1280 688))

; 2.35:1 formats
; Another popular aspect for many movies

  
; For printing on paper.
; Photos are normally printed at 240dpi and posters at 300dpi.

;; Converts dpi to dots-per-millimeter. Using 1inch = 2.54mm
(define (dpi->dpmm dpi) (/ dpi 25.4))

(define (mm&dpi->pixels mm dpi)
  (inexact->exact (round (* mm (dpi->dpmm dpi)))))

; A4 is 297mm x 210mm in landscape.
(define a4-landscape@300dpi (list (mm&dpi->pixels 297 300) (mm&dpi->pixels 210 300))) ; -> (3508 2480)
(define a4-landscape@240dpi (list (mm&dpi->pixels 297 240) (mm&dpi->pixels 210 240))) ; -> (2806 1984)

; A3 is 420mm x 297mm in landscape

; A5 is 210mm x 148mm in landscape


