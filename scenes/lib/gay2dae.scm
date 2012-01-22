; Introduction
; ------------
; A method of converting raygay scenes into Collada 3D models.
; This is done by shadowing all the object-creating functions from
; normal raygay-scenes with new functions that instead of creating
; objects capture the input and use it for the creation of Collada-XML.
; This .dae file can be output at the end of scene creation.
;
; The 1.4 spec
; http://www.khronos.org/files/collada%5Fspec%5F1%5F4.pdf
; A newer 1.5 spec exists, but the 1.4 is probably enough.
;
; Options
; ------- 
; The output function can take a hash of options such as:
;
; sphere-longitude-subsections
; sphere-latitude-subsections
; cylinder-subsections
; torus-longitude-subsections (what are the "axis" on a torus called?)
; torus-latitude-subsections
;
; All options have sane defaults.
;
; Caveats
; -------
; CSG operations will probably be very hard to implement. 
