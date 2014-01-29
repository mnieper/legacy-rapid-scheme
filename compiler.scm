(import
  (scheme base)
  (scheme write)
  (rapid load)
  (rapid expand)
  (rapid import-set)
  (rapid compile)
  (rapid library))

; TODO: The compilation is completely missing  
(display
  (expand (load)))
(newline)

; Wie funktioniert die expand-Routine:
;
; Parameter:
;   environment: aktuelles lexikographisches environment
;   export-sets: bibliotheken
;   imports: was gerade importiert worden ist ins environment (zur Fehlerfindung)
;   code: was an expandiertem Code herauskommt, der dann zu kompilieren ist (cps-transform, optimize).
;
;   sowas wie import muß auch expandiert werden
;
; aufbau:
;
; (expand source ...) -> code (eine Liste von Ausdrücken)
; wenn er define-library liest: rufe expand-library auf
; diese hat eigenes environment; was, wenn das Ende der Bibliothek gekommen ist?
; dann expandiere weiter den Rest...
; alles tail-recursive... 
;

; TODO: What do we have to put in the loop below?
; Can we abbreviate it? Or use parameter objects?
; Write the thing on paper first

#|
#;(let ((library-table (make-library-table)))
  (let loop ((sources (load)))
    (let ((source (car sources)) (sources (cdr sources)))
      (cond
        ((null? sources)
          (compile-program library-table source))
        (else
          (compile-library library-table source)
          (loop sources))))))
|#
   
; 
; Load libraries in the order they are used

;(with-new-environment
;  (lambda ()
;    (compile (load))))

