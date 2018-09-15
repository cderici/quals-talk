#lang slideshow

(require (for-syntax syntax/parse)
         (rename-in slideshow (slide orig-slide))
         slideshow/code
         slideshow/base
         pict
         racket/draw)

(define slide-count 4) ; 4 outline minus the title

;; A pict to use behind the main content
(define fade-bg
  (let ([w (+ (* 2 margin) client-w)]
        [h (+ (* 2 margin) client-h)]
        [trans (make-object brush% "white" 'transparent)]
        [inside (make-object brush% "white" 'solid)])
    (inset (dc (lambda (dc x y)
                 (let ([b (send dc get-brush)]
                       [p (send dc get-pen)]
                       [draw-one
                        (lambda (i)
                          (send dc draw-rectangle
                                (+ x i) (+ y i)
                                (- w (* 2 i)) (- h (* 2 i))))])
                   (send dc set-brush trans)
                   (color-series 
                    dc margin 1
                    (make-object color% "black")
                    (make-object color% "white")
                    draw-one
                    #t #t)
                   (send dc set-brush inside)
                   (draw-one margin)
                   (send dc set-pen p)
                   (send dc set-brush b)))
               w h 0 0)
           (- margin))))

(current-slide-assembler
 (lambda (s v-sep c)
   (lt-superimpose
    (lbl-superimpose
     fade-bg
     (hc-append (scale (bitmap (build-path "images" "plt-logo.png")) 0.3)
                (scale (bitmap (build-path "images" "isabelle.png")) 0.3)
                (scale (bitmap (build-path "images" "pycket.png")) 0.3)))
    (let ([c (colorize c "darkred")])
      (if s
          (vc-append v-sep 
                     ;; left-aligns the title:
                     (ghost (scale (titlet s) 2))
                     (titlet s)
                     c)
          c))
      )))

(define-syntax slide
  (syntax-parser
    [(_ arg ...)
     #'(begin
         (set! slide-count (add1 slide-count))
         (orig-slide arg ...))]))

(current-page-number-adjust
 (λ (str n)
   (format "~a / ~a" n slide-count)))

(define outline
  (let ([sub-para
         (lambda l
           (para #:width (* 3/4 (current-para-width)) l))])
    (make-outline
     'one "Part I: Modeling Linklets Using PLT Redex"
     #f

     'two "Part II: Adding Pairs to the Denotational Model"
     (lambda (tag)
       (sub-para "... of the CBV λ-calculus"))

     'three "Part III: Measuring Pycket's Performance"
     (lambda (tag)
       (sub-para "... with the new linklets"))

     'end "Conclusion"
     #f)))

(set-page-numbers-visible! #f)

 (slide
  #:title "Qualifying Examination Response"
  (scale (t "Caner Derici") 0.8)
  (scale (t "19 Sep 2018") 0.5)
  (blank)
  (scale (it "Advisor:") 0.5)
  (scale (it "Sam Tobin-Hochstadt") 0.5)
  )

#;(start-at-recent-slide)

#;(set-page-numbers-visible! #t)

(outline 'one)



 (slide
  #:title "Linklets"
  (t "linklets")
  )

(outline 'two)

(outline 'three)

(outline 'end)

;Adding Pairs To The Denotational Model Of The Untyped CBV Lambda Calculus

;Measuring The Performance Of The Linklet System On Pycket