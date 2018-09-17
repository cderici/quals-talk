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
        [inside (make-object brush% (make-object color% 120 153 169) 'solid)])
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
                    (make-object color% (make-object color% 120 153 169))
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
     (hc-append gap-size
                (scale (bitmap (build-path "images" "plt-logo.png")) 0.3)
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
 #:title "Qualifying Examination"
 (scale (t "Caner Derici") 0.8)
 (scale (t "19 Sep 2018") 0.5)
 (blank)
 (scale (it "Advisor:") 0.5)
 (scale (it "Sam Tobin-Hochstadt") 0.5)
 )

#;(start-at-recent-slide)

#;(set-page-numbers-visible! #t)

(current-font-size 25)

(current-slide-assembler
 (lambda (s v-sep c)
   (lt-superimpose
    fade-bg
    (let ([c (colorize c "darkred")])
      (if s
          (vc-append v-sep 
                     ;; left-aligns the title:
                     (ghost (scale (titlet s) 2))
                     (titlet s)
                     c)
          c))
      )))

(slide
 #:title "Questions"
 (table 2 ; two columns
        (list
         (item #:bullet (colorize (tt ">") "darkred")
               (para #:width 1000
                     (it "Model the basics of the new Racket \"linklet\" module system using PLT Redex.")
                     (it "Use random testing to confirm that the model produces the same answers as")
                     (it "both the existing implementation and the implementation that you have produced")
                     (it "using the Pycket JIT.")))
         (scale (bitmap (build-path "images" "plt-logo.png")) 0.3)
         (item #:bullet (colorize (tt ">") "darkred")
               (para #:width 1000
                     (it "Extend Jeremy’s denotational model of the untyped lambda calculus with pairs.")
                     (it "Extend the proof of correspondence with the operational semantics in Isabelle")
                     (it "to handle pairs.")))
         (scale (bitmap (build-path "images" "isabelle.png")) 0.3)
         (item #:bullet (colorize (tt ">") "darkred")
               (para #:width 1000
                     (it "Measure the performance of your changes to the Pycket JIT on the existing")
                     (it "benchmarks. For at least 3 performance differences, propose a hypothesis")
                     (it "that explains the difference.")))
         (scale (bitmap (build-path "images" "pycket.png")) 0.3))
         (list* lc-superimpose  ; left-align first column
                cc-superimpose) ; h-center the rest
         cc-superimpose ; v-center all rows
         gap-size  ; separate all columns by gap-size
         gap-size)) ; separate all rows by gap-size

(outline 'one)

(current-slide-assembler
 (lambda (s v-sep c)
   (lt-superimpose
    (lbl-superimpose
     fade-bg
     (scale (bitmap (build-path "images" "plt-logo.png")) 0.3))
    (let ([c (colorize c "darkred")])
      (if s
          (vc-append v-sep
                     ;; left-aligns the title:
                     #;(ghost (scale (titlet s) 2))
                     (inset (titlet s) 20)
                     c)
          c))
      )))


 (slide
  #:title "What is a linklet?"
  (scale (bitmap (build-path "images" "linklet-grammar.png")) 1.2)
  )

 (slide
  #:title "Motivation for linklets"
   (item #:bullet (colorize (tt ">") "darkred")
         (t "Implementing Racket in Racket"))
   'next
   (item #:bullet (colorize (tt ">") "darkred")
         (t "Racket on Chez"))
  )

 (slide
  #:title "Racket on Chez"
  (scale (bitmap (build-path "images" "racket6core.png")) 0.7)
  )

(slide
  #:title "Racket on Chez"
  (hc-append (* gap-size 2)
    (scale (bitmap (build-path "images" "racket6core.png")) 0.7)    
    (scale (bitmap (build-path "images" "racket7core.png")) 0.7))
  )

(slide
  #:title "Racket on Chez"
  (hc-append (* gap-size 2)
    (scale (bitmap (build-path "images" "racket6core.png")) 0.7)    
    (scale (bitmap (build-path "images" "racket7core.png")) 0.7)
    (scale (bitmap (build-path "images" "chez.png")) 0.7))
  )
   
(slide
  #:title "Racket on Chez"
  (hc-append (* gap-size 2)
    (scale (bitmap (build-path "images" "racket6core.png")) 0.7)    
    (scale (bitmap (build-path "images" "racket7core.png")) 0.7)
    (scale (bitmap (build-path "images" "chez.png")) 0.7)
    (scale (bitmap (build-path "images" "racket-on-chez.png")) 0.7))
  )

(slide
  #:title "Linklets and Instantiation"
  (scale (bitmap (build-path "images" "linklet-grammar.png")) 1)
  (item #:bullet (colorize (tt ">") "darkred")
        (t "Linklets contain Racket Core expressions."))
  (item #:bullet (colorize (tt ">") "darkred")
        (t "A linklet instance is a collection of variable definitions."))
  'alts
  (list (list (scale (bitmap (build-path "images" "linklet-example.png")) 0.8))
        (list (scale (bitmap (build-path "images" "inst-example.png")) 0.8)
              (para #:align 'center
                    (t "Instantiating the linklet will result in a ")
                    (tt "linklet instance")))))

(slide
  #:title "Evaluating a Linklet (i.e. Targeted Instantiation)"
  (scale (bitmap (build-path "images" "eval-example.png")) 1.2)
  (t "If a target is provided, then an instantiation produces a value."))

(slide
  #:title "Building the PLT Redex Model for Linklets"
  (item #:bullet (colorize (tt ">") "darkred")
         (t "Racket Core Model"))
  (item #:bullet (colorize (tt ">") "darkred")
         (t "Linklet Model"))
  )

(slide
  #:title "Racket Core (RC) Model"
  (scale (bitmap (build-path "images" "rc-grammar.png")) 0.8)
  (para #:align 'center "Racket Core Model Grammar")
  )

(slide
  #:title "Racket Core (RC) Model"
  (scale (bitmap (build-path "images" "rc-red-relation.png")) 0.6)
  (para #:align 'center "Racket Core Model Standard Reduction Relation")
  )

(slide
  #:title "Racket Core (RC) Model"
  (scale (bitmap (build-path "images" "rc-run-rc.png")) 0.8)
  (para #:align 'center "Racket Core Model Evaluation Function")
  )

(slide
  #:title "Linklet Model"
  (scale (bitmap (build-path "images" "linklet-model-grammar.png")) 0.6)
  (para #:align 'center "Linklet Model Grammar")
  )

(slide
  #:title "Linklet Model"
  'alts
  (list (list 
         (scale (bitmap (build-path "images" "linklet-red-relation.png")) 0.6)
         (para #:align 'center "Linklet Model Standard Reduction Relation"))
        (list
         (scale (bitmap (build-path "images" "linklet-red-relation-focus.png")) 0.6)
         (para #:align 'center "Linklet Model Standard Reduction Relation"))))


(outline 'two)

(outline 'three)

(outline 'end)

;Adding Pairs To The Denotational Model Of The Untyped CBV Lambda Calculus

;Measuring The Performance Of The Linklet System On Pycket