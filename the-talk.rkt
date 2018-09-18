#lang slideshow

(require (for-syntax syntax/parse)
         (rename-in slideshow (slide orig-slide))
         slideshow/code
         slideshow/base
         pict
         pict/face
         racket/draw)

(set-spotlight-style! #:size 20 #:color (make-object color% 255 0 0 0.4))

(define slide-count 4) ; 4 outline minus the title

;; A pict to use behind the main content
(define fade-bg
  (let ([w (+ (* 2 margin) client-w)]
        [h (+ (* 2 margin) client-h)]
        [trans (make-object brush% "white" 'transparent)]
        [inside (make-object brush% (make-object color% 153 187 204) 'solid)])
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
 (comment "Girizgah")
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
 (comment "Buralar beyle bostan idi")
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
  (scale (bitmap (build-path "images" "rc-grammar.png")) 0.95)
  (para #:align 'center "Racket Core Model Grammar")
  )

(slide
  #:title "Racket Core (RC) Model"
  (scale (bitmap (build-path "images" "rc-red-relation.png")) 0.6)
  (para #:align 'center "Racket Core Model Standard Reduction Relation")
  )

(slide
  #:title "Racket Core (RC) Model"
  (scale (bitmap (build-path "images" "rc-run-rc.png")) 1)
  (para #:align 'center "Racket Core Model Evaluation Function")
  )

(slide
  #:title "Linklet Model"
  (scale (bitmap (build-path "images" "linklet-model-grammar.png")) 0.65)
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

(slide
 #:title "Instantiating a Linklet"
 (scale (bitmap (build-path "images" "examples-inst-1.png")) 1.5)
 (para #:align 'center "Instantiating a linklet without a target creates a new instance"))

(slide
 #:title "Instantiating a Linklet"
 (scale (bitmap (build-path "images" "examples-inst-2.png")) 1.5)
 (para #:align 'center "Getting imported variables (as values)"))

(slide
 #:title "Instantiating a Linklet"
 (scale (bitmap (build-path "images" "examples-inst-3.png")) 1.5)
 (para #:align 'center "Start evaluating the body using the Racket Core Model")
 (para #:align 'center "A new variable is created in the target."))

(slide
 #:title "Instantiating a Linklet"
 (scale (bitmap (build-path "images" "examples-inst-4.png")) 1.5)
 (para #:align 'center "A Linklet can modify variables (of the target too)"))

(slide
 #:title "Instantiating a Linklet"
 (scale (bitmap (build-path "images" "examples-inst-5.png")) 1.5)
 (para #:align 'center "The resulting instance is returned")
 (para #:align 'center "The result of" (code (+ c c)) "is ignored"))

(slide
 #:title "Running a Linklet"
 (para #:align 'center "= Instantiating a linklet with a given target instance")
 (scale (bitmap (build-path "images" "examples-target-1.png")) 1.5)
 (para #:align 'center "Target may have variables with the same names"))

(slide
 #:title "Running a Linklet"
 (scale (bitmap (build-path "images" "examples-target-2.png")) 1.5)
 (para #:align 'center "Getting the imported variables"))

(slide
 #:title "Running a Linklet"
 (scale (bitmap (build-path "images" "examples-target-3.png")) 1.5)
 (para #:align 'center "An exported variable replaces target's variable"))

(slide
 #:title "Running a Linklet"
 (scale (bitmap (build-path "images" "examples-target-4.png")) 1.5)
 (para #:align 'center "Mutation happens (wherever the variable is)"))

(slide
 #:title "Running a Linklet"
 (scale (bitmap (build-path "images" "examples-target-5.png")) 1)
 (para #:align 'center "Linklet may use target's variable (e.g. " (code x) ")")
 (para #:align 'center "Instead of the instance, the result of" (code (+ c x)) "is returned"))

(slide
 #:title "Testing the Models"
 'alts
 (list
  (list
   (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Racket Core Model")))
   (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Linklet Model"))))
  (list
   (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Racket Core Model")))
   (subitem (para "Random testing : " (code eval-rc=racket-core)))
   (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Linklet Model"))))
  (list
   (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Racket Core Model")))
   (subitem (para "Random testing : " (code eval-rc=racket-core)))
   (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Linklet Model")))
   (subitem (para (t "Using Pycket's unit tests for linklets")))
   (subitem (para "Random testing : " (code eval-prog=racket-linklets))))
   ))

(slide
 #:title "Testing the Models : Restricting the Grammars"
 'alts
 (list (list (scale (bitmap (build-path "images" "restricted-rc-grammar.png")) 1)
             (para #:align 'center "Restricting RC grammar by ruling out closures"))
       (list (scale (bitmap (build-path "images" "restricted-linklet-grammar.png")) 1)
             (para #:align 'center "Restricting linklet grammar by ruling out closures")
             (para #:align 'center "Also the explicit terms for linklet instances"))))

(slide
 #:title "Testing the Models : The \"program\" Form"
 (comment "We needed a form to basically start the computation")
 (code
  (test-equal
   (term
    (eval-prog
     (program (use-linklets
               [l1 (linklet () ())]
               [l2 (linklet ((b)) () (define-values (a) 5) (+ a b))]
              [l3 (linklet () (b) (define-values (b) 3))])
              (let-inst t3 (instantiate l3))
              (let-inst t1 (instantiate l1))
              (instantiate l2 t3 #:target t1)))) 8))
 (para #:align 'center "The " (code (program ...)) " form in action.")
 (comment "Therefore, we created the form program, that will define some linklets in the
use-linklets part, and have a begin-like body that models a top-level context where we
can define some instances using the let-inst form and “run” some linklets (i.e. instan-
tiate with targets). Note that the body of a program may also have other Racket Core
expressions like “(+ 1 2)”"))

(slide
 #:title "Testing the Models : To Actual Racket"
 (code
  (term
   (to-actual-racket
    (program (use-linklets
              [l1 (linklet () ())]
              [l2 (linklet ((b)) () (define-values (a) 5) (+ a b))]
              [l3 (linklet () (b) (define-values (b) 3))])
             (let-inst t3 (instantiate l3))
             (let-inst t1 (instantiate l1))
             (instantiate l2 t3 #:target t1)))))
 (para #:align 'center "Transforming the \"program\" into a Racket program.")
 (comment "We couldn’t use the Racket Core forms
like let-values to basically define some linklets and instantiate them in the body, be-
cause then we would need to make a linklet an expression in the Racket Core language,
which is wrong not only from the perspective of the Racket Core, but also from the
Linklets’ perspective, since it would also mean that a linklet could have another linklet
in its body.")
 (code
  (let ((l1 (compile-linklet '(linklet () ())))
        (l2 (compile-linklet '(linklet ((b)) () (define-values (a) 5) (+ a b))))
        (l3 (compile-linklet '(linklet () (b) (define-values (b) 3)))))
    (define t3 (instantiate-linklet l3 (list)))
    (define t1 (instantiate-linklet l1 (list)))
    (instantiate-linklet l2 (list t3) t1))))

(slide
 #:title "Testing the Models : Perils of Random Testing - Corner Cases"
 (scale (bitmap (build-path "images" "eval-prog.png")) 0.7)
 (para #:align 'center "Metafunctions on alert to report corner case errors"))

(slide
 #:title "Testing the Models : Success!"
 (item #:bullet (colorize (tt ">") "darkred")
         (para (t "Racket Core Model")))
 (subitem (para "Tested " (code eval-rc=racket-core) "with 1000 randomly generated terms"))
 (item #:bullet (colorize (tt ">") "darkred")
       (para (t "Linklet Model")))
 (subitem (para (t "Tested using Pycket's unit tests for linklets (~110 cases)")))
 (subitem (para "Tested " (code eval-prog=racket-linklets) "with 2000 randomly generated terms"))
 (scale (bitmap (build-path "images" "linklet-references.png")) 0.7))

(current-slide-assembler
 (lambda (s v-sep c)
   (lt-superimpose
    fade-bg
    (let ([c (colorize c "darkred")])
      (if s
          (vc-append v-sep
                     ;; left-aligns the title:
                     #;(ghost (scale (titlet s) 2))
                     (inset (titlet s) 20)
                     c)
          c))
    )))

;Adding Pairs To The Denotational Model Of The Untyped CBV Lambda Calculus

(outline 'two)

(define isabelle-default-assembler
  (lambda (s v-sep c)
    (lt-superimpose
     (lbl-superimpose
      fade-bg
      (scale (bitmap (build-path "images" "isabelle.png")) 0.3))
     (let ([c (colorize c "darkred")])
       (if s
           (vc-append v-sep
                     ;; left-aligns the title:
                      #;(ghost (scale (titlet s) 2))
                      (inset (titlet s) 20)
                      c)
           c))
     )))

(current-slide-assembler isabelle-default-assembler)

(slide
 #:title "Adding Pair to the Denotational Model of CBV λ-calculus"
 'alts
 (list (list
        (para #:width 1000
              (it "Extend Jeremy’s denotational model of the untyped lambda calculus with pairs.")
              (it "Extend the proof of correspondence with the operational semantics in Isabelle")
              (it "to handle pairs."))
        (scale (bitmap (build-path "images" "isabelle" "jeremy-paper.png")) 0.7))
       (list
        (scale (bitmap (build-path "images" "isabelle" "jeremy-archive.png")) 0.7))
       (list
        (scale (bitmap (build-path "images" "isabelle" "jeremy-fig4.png")) 1.4))))

(define (isabelle-assembler-file file)
  (lambda (s v-sep c)
    (lt-superimpose
     (rb-superimpose
      (lbl-superimpose
       fade-bg
       (scale (bitmap (build-path "images" "isabelle.png")) 0.3))
      (inset
       (cc-superimpose
       (filled-rounded-rectangle (+ (pict-width (it file)) 20)
                                 (+ (pict-height (it file)) 20)
                                 #:draw-border? #t
                                 #:color "Burlywood"
                                 #:border-color "Bisque"
                                 #:border-width 5)
       (it file)) 10))
      (let ([c (colorize c "darkred")])
        (if s
            (vc-append v-sep
                     ;; left-aligns the title:
                       #;(ghost (scale (titlet s) 2))
                       (inset (titlet s) 20)
                       c)
            c))
      )))

(current-slide-assembler (isabelle-assembler-file "Lambda.thy"))

(slide
 #:title "Extending the grammar"
 (scale (bitmap (build-path "images" "isabelle" "lc-grammar.png")) 1.2))

(current-slide-assembler isabelle-default-assembler)

(slide
 #:title "Extending the Operational Semantics"
 (item #:bullet (colorize (tt ">") "darkred")
       "The soundness is proved using the big-step semantics")
 (item #:bullet (colorize (tt ">") "darkred")
       "The completeness is proved using the small-step semantics")
 (blank)
 'next
 (item #:bullet (colorize (tt ">") "darkred")
       "In Isabelle proofs the big-step semantics depend on the small-step semantics"))

(current-slide-assembler (isabelle-assembler-file "SmallStepLam.thy"))

(slide
 #:title "Extending the Small-Step Operational Semantics"
 (scale (bitmap (build-path "images" "isabelle" "isval.png")) 1.5)
 (para #:align 'center "A pair is a value when it's elements are values.")
 (scale (bitmap (build-path "images" "isabelle" "small-step-red-rel.png")) 1.5)
 (para #:align 'center "Extending the standard reduction relation with pairs."))

(current-slide-assembler (isabelle-assembler-file "BigStepLam.thy"))

(slide
 #:title "Extending the Big-Step Operational Semantics"
 (scale (bitmap (build-path "images" "isabelle" "big-step-vals.png")) 0.8)
 (para #:align 'center "Extending the big-step values.")
 (scale (bitmap (build-path "images" "isabelle" "big-step-red-rel.png")) 0.8)
 (para #:align 'center "Extending the big-step reduction relation with pairs."))

(current-slide-assembler (isabelle-assembler-file "DeclSemAsDenotFSet.thy"))

(slide
 #:title "Extending the Denotational Semantics"
 (scale (bitmap (build-path "images" "isabelle" "new-fig4.png")) 0.5)
 (comment "However, following in the footsteps of the prior elementary semantics, we accommodate
self application by allowing the argument d2 to be a larger approximation than
the input entry d1"))

(slide
 #:title "Does this semantics form a filter model?"
 (comment "One of the questions that Jeremy's paper asks is this.")
 (para (t "A ") (tt "filter") (t " is a set that is upward closed and closed under finite intersection."))
 'next
 (para (t "But ⊑ goes the opposite way -> the question becomes,"))
 (scale (bitmap (build-path "images" "isabelle" "ordering-rel.png")) 0.5)
 (para (t "Is this an ") (it "ideal?") (t " (the dual of filter)")))

(current-slide-assembler isabelle-default-assembler)

(slide
 #:title "Does the denotational semantics form an ideal?"
 'alts
 (list
  (list 
   (item  #:bullet (colorize (tt ">") "darkred")
               (t "Downward closed (i.e. Subsumption)"))
   (item  #:bullet (colorize (tt ">") "darkred")
          (t "Closed under finite union")))
  (list 
   (item  #:bullet (colorize (tt ">") "darkred")
          (t "Downward closed (i.e. Subsumption)")
          (subitem (scale (bitmap (build-path "images" "isabelle" "subsumption.png")) 0.5)))
   (item  #:bullet (colorize (tt ">") "darkred")
          (t "Closed under finite union")))
  (list 
   (item  #:bullet (colorize (tt ">") "darkred")
          (t "Downward closed (i.e. Subsumption)")
          (subitem (scale (bitmap (build-path "images" "isabelle" "subsumption.png")) 0.5)))
   (item  #:bullet (colorize (tt ">") "darkred")
          (t "Closed under finite union")
          (subitem (scale (bitmap (build-path "images" "isabelle" "join-is-the-least-upper.png")) 0.5))
          (subitem (scale (bitmap (build-path "images" "isabelle" "closed-under-join-for-values.png")) 0.5))))))
  

(current-slide-assembler (isabelle-assembler-file "DeclSemAsDenotFSet.thy"))

(outline 'three)

(outline 'end)

;Measuring The Performance Of The Linklet System On Pycket



(current-slide-assembler isabelle-default-assembler)

(slide
 #:title "Trivial changes in Isabelle theories are omitted because..."
 (para (t "well.. because they're trivial."))
 (scale (bitmap (build-path "images" "aux" "isabelle-trivial.png")) 1))

(slide
 #:title "le_union1 proof - the case of pair"
 (scale (bitmap (build-path "images" "aux" "le_union1.png")) 1))
