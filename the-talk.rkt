#lang slideshow

(require (for-syntax syntax/parse)
         (rename-in slideshow (slide orig-slide))
         slideshow/code)

(define slide-count 0)

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

(slide
 #:title "Qualifying Examination Response"
 (t "Caner Derici"))

(outline 'one)

(outline 'two)

(outline 'three)

(outline 'end)

;Adding Pairs To The Denotational Model Of The Untyped CBV Lambda Calculus

;Measuring The Performance Of The Linklet System On Pycket