#lang slideshow

(require (for-syntax syntax/parse)
         (rename-in slideshow (slide orig-slide)))

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