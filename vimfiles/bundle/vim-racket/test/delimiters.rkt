#lang racket
(define good-highlight1 '(a b #\space))
(define good-highlight2 '(a b #\tab))
(define bad-highlight1 '(a b #[))
(define bad-highlight2 '(a b $[))
(define stuff-here-should-not-be-quoted '())
