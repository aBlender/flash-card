#lang racket

;if using #lang flash-card
(module reader syntax/module-reader
  flash-card/lang)

;if you (require flash-card)
(provide (all-from-out "./lang.rkt"))
(require "./lang.rkt")
