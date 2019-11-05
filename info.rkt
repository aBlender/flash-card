#lang info

(define scribblings '(("scribblings/manual.scrbl" (multi-page))))

(define deps '(
  "https://github.com/thoughtstem/meta-engine.git"
  "threading"
  "image-coloring"
  ))

(define compile-omit-paths '(
  "examples.rkt"))

