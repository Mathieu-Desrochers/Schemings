(import srfi-13)

(import (chicken file))
(import (chicken file posix))
(import (chicken pathname))
(import (chicken process))
(import (chicken process-context))

(declare (unit latex))

(declare (uses exceptions))
(declare (uses file*))

;; escapes the latex characters in a string
(: latex-escape (string -> string))
(define (latex-escape string)
  (letrec (
    (latex-escape-inner
      (lambda (string accumulator)
        (if (> (string-length string) 0)
          (latex-escape-inner
            (string-drop string 1)
            (string-append
              accumulator
              (cond ((equal? (string-take string 1) "&") "\\&")
                    ((equal? (string-take string 1) "#") "\\#")
                    ((equal? (string-take string 1) "$") "\\$")
                    ((equal? (string-take string 1) "%") "\\%")
                    ((equal? (string-take string 1) "\\") "\\textbackslash{}")
                    ((equal? (string-take string 1) "^") "\\textasciicircum{}")
                    ((equal? (string-take string 1) "_") "\\_")
                    ((equal? (string-take string 1) "{") "\\{")
                    ((equal? (string-take string 1) "}") "\\}")
                    ((equal? (string-take string 1) "~") "\\textasciitilde{}")
                    (else (string-take string 1)))))
          accumulator))))
    (if string
      (latex-escape-inner string "")
      "")))

;; formats a number to a latex string with fractions
(: latex-number->string-with-fractions (number -> string))
(define (latex-number->string-with-fractions number)
  (let* ((full-units (inexact->exact (floor number)))
         (partial-units (- number full-units)))
    (string-append
      (number->string full-units)
      (cond ((>= partial-units 0.75) " \\nicefrac{3}{4}")
            ((>= partial-units 0.5) " \\nicefrac{1}{2}")
            ((>= partial-units 0.25) " \\nicefrac{1}{4}")
            (else "")))))

;; returns latex headers that produce a nice layout
(: latex-headers (-> string))
(define (latex-headers)
  (string-append
    "\\documentclass{article}\n"
    "\\usepackage{array,multirow}\n"
    "\\usepackage{fancyhdr}\n"
    "\\usepackage[letterpaper,portrait,margin=0.4in]{geometry}\n"
    "\\usepackage{graphicx}\n"
    "\\usepackage{grfext}\n"
    "\\usepackage[utf8]{inputenc}\n"
    "\\usepackage{ltablex}\n"
    "\\usepackage{nicefrac}\n"
    "\\usepackage{textpos}\n"
    "\\usepackage{tikz}\n"
    "\\usepackage[breakwords]{truncate}\n"
    "\\usepackage{utopia}\n"
    "\\keepXColumns\n"
    "\\pagestyle{fancyplain}\n"
    "\\renewcommand{\\arraystretch}{1.4}\n"
    "\\renewcommand{\\headrulewidth}{0in}\n"
    "\\setlength{\\headheight}{0.9in}\n"
    "\\setlength{\\headsep}{0.2in}\n"
    "\\setlength{\\LTleft}{0in}\n"
    "\\setlength{\\parindent}{0in}\n"
    "\\setlength{\\textheight}{9.2in}\n"
    "\\PrependGraphicsExtensions{}\n"
    "\\DeclareGraphicsRule{*}{jpg}{*}{}\n"))

;; returns latex headers that produce a nice legal layout
(: latex-headers-legal (-> string))
(define (latex-headers-legal)
  (string-append
    "\\documentclass{article}\n"
    "\\usepackage{array}\n"
    "\\usepackage{fancyhdr}\n"
    "\\usepackage[legalpaper,portrait,margin=0.4in]{geometry}\n"
    "\\usepackage[utf8]{inputenc}\n"
    "\\usepackage{ltablex}\n"
    "\\usepackage{nicefrac}\n"
    "\\usepackage[breakwords]{truncate}\n"
    "\\usepackage{utopia}\n"
    "\\keepXColumns\n"
    "\\pagestyle{fancyplain}\n"
    "\\renewcommand{\\arraystretch}{1.4}\n"
    "\\renewcommand{\\headrulewidth}{0in}\n"
    "\\setlength{\\headheight}{0.9in}\n"
    "\\setlength{\\headsep}{0.2in}\n"
    "\\setlength{\\LTleft}{0in}\n"
    "\\setlength{\\parindent}{0in}\n"
    "\\setlength{\\textheight}{12.2in}\n"))

;; returns latex headers that produce
;; an absolute positioning friendly layout
(: latex-headers-absolute-positioning (-> string))
(define (latex-headers-absolute-positioning)
  (string-append
    "\\documentclass{article}\n"
    "\\usepackage[letterpaper,portrait,margin=0in]{geometry}\n"
    "\\usepackage{graphicx}\n"
    "\\usepackage{grfext}\n"
    "\\usepackage[utf8]{inputenc}\n"
    "\\usepackage{textpos}\n"
    "\\usepackage[breakwords]{truncate}\n"
    "\\usepackage{utopia}\n"
    "\\setlength{\\parindent}{0in}\n"
    "\\setlength{\\topskip}{0pt}\n"
    "\\setlength{\\TPHorizModule}{1in}\n"
    "\\setlength{\\TPVertModule}{1in}\n"
    "\\PrependGraphicsExtensions{}\n"
    "\\DeclareGraphicsRule{*}{jpg}{*}{}\n"))

;; generates a pdf file from a latex source
(: latex-generate-pdf (string string -> noreturn))
(define (latex-generate-pdf latex-source output-file-name)
  (let ((input-file (file*-unique-name "/tmp/latex-"))
        (current-working-directory (current-directory)))
    (with-guaranteed-release
      (lambda ()
        (file-open input-file (+ open/wronly open/creat)))
      (lambda (input-file-descriptor)
        (file-write input-file-descriptor latex-source)
        (change-directory "/usr/local/bin/")
        (let-values ((
            (pid success code)
            (process-wait
              (process-run "/usr/local/bin/pdflatex"
                (list
                  "-interaction=batchmode"
                  "-output-format=pdf"
                  (string-append "-output-directory=" (pathname-directory input-file))
                  input-file)))))
          (change-directory current-working-directory)
          (move-file (string-append input-file ".pdf") output-file-name #t)))
      (lambda (input-file-descriptor)
        (file-close input-file-descriptor)))))
