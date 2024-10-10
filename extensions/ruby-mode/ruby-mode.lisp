(defpackage :lem-ruby-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools)
  (:import-from :lem-js-mode
                :get-line-indent
                :move-to-previous-line)
  (:export :ruby-mode))

(in-package :lem-ruby-mode)

(defvar *ruby-keywords*
  '("alias" "break"
    "defined\?" "fail"
    "end" "next" 
    "redo" "retry"
    "return" "self" "super"
    "undef" "yield" "private"
    "public" "protected"))

(defvar *ruby-block-beg-keywords*
  '("class" "module" "def" "case" "for" "begin" "do"))

(defvar *ruby-modifier-beg-keywords*
  '("if" "unless" "while" "until"))

(defvar *ruby-block-mid-keywords*
  '("then" "else" "elsif" "when" "in" "rescue" "ensure"))

(defvar *ruby-block-op-keywords*
  '("and" "or" "not"))

(defvar *ruby-block-start*
  "(^|\\s)(def|class|module|loop|for|begin|do)\\b")

;; I Need a regex for keywords that could be at the end of a line, such as
;; while|if|unless|until|case|when
(defvar *ruby-block-end*
  "(^|\\s)(end)\\b")

(defvar *ruby-boolean-literals*
  '("true" "false"))

(defvar *ruby-null-literal*
  '("nil"))

;; From:  https://www.rubyguides.com/2018/07/ruby-operators/
(defvar *ruby-operators*
  '(
    "+" "*" "++" "--" "<>" "||" "&&" "!"
    "==" "!=" "===" "!==" ">=" "<=" "<=>"
   "?" "~" "<<" ">>" "%" "|"
    ))


(defun tokens (boundary strings)
  (let ((alternation
          `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))


(defun make-tmlanguage-ruby ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match ":\\w+" 
                                   :name 'syntax-constant-attribute) 
                    (make-tm-match "\\w+:"
                                   :name 'syntax-constant-attribute)
                    (make-tm-line-comment-region "#")
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-string-region "\"\"\"")
                    (make-tm-match (tokens :word-boundary 
                                           (append *ruby-boolean-literals*
                                                   *ruby-null-literal*))
                                   :name 'syntax-constant-attribute)
                    (make-tm-match (tokens :word-boundary (append
                                                           *ruby-keywords*
                                                           *ruby-block-op-keywords*
                                                           *ruby-block-mid-keywords*
                                                           *ruby-modifier-beg-keywords*
                                                           *ruby-block-beg-keywords*))
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (tokens :word-boundary *ruby-operators*)  
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *ruby-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\')
                :block-string-pairs '(("\"\"\"" . "\"\"\"")
                                      ("'''" . "'''"))
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-ruby)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode ruby-mode language-mode
    (:name "Ruby"
     :syntax-table *ruby-syntax-table*
     :mode-hook *ruby-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
		(variable-value 'indent-tabs-mode) nil 
		(variable-value 'calc-indent-function) 'ruby-calc-indent
		(variable-value 'tab-width) 2
		(variable-value 'line-comment) "#"
		(variable-value 'beginning-of-defun-function) 'beginning-of-defun
		(variable-value 'end-of-defun-function) 'end-of-defun))

(defun beginning-of-defun (point n)
  (loop :with regex = *ruby-block-start*
        :repeat n 
        :do (search-backward-regexp point regex)))


(defun end-of-defun (point n)
  (with-point ((p point))
    (loop :repeat n
          :do (line-offset p 1)
              (unless (search-forward-regexp p "^  end") (return)))
    (line-start p)
    (move-point point p)))


;; https://rubystyle.guide/ to determine indentation (this could be changed)
;; IDEA--- Instead of getting the current indent, i should check the previous indentation
;; and decide wether or not i should increase it or decrease it

(defun ruby-calc-indent (point)
  (with-point ((prev point))
    (move-to-previous-line prev)
    ;; What happens when we are in the first line
    (with-point ((p point))
      (if (start-buffer-p (line-start p))
          (return-from ruby-calc-indent 0)))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (length (get-line-indent prev))))
      ;; when in a comment
    (when (in-string-or-comment-p point)
      (with-point ((p point))
        (back-to-indentation p)
        (return-from ruby-calc-indent (point-column p))))

    ;; What happens if the previous line has a start of block keyword
    (with-point ((p point))
      (when (move-to-previous-line p)
        (with-point ((start p)
                     (end p))
          (line-start start)
          (line-end end)
          (when (search-backward-regexp end *ruby-block-start* start)
            (return-from ruby-calc-indent (incf column tab-width)) 
            ))))
    
      ;; what happens if the current line has an end keyword
      (with-point ((p point)
                 (end point))
      (if (search-forward-regexp p "\\b(end)" (line-end end))
          (decf column tab-width)
          )
      )
    column))
  )

(define-file-type ("rb" "rspec" "Gemfile") ruby-mode)
