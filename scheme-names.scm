(module scheme-names mzscheme
  (provide *scm-builtins* *scm-keywords*)

  (define *scm-builtins*
    '("abs"
      "acos"
      "angle"
      "append"
      "apply"
      "asin"
      "assq" "assv" "assoc"
      "atan"
      "boolean?"
      "car" "cdr"
      "caar"   "cadr"   "cdar"   "cddr"
      "caaar"  "caadr"  "cadar"  "caddr"  "cdaar"  "cdadr"  "cddar"  "cdddr"
      "caaaar" "caaadr" "caadar" "caaddr" "cadaar" "cadadr" "caddar" "cadddr"
      "cdaaar" "cdaadr" "cdadar" "cdaddr" "cddaar" "cddadr" "cdddar" "cddddr"
      "call-with-current-continuation" "call/cc"
      "call-with-input-file"
      "call-with-output-file"
      "call-with-values"
      "char?"
      "char=?"    "char<?"    "char>?"    "char<=?"    "char>=?"
      "char-ci=?" "char-ci<?" "char-ci>?" "char-ci<=?" "char-ci>=?"
      "char-alphabetic?" "char-numeric?" "char-whitespace?" "char-upper-case?" "char-lower-case?"
      "char-ready?"
      "char->integer"
      "char-upcase" "char-downcase"
      "cons"
      "ceiling"
      "close-input=port"
      "close-output-port"
      "complex?"
      "cos"
      "current-input-port"
      "current-output-port"
      "denominator"
      "display"
      "dynamic-wind"
      "eof-object?"
      "eq?"
      "equal?"
      "eqv?"
      "eval"
      "even?"
      "exact?"
      "exact->inexact"
      "exp"
      "expt"
      "floor"
      "for-each"
      "force"
      "gcd"
      "imag-part"
      "inexact?"
      "inexact->exact"
      "input-port?"
      "integer?"
      "integer->char"
      "interaction-environment"
      "lcm"
      "length"
      "list"
      "list?"
      "list-ref"
      "list-tail"
      "list->string"
      "list->vector"
      "load"
      "log"
      "magnitude"
      "make-polar"
      "make-rectangular"
      "make-string"
      "make-vector"
      "map"
      "max" "min"
      "memq" "memv" "member"
      "modulo"
      "negative?"
      "newline"
      "not"
      "null?"
      "null-environment"
      "number?"
      "number->string"
      "numerator"
      "odd?"
      "open-input-file"
      "open-output-file"
      "output-port?"
      "pair?"
      "peek-char"
      "positive?"
      "procedure?"
      "quotient"
      "rational?"
      "rationalize"
      "read"
      "read-char"
      "real?"
      "real-part"
      "remainder"
      "reverse"
      "round"
      "scheme-report-environment"
      "sin"
      "set-car!"
      "set-cdr!"
      "sqrt"
      "string"
      "string?"
      "string-append"
      "string-copy"
      "string-fill!"
      "string-length"
      "string-ref"
      "string-set!"
      "string->list"
      "string->number"
      "string->symbol"
      "string=?"
      "string-ci=?"
      "string<?" "string>?" "string<=?" "string>=?"
      "string-ci<?" "string-ci>?" "string-ci<=?" "string-ci>=?"
      "substring"
      "symbol?"
      "symbol->string"
      "tan"
      "transcript-on"
      "transcript-off"
      "truncate"
      "values"
      "vector"
      "vector?"
      "vector-fill!"
      "vector-length"
      "vector-ref"
      "vector-set!"
      "vector->list"
      "with-input-from-file"
      "with-output-to-file"
      "write"
      "write-char"
      "zero?"
      "=" "<" ">" "<=" ">="
      "+" "*" "-" "/"
      ))

  (define *scm-keywords*
    '("=>"
      "and"
      "begin"
      "begin0"
      "case"
      "cond"
      "define"
      "define-macro"
      "define-syntax"
      "define-struct"
      "delay"
      "do"
      "else"
      "fluid-let"
      "if"
      "lambda"
      "let"
      "let-syntax"
      "let*"
      "letrec"
      "letrec-syntax"
      "module"
      "or"
      "provide"
      "quasiquote"
      "quote"
      "require"
      "require-for-syntax"
      "set!"
      "syntax-case"
      "syntax-rules"
      "unless"
      "unquote"
      "unquote-splicing"
      "when"
      "with-handlers")))
