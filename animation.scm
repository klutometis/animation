(module animation
  (make-animator)
  (import scheme
          chicken
          srfi-13)
  (use files
       format
       matchable
       miscmacros
       posix
       shell)
  (include "animation-core.scm"))
