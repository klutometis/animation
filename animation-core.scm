@(egg "animation")
@(description "Utility for creating animations from a series of images")
@(author "Peter Danenberg")
@(username "klutometis")
@(email "pcd@roxygen.org")

(define (make-animator #!key
                       (magnitude 10000)
                       (frames-per-second 4)
                       (type "png"))

  @("Create an animator, which consists of two values: a frame-creator and a finalizer."
    "{{Next-frame}} is a niladic function which returns the filename
of the next frame; {{finalize}} is a monadic function taking the
basename of the animation (e.g. {{graph}} → {{graph.avi}})."
    (magnitude "Roughly the number of animations one anticipates")
    (frames-per-second "Frames per second")
    (type "The frame type; one of e.g. \"png\", \"jpg\"")
    (@to "Two values: next-frame and finalize")
    (@example-no-eval "In this hypothetical example, we're running a
depth-first-search on a graph; outputting an animation frame every step."
                      (receive (next-frame finalize)
                        (make-animator)
                        (let ((graph (make-random-graph)))
                          (call-for-each-frame (depth-first-search graph)
                                               (lambda (graph)
                                                 (write-graph-as-png graph (next-frame))))
                          (finalize "graph")))))
  (let ((directory (create-temporary-directory))
        (current-frame 0)
        (digits (inexact->exact (ceiling (/ (log magnitude) (log 10))))))
    (define (next-frame)
      (let ((frame (make-pathname
                    directory
                    (format (format "~~~a,48d" digits) current-frame)
                    type)))
        (inc! current-frame)
        frame))
    (define (finalize animation)
      (run (cd ,directory &&
               ;; More apropos to use a URL-builder.
               mencoder ,(format "mf://~a"
                                 (make-pathname directory
                                                (format "*.~a" type)))
               -mf ,(format "type=~a:fps=~a" type frames-per-second)
               -ovc lavc
               -o ,(if (absolute-pathname? animation)
                       animation
                       (make-pathname (current-directory) animation ".avi")))))
    (values next-frame finalize)))
