@(egg "animation")
@(description "Utility for creating animations from a series of images")
@(author "Peter Danenberg")
@(username "klutometis")
@(email "pcd@roxygen.org")

(define (make-animator #!key
                       (magnitude 10000)
                       (frames-per-second 4)
                       (type "png")
                       (width 1600)
                       (height 900))
  @("Create an animator, which consists of two values: a frame-creator and a finalizer."
    "{{Next-frame}} is a niladic function which returns the filename
of the next frame; {{finalize}} is a monadic function taking the
name of the resultant animation (e.g. {{graph.avi}})."
    (magnitude "Roughly the number of animations one anticipates")
    (frames-per-second "Frames per second")
    (type "The frame type; one of e.g. \"png\", \"jpg\"")
    (width "The width of the frame in pixels (#f for no scaling)")
    (height "The height of the frame (#f for no scaling)")
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
      (let ((options
             (option-string (append
                             `((type . ,type)
                               (fps . ,frames-per-second))
                             (if width `((w . ,width)) '())
                             (if height `((h . ,height)) '())))))
        (run (mencoder ,(format "mf://~a"
                                (make-pathname directory
                                               (format "*.~a" type)))
                       -mf ,options
                       -ovc lavc
                       -o ,animation))))
    (values next-frame finalize)))

(define (option-string options)
  (string-join
   (map (match-lambda ((key . value) (format "~a=~a" key value))) options)
   ":"))
