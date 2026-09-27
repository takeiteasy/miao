(defpackage #:miao/cli
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:m #:meow)
                    (#:ui #:miao/ui)
                    (#:bt #:bordeaux-threads-2))
  (:export #:main #:exit-code))
