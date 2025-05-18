(define-module (guile-wayland packages guile-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public guile-bytestructure-class
  (let ((commit "3d126a8fef81aa1422742d92478d94bf1b575c51")
        (revision "0"))
    (package
      (name "guile-bytestructure-class")
      (version (git-version "0.2.0" revision commit))
      (source (origin (method git-fetch)
                      (uri (git-reference
                            (url
                             "https://github.com/Z572/guile-bytestructure-class")
                            (commit commit)))
                      (file-name (git-file-name name version))
                      (sha256
                       (base32
                        "195ldnix62aghahljadbji3q4palc8illcmkggdwwq5vkbkzd0p3"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags #~'("GUILE_AUTO_COMPILE=0")))
      (native-inputs
       (list autoconf
             automake
             pkg-config
             guile-3.0-latest))
      (inputs (list guile-3.0-latest))
      (propagated-inputs (list guile-bytestructures))
      (synopsis "")
      (description "")
      (home-page "")
      (license license:gpl3+))))

(define-public guile-util572
  (let ((commit "e97642f6f9cca770451b6701260f7a3dab4a210b")
        (revision "2"))
    (package
      (name "guile-util572")
      (version (git-version "0" revision commit))
      (source (origin (method git-fetch)
                      (uri (git-reference
                            (url
                             "https://github.com/Z572/util572")
                            (commit commit)))
                      (file-name (git-file-name name version))
                      (sha256
                       (base32
                        "1jb3wvzmdqkdkk1y9qvashhh1zk26dhhgdshfawq29slz6nv6df5"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        #~(list "GUILE_AUTO_COMPILE=0")))
      (native-inputs
       (list guile-3.0
             pkg-config
             autoconf
             automake
             texinfo))
      (inputs
       (list guile-3.0))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:gpl3+))))
