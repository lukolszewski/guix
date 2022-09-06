;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Lukasz Olszewski <dev@lukaszolszewski.info>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.


(define-module (gnu packages libldm)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gtk)
  #:use-module (guix git-download))

(define-public libldm
  (package
    (name "libldm")
    (version "0.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdbooth/libldm.git")
                    (commit (string-append "libldm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08iz3kq4ci79abpyxwwqmzi3bayyk4s29n8h1jqgdgk5yskwgnrn"))))
    (build-system gnu-build-system)
    (inputs (list json-glib
                  glib
                  zlib
                  readline
                  lvm2
                  libgudev))
    (native-inputs (list which
                         m4
                         libtool
                         autoconf-wrapper
                         automake
                         pkg-config
                         `(,glib "bin")
                         gtk-doc
                         libxml2
                         libxslt
                         docbook-xsl))
    (arguments
     '(#:tests? #f
       #:parallel-build? #t
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-env
                    (lambda _
                      (setenv "CONFIG_SHELL"
                              (which "")) #t))
                  (add-before 'bootstrap 'run-gtkdocize
                    (lambda _
                      (invoke "gtkdocize")))
                  (replace 'bootstrap
                    (lambda _
                      (invoke "autoreconf" "-fiv"))))))
    (home-page "https://github.com/mdbooth/libldm")
    (synopsis "Manager for Microsoft Windows dynamic disks")
    (description
     "Libldm is a library for managing Microsoft Windows dynamic disks, which use Microsoft's LDM metadata.  It can inspect them, and also create and remove device-mapper block devices which can be mounted.")
    (license license:gpl3)))
