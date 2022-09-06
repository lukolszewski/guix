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


(define-module (gnu services libldm)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (guix modules)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages libldm)
  #:export (libldm-configuration libldm-configuration? libldm-service-type))

(define-record-type* <libldm-configuration>
                     libldm-configuration
                     make-libldm-configuration
                     libldm-configuration?
                     (package
                       libldm-configuration-package
                       (default libldm))
                     (action libldm-configuration-action
                             (default '("create" "all"))))

(define (libldm-shepherd-service config)
  "Return a <shepherd-service> for libldm with CONFIG"
  (let* ((libldm (libldm-configuration-package config))
         (action (libldm-configuration-action config)))
    (list (shepherd-service (documentation
                             "Run ldmtool to create Windows dynamic disc device nodes at startup.")
                            (provision '(libldmd))
                            (one-shot? #t)
                            (start #~(make-forkexec-constructor (append (list (string-append #$libldm
                                                                               "/bin/ldmtool"))
                                                                        '(#$@action))))
                            (stop #~(make-kill-destructor))))))

(define libldm-service-type
  (service-type (name 'libldm)
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   libldm-shepherd-service)))
                (default-value (libldm-configuration))
                (description
                 "Run ldmtool to create device nodes for Windows dynamic discs so they can be mounted")))
