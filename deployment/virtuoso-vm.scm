;;; Copyright © 2018  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file contains a minimal operating system configuration to run
;; virtuoso.
;;
;; A virtual machine image can be generated from this using:
;; $ cp `guix system vm-image virtuoso-vm.scm --image-size=50G` node.qcow2
;;
;; The machine can be run using:
;; $ qemu-system-x86_64 -m 4096 -smp 2 -enable-kvm \
;;     -net user,hostfwd=tcp::2201-:22, -net nic,model=virtio \
;;     -drive file=node.qcow2
;;
;; Have fun!
;;

(use-modules (gnu))
(use-service-modules base networking ssh)
(use-package-modules screen ssh ncurses databases)

(operating-system
  (host-name "virtuoso")
  (timezone "Europe/Amsterdam")
  (locale "en_US.utf8")

  ;; Assuming /dev/vda is the target hard disk, and "root" is
  ;; the label of the target root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/vda")))
  (file-systems (cons (file-system
                        (device "root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                (name "lang")     ; Lang Lang is a different kind of virtuoso. :-)
                (password "lang") ; Change this for a little better security.
                (group "users")
                (home-directory "/home/lang"))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (cons* openssh virtuoso-ose %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (cons* (dhcp-client-service)
                   (rngd-service)
                   (service openssh-service-type
                            (openssh-configuration
                              (port-number 22)))
                   %base-services)))
