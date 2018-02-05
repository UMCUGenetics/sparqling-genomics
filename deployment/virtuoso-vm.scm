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
(use-service-modules base networking ssh shepherd)
(use-package-modules screen ssh ncurses databases linux)

(define start-firewall
  #~(let ((iptables
           (lambda (str) (zero? (apply system*
                           #$(file-append iptables "/sbin/iptables")
                           (string-tokenize str))))))
      (format #t "Installing iptables SSH rules...~%")
      (and (iptables "-P INPUT ACCEPT")
           (iptables "-F")
           (iptables "-A INPUT -i lo -j ACCEPT")
           (iptables "-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT")
           (iptables "-A INPUT -m state --state NEW -m tcp -p tcp --dport 8890 -j ACCEPT")
           (iptables "-A INPUT -m state --state NEW -m tcp -p tcp --dport 1111 -j ACCEPT")
           (iptables "-A INPUT -m state --state NEW -m tcp -p tcp --dport 22 -j ACCEPT")
           (iptables "-P INPUT DROP")
           (iptables "-P FORWARD DROP")
           (iptables "-P OUTPUT ACCEPT"))))

(define %firewall-service
  ;; The "firewall".  Make it a Shepherd service because as an activation
  ;; script it might run too early, before the Netfilter modules can be
  ;; loaded for some reason.
  (simple-service 'firewall shepherd-root-service-type
                  (list (shepherd-service
                         (provision '(firewall))
                         (requirement '())
                         (start #~(lambda () #$start-firewall))
                         (respawn? #f)))))

(define start-virtuoso
  #~(let ((virtuoso
           (lambda (str) (zero? (apply system*
                           #$(file-append virtuoso-ose "/bin/virtuoso-t")
                           (string-tokenize str))))))
      (format #t "Starting Virtuoso...~%")
      (with-output-to-file "/etc/virtuoso.ini"
        (lambda _
          (format #t "
[Database]
DatabaseFile			= /home/lang/virtuoso.db
ErrorLogFile			= /home/lang/virtuoso.log
LockFile			= /home/lang/virtuoso.lck
TransactionFile			= /home/lang/virtuoso.trx
xa_persistent_file		= /home/lang/virtuoso.pxa
ErrorLogLevel			= 7
FileExtend			= 200
MaxCheckpointRemap		= 2000
Striping			= 0
TempStorage			= TempDatabase

[TempDatabase]
DatabaseFile			= /home/lang/virtuoso-temp.db
TransactionFile			= /home/lang/virtuoso-temp.trx
MaxCheckpointRemap		= 2000
Striping			= 0

[Parameters]
ServerPort			= 1111
LiteMode			= 0
DisableUnixSocket		= 1
DisableTcpSocket		= 0
MaxClientConnections		= 10
CheckpointInterval		= 60
O_DIRECT			= 0
CaseMode			= 2
MaxStaticCursorRows		= 5000
CheckpointAuditTrail		= 0
AllowOSCalls			= 0
SchedulerInterval		= 10
DirsAllowed			= ., ~a/share/virtuoso/vad
ThreadCleanupInterval		= 0
ThreadThreshold			= 10
ResourcesCleanupInterval	= 0
FreeTextBatchSize		= 100000
SingleCPU			= 0
VADInstallDir			= ~a/share/virtuoso/vad/
PrefixResultNames               = 0
RdfFreeTextRulesSize		= 100
IndexTreeMaps			= 256
MaxMemPoolSize                  = 200000000
PrefixResultNames               = 0
MacSpotlight                    = 0
IndexTreeMaps                   = 64
MaxQueryMem 		 	= 2G
VectorSize 		 	= 1000
MaxVectorSize 		 	= 1000000
AdjustVectorSize 	 	= 0
ThreadsPerQuery 	 	= 4
AsyncQueueMaxThreads 	 	= 10
NumberOfBuffers          = 340000
MaxDirtyBuffers          = 250000

[HTTPServer]
ServerPort			= 8890
ServerRoot			= /home/lang
MaxClientConnections		= 10
DavRoot				= DAV
EnabledDavVSP			= 0
HTTPProxyEnabled		= 0
TempASPXDir			= 0
DefaultMailServer		= localhost:25
ServerThreads			= 10
MaxKeepAlives			= 10
KeepAliveTimeout		= 10
MaxCachedProxyConnections	= 10
ProxyConnectionCacheTimeout	= 15
HTTPThreadSize			= 280000
HttpPrintWarningsInOutput	= 0
Charset				= UTF-8
HTTPLogFile		        = logs/http.log
MaintenancePage             	= atomic.html
EnabledGzipContent          	= 1

[AutoRepair]
BadParentLinks			= 0

[Client]
SQL_PREFETCH_ROWS		= 100
SQL_PREFETCH_BYTES		= 16000
SQL_QUERY_TIMEOUT		= 0
SQL_TXN_TIMEOUT			= 0

[VDB]
ArrayOptimization		= 0
NumArrayParameters		= 10
VDBDisconnectTimeout		= 1000
KeepConnectionOnFixedThread	= 0

[Replication]
ServerName			= db-localhost
ServerEnable			= 1
QueueMax			= 50000

[Striping]
Segment1			= 100M, db-seg1-1.db, db-seg1-2.db
Segment2			= 100M, db-seg2-1.db

[Zero Config]
ServerName			= virtuoso (localhost)

[Mono]

[URIQA]
DynamicLocal			= 0
DefaultHost			= localhost:8890

[SPARQL]
ResultSetMaxRows           	= 10000
MaxQueryCostEstimationTime 	= 900000000000
MaxQueryExecutionTime      	= 3600
DefaultQuery               	= select distinct ?Concept where {[] a ?Concept} LIMIT 100
DeferInferenceRulesInit    	= 0

[Plugins]
" #$virtuoso-ose #$virtuoso-ose)))
      (virtuoso "+configfile /etc/virtuoso.ini")))

(define %virtuoso-service
  ;; The "firewall".  Make it a Shepherd service because as an activation
  ;; script it might run too early, before the Netfilter modules can be
  ;; loaded for some reason.
  (simple-service 'virtuoso shepherd-root-service-type
                  (list (shepherd-service
                         (provision '(virtuoso))
                         (requirement '())
                         (start #~(lambda () #$start-virtuoso))
                         (respawn? #f)))))

(operating-system
  (host-name "virtuoso")
  (timezone "Europe/Amsterdam")
  (locale "en_US.utf8")

  ;; Assuming /dev/vda is the target hard disk, and "root" is
  ;; the label of the target root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/vda")
                (timeout 0)))
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
                   (service openssh-service-type
                            (openssh-configuration
                             (port-number 22)))
                   %firewall-service
                   %virtuoso-service
                   %base-services)))
