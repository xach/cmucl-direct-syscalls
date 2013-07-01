;;; -*- Mode: Lisp; Package: Unix -*-
;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher based on public domain
;;; code from Carnegie Mellon University and has been placed in the
;;; Public domain, and is provided 'as is'.
;;;
(file-comment
  "$Header: /home/dtc/cvs/src/cmucl/p86/code/internet-syscall.lisp,v 1.3 1999/09/13 07:28:34 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Re-write of the internet support unix calls to use the direct
;;; system calls available on the x86 port which safely handling the
;;; errno.

(in-package "EXT")

(defun create-unix-socket (&optional (kind :stream))
  (multiple-value-bind (proto type)
      (internet-protocol kind)
    (declare (ignore proto))
    (multiple-value-bind (socket errno)
	(unix::%unix-socket af-unix type 0)
      (if (minusp socket)
	  (error "Error creating socket: ~A" (unix:get-unix-error-msg errno))
	  socket))))

(defun connect-to-unix-socket (path &optional (kind :stream))
  (declare (simple-string path))
  (let ((socket (create-unix-socket kind)))
    (with-alien ((sockaddr unix-sockaddr))
      (setf (slot sockaddr 'family) af-unix)
      (kernel:copy-to-system-area path
				  (* vm:vector-data-offset vm:word-bits)
				  (alien-sap (slot sockaddr 'path))
				  0
				  (* (1+ (length path)) vm:byte-bits))
      (multiple-value-bind (result errno)
	  (unix::%unix-connect socket (alien-sap sockaddr)
			       (alien-size unix-sockaddr :bytes))
	(when (minusp result)
	  (unix:unix-close socket)
	  (error "Error connecting socket to [~A]: ~A"
		 path (unix:get-unix-error-msg errno))))
      socket)))

(defun create-inet-socket (&optional (kind :stream))
  (multiple-value-bind (proto type)
      (internet-protocol kind)
    (multiple-value-bind (socket errno)
	(unix::%unix-socket af-inet type proto)
      (if (minusp socket)
	  (error "Error creating socket: ~A" (unix:get-unix-error-msg errno))
	  socket))))

(defun connect-to-inet-socket (host port &optional (kind :stream))
  "The host may be an address string or an IP address in host order."
  (let ((socket (create-inet-socket kind))
	(hostent (or (lookup-host-entry host)
		     (error "Unknown host: ~S." host))))
    (with-alien ((sockaddr inet-sockaddr))
      (setf (slot sockaddr 'family) af-inet)
      (setf (slot sockaddr 'port) (htons port))
      (setf (slot sockaddr 'addr) (htonl (host-entry-addr hostent)))
      (multiple-value-bind (result errno)
	  (unix::%unix-connect socket (alien-sap sockaddr)
			       (alien-size inet-sockaddr :bytes))
	(when (minusp result)
	  (unix:unix-close socket)
	  (error "Error connecting socket to [~A:~A]: ~A"
		 (host-entry-name hostent) port
		 (unix:get-unix-error-msg errno))))
      socket)))

(defconstant so-reuseaddr #+linux 2 #+(or solaris bsd hpux irix) 4)

(defun get-socket-option (socket level optname)
  "Get an integer value socket option."
  (declare (type unix:unix-fd socket)
	   (type (signed-byte 32) level optname))
  (with-alien ((optval signed))
    (if (minusp (unix:unix-getsockopt socket level optname
				      (alien-sap (addr optval)) 4))
	(values nil unix:unix-errno)
	(values optval 0))))

(defun set-socket-option (socket level optname optval)
  "Set an integer value socket option."
  (declare (type unix:unix-fd socket)
	   (type (signed-byte 32) level optname optval))
  (with-alien ((optval signed optval))
    (if (minusp (unix:unix-setsockopt socket level optname
				      (alien-sap (addr optval)) 4))
	(values nil unix:unix-errno)
	(values optval 0))))

(defun create-inet-listener (port &optional (kind :stream)
                                  &key reuse-address
                                  (backlog 5))
  (let ((socket (create-inet-socket kind)))
    (with-alien ((sockaddr inet-sockaddr))
      (setf (slot sockaddr 'family) af-inet)
      (setf (slot sockaddr 'port) (htons port))
      (setf (slot sockaddr 'addr) 0)

      (when reuse-address
        (multiple-value-bind (optval errno)
            (set-socket-option socket sol-socket so-reuseaddr 1)
          (or optval (error "Error ~s setting socket option on socket ~d."
                            (unix:get-unix-error-msg errno) socket))))
      
      (multiple-value-bind (result errno)
	  (unix::%unix-bind socket (alien-sap sockaddr)
			    (alien-size inet-sockaddr :bytes))
	(when (minusp result)
	  (unix:unix-close socket)
	  (error "Error binding socket to port ~a: ~a"
		 port (unix:get-unix-error-msg errno)))))
    (when (eq kind :stream)
      (multiple-value-bind (result errno)
	  (unix::%unix-listen socket backlog)
	(when (minusp result)
	  (unix:unix-close socket)
	  (error "Error listening to socket: ~A"
		 (unix:get-unix-error-msg errno)))))
    socket))

(defun accept-tcp-connection (unconnected)
  (declare (type unix:unix-fd unconnected))
  (with-alien ((sockaddr inet-sockaddr)
	       (length (alien:array unsigned 1)))
    (setf (deref length 0) (alien-size inet-sockaddr :bytes))
    (multiple-value-bind (connected errno)
	(unix::%unix-accept unconnected (alien-sap sockaddr)
			    (alien-sap length))
      (if (minusp connected)
	  (error "Error accepting a connection: ~A"
		 (unix:get-unix-error-msg errno))
	  (values connected (ntohl (slot sockaddr 'addr)))))))

(defun get-peer-host-and-port (fd)
  "Return the peer host address and port in host order."
  (declare (type unix:unix-fd fd))
  (with-alien ((sockaddr inet-sockaddr)
	       (length (alien:array unsigned 1)))
    (setf (deref length 0) (alien-size inet-sockaddr :bytes))
    (multiple-value-bind (res errno)
	(unix::%unix-getpeername fd (alien-sap sockaddr) (alien-sap length))
      (if (minusp res)
	  (error "Error ~s getting peer host and port on FD ~d."
		 (unix:get-unix-error-msg errno) fd)
	  (values (ntohl (slot sockaddr 'addr))
		  (ntohs (slot sockaddr 'port)))))))

(defun get-socket-host-and-port (fd)
  (declare (type unix:unix-fd fd))
  (with-alien ((sockaddr inet-sockaddr)
	       (length (alien:array unsigned 1)))
    (setf (deref length 0) (alien-size inet-sockaddr :bytes))
    (multiple-value-bind (res errno)
	(unix::%unix-getsockname fd (alien-sap sockaddr) (alien-sap length))
      (if (minusp res)
	  (error "Error ~s getting socket host and port on FD ~d."
		 (unix:get-unix-error-msg errno) fd)
	  (values (ntohl (slot sockaddr 'addr))
		  (ntohs (slot sockaddr 'port)))))))


;;;; Out of Band Data.

;;; SIGURG-HANDLER -- internal
;;;
;;;   Routine that gets called whenever out-of-band data shows up. Checks each
;;; file descriptor for any oob data. If there is any, look for a handler for
;;; that character. If any are found, funcall them.

(defun sigurg-handler (signo code scp)
  (declare (ignore signo code scp))
  (let ((buffer (make-string 1))
	(handled nil))
    (declare (simple-string buffer))
    (dolist (handlers *oob-handlers*)
      (declare (list handlers))
      (multiple-value-bind (result errno)
	  #-freebsd (unix::%unix-recv (car handlers) buffer 1 msg-oob)
	  #+freebsd (unix::%unix-recvfrom (car handlers) buffer 1 msg-oob
					  (sys:int-sap 0) 0)
	(cond ((minusp result)
	       (cerror "Ignore it"
		       "Error recving oob data on ~A: ~A"
		       (car handlers)
		       (unix:get-unix-error-msg errno)))
	      (t
	       (setf handled t)
	       (let ((char (schar buffer 0))
		     (handled nil))
		 (declare (base-char char))
		 (dolist (handler (cdr handlers))
		   (declare (list handler))
		   (when (eql (car handler) char)
		     (funcall (cdr handler))
		     (setf handled t)))
		 (unless handled
		   (cerror "Ignore it"
			   "No oob handler defined for ~S on ~A"
			   char
			   (car handlers))))))))
    (unless handled
      (cerror "Ignore it"
	      "Got a SIGURG, but couldn't find any out-of-band data.")))
  (undefined-value))

;;; SEND-CHARACTER-OUT-OF-BAND -- public
;;;
;;;   Sends CHAR across FD out of band.

(defun send-character-out-of-band (fd char)
  (declare (integer fd)
	   (base-char char))
  (let ((buffer (make-string 1 :initial-element char)))
    (declare (simple-string buffer))
    (multiple-value-bind (result errno)
	#-freebsd (unix::%unix-send fd buffer 1 msg-oob)
	#+freebsd (unix::%unix-sendto fd buffer 1 msg-oob (sys:int-sap 0) 0)
      (when (minusp result)
	(error "Error sending ~S OOB to across ~A: ~A"
	       char fd (unix:get-unix-error-msg errno))))))
