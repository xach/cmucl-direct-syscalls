;;; -*- Mode: Lisp; Package: Unix -*-
;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
 "$Header: /home/dtc/cvs/src/cmucl/p86/code/unix-syscall.lisp,v 1.11 1999/09/13 15:38:11 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Support for direct system calls. These have the advantage that the
;;; errno is handled safely.
;;;
;;; Contributions from Peter Van Eynde 1999.

(in-package "UNIX")



;;; The Linux 'struct stat' as returned by the kernel differs from
;;; that returned by the C library so is redefined here.

#+linux
(def-alien-type nil
    (struct kernel-stat
            (st-dev unsigned-short)
            (st-pad1 unsigned-short)
            (st-ino long)
            (st-mode unsigned-short)
            (st-nlink unsigned-short)
            (st-uid unsigned-short)
            (st-gid unsigned-short)
            (st-rdev unsigned-short)
            (st-pad2 unsigned-short)
            (st-size unsigned-long)
            (st-blksize unsigned-long)
            (st-blocks unsigned-long)
            (st-atime unsigned-long)
            (unused-1 unsigned-long)
            (st-mtime unsigned-long)
            (unused-2 unsigned-long)
            (st-ctime unsigned-long)
            (unused-3 unsigned-long)
            (unused-4 unsigned-long)
            (unused-5 unsigned-long)))

;;; STAT and friends.

#+linux
(defmacro extract-kernel-stat-results (buf)
  `(values T
	   (slot ,buf 'st-dev)
	   (slot ,buf 'st-ino)
	   (slot ,buf 'st-mode)
	   (slot ,buf 'st-nlink)
	   (slot ,buf 'st-uid)
	   (slot ,buf 'st-gid)
	   (slot ,buf 'st-rdev)
	   (slot ,buf 'st-size)
	   (slot ,buf 'st-atime)
	   (slot ,buf 'st-mtime)
	   (slot ,buf 'st-ctime)
	   (slot ,buf 'st-blksize)
	   (slot ,buf 'st-blocks)))



(defvar *kernel-errno-values*
  (let ((hash (make-hash-table :test #'eq)))
    (loop for (code string) in (list 
                                (list 1	"Operation not permitted ")
                                (list 2	"No such file or directory ")
                                (list 3	"No such process ")
                                (list 4	"Interrupted system call ")
                                (list 5	"I/O error ")
                                (list 6	"No such device or address ")
                                (list 7	"Arg list too long ")
                                (list 8	"Exec format error ")
                                (list 9	"Bad file number ")
                                (list 10	"No child processes ")
                                (list 11	"Try again ")
                                (list 12	"Out of memory ")
                                (list 13	"Permission denied ")
                                (list 14	"Bad address ")
                                (list 15	"Block device required ")
                                (list 16	"Device or resource busy ")
                                (list 17	"File exists ")
                                (list 18	"Cross-device link ")
                                (list 19	"No such device ")
                                (list 20	"Not a directory ")
                                (list 21	"Is a directory ")
                                (list 22	"Invalid argument ")
                                (list 23	"File table overflow ")
                                (list 24	"Too many open files ")
                                (list 25	"Not a typewriter ")
                                (list 26	"Text file busy ")
                                (list 27	"File too large ")
                                (list 28	"No space left on device ")
                                (list 29	"Illegal seek ")
                                (list 30	"Read-only file system ")
                                (list 31	"Too many links ")
                                (list 32	"Broken pipe ")
                                (list 33	"Math argument out of domain of func ")
                                (list 34	"Math result not representable ")
                                (list 35	"Resource deadlock would occur ")
                                (list 36	"File name too long ")
                                (list 37	"No record locks available ")
                                (list 38	"Function not implemented ")
                                (list 39	"Directory not empty ")
                                (list 40	"Too many symbolic links encountered ")
                                (list 41 	"Operation would block")
                                (list 42	"No message of desired type ")
                                (list 43	"Identifier removed ")
                                (list 44	"Channel number out of range ")
                                (list 45	"Level 2 not synchronized ")
                                (list 46	"Level 3 halted ")
                                (list 47	"Level 3 reset ")
                                (list 48	"Link number out of range ")
                                (list 49	"Protocol driver not attached ")
                                (list 50	"No CSI structure available ")
                                (list 51	"Level 2 halted ")
                                (list 52	"Invalid exchange ")
                                (list 53	"Invalid request descriptor ")
                                (list 54	"Exchange full ")
                                (list 55	"No anode ")
                                (list 56	"Invalid request code ")
                                (list 57	"Invalid slot ")
                                (list 59	"Bad font file format ")
                                (list 60	"Device not a stream ")
                                (list 61	"No data available ")
                                (list 62	"Timer expired ")
                                (list 63	"Out of streams resources ")
                                (list 64	"Machine is not on the network ")
                                (list 65	"Package not installed ")
                                (list 66	"Object is remote ")
                                (list 67	"Link has been severed ")
                                (list 68	"Advertise error ")
                                (list 69	"Srmount error ")
                                (list 70	"Communication error on send ")
                                (list 71	"Protocol error ")
                                (list 72	"Multihop attempted ")
                                (list 73	"RFS specific error ")
                                (list 74	"Not a data message ")
                                (list 75	"Value too large for defined data type ")
                                (list 76	"Name not unique on network ")
                                (list 77	"File descriptor in bad state ")
                                (list 78	"Remote address changed ")
                                (list 79	"Can not access a needed shared library ")
                                (list 80	"Accessing a corrupted shared library ")
                                (list 81	".lib section in a.out corrupted ")
                                (list 82	"Attempting to link in too many shared libraries ")
                                (list 83	"Cannot exec a shared library directly ")
                                (list 84	"Illegal byte sequence ")
                                (list 85	"Interrupted system call should be restarted ")
                                (list 86	"Streams pipe error ")
                                (list 87	"Too many users ")
                                (list 88	"Socket operation on non-socket ")
                                (list 89	"Destination address required ")
                                (list 90	"Message too long ")
                                (list 91	"Protocol wrong type for socket ")
                                (list 92	"Protocol not available ")
                                (list 93	"Protocol not supported ")
                                (list 94	"Socket type not supported ")
                                (list 95	"Operation not supported on transport endpoint ")
                                (list 96	"Protocol family not supported ")
                                (list 97	"Address family not supported by protocol ")
                                (list 98	"Address already in use ")
                                (list 99	"Cannot assign requested address ")
                                (list 100	"Network is down ")
                                (list 101	"Network is unreachable ")
                                (list 102	"Network dropped connection because of reset ")
                                (list 103	"Software caused connection abort ")
                                (list 104	"Connection reset by peer ")
                                (list 105	"No buffer space available ")
                                (list 106	"Transport endpoint is already connected ")
                                (list 107	"Transport endpoint is not connected ")
                                (list 108	"Cannot send after transport endpoint shutdown ")
                                (list 109	"Too many references: cannot splice ")
                                (list 110	"Connection timed out ")
                                (list 111	"Connection refused ")
                                (list 112	"Host is down ")
                                (list 113	"No route to host ")
                                (list 114	"Operation already in progress ")
                                (list 115	"Operation now in progress ")
                                (list 116	"Stale NFS file handle ")
                                (list 117	"Structure needs cleaning ")
                                (list 118	"Not a XENIX named type file ")
                                (list 119	"No XENIX semaphores available ")
                                (list 120	"Is a named type file ")
                                (list 121	"Remote I/O error ")
                                (list 122	"Quota exceeded ")
                                (list 123	"No medium found ")
                                (list 124	"Wrong medium type ")
                                (list 512    "ERESTARTSYS")
                                (list 512    "ERESTARTNOINTR")
                                (list 514	"restart if no handler.. ")
                                (list 515	"No ioctl command ")
                                (list 522	"Update synchronization mismatch ")
                                (list 523	"Cookie is stale ")
                                (list 524	"Operation is not supported ")
                                (list 525	"Buffer or request is too small ")
                                (list 526	"An untranslatable error occurred ")
                                (list 527	"Type not supported by server ")
                                (list 528	"Request initiated, but will not complete before timeout ")
                                ) 
          do
          (setf (gethash code hash) string))
    hash))

(defun sanity-check-errno (errno)
  "Checks if the errno is sane..."
  (when nil (or (member errno (list 4 7 11 14))
            (>= errno 512)
            (eq :EOF 
                (gethash errno *kernel-errno-values*
                         :EOF)))
    (error "Inpossible errno: ~S ~S" errno 
           (gethash errno *kernel-errno-values* "Unknown error"))))

(defmacro with-syscall-safe (&body code)
  "Ensure that in the body the blocks of memory won't move round
  and confuse the FFI"
  `(system:without-gcing
    (without-interrupts
     (progn
       ,@code))))

;;; Unix-access accepts a path and a mode.  It returns two values the
;;; first is T if the file is accessible and NIL otherwise.  The second
;;; only has meaning in the second case and is the unix errno value.

(defun unix-access (path mode)
  "Given a file path (a string) and one of four constant modes,
   unix-access returns T if the file is accessible with that
   mode and NIL if not.  It also returns an errno value with
   NIL which determines why the file was not accessible.

   The access modes are:
	r_ok     Read permission.
	w_ok     Write permission.
	x_ok     Execute permission.
	f_ok     Presence of file."
  (declare (type unix-pathname path)
	   (type (mod 8) mode))
  (multiple-value-bind (result errno)
      (%unix-access path mode)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-chdir accepts a directory name and makes that the
;;; current working directory.

(defun unix-chdir (path)
  "Given a file path string, unix-chdir changes the current working 
   directory to the one specified."
  (declare (type unix-pathname path))
  (multiple-value-bind (result errno)
      (%unix-chdir path)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-chmod (path mode)
  "Given a file path string and a constant mode, unix-chmod changes the
   permission mode for that file to the one specified. The new mode
   can be created by logically OR'ing the following:

      setuidexec        Set user ID on execution.
      setgidexec        Set group ID on execution.
      savetext          Save text image after execution.
      readown           Read by owner.
      writeown          Write by owner.
      execown           Execute (search directory) by owner.
      readgrp           Read by group.
      writegrp          Write by group.
      execgrp           Execute (search directory) by group.
      readoth           Read by others.
      writeoth          Write by others.
      execoth           Execute (search directory) by others.
  
  It returns T on successfully completion; NIL and an error number
  otherwise."
  (declare (type unix-pathname path)
	   (type unix-file-mode mode))
  (multiple-value-bind (result errno)
      (%unix-chmod path mode)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-fchmod accepts a file descriptor ("fd") and a file protection mode
;;; ("mode") and changes the protection of the file described by "fd" to 
;;; "mode".

(defun unix-fchmod (fd mode) ;;; untested
  "Given an integer file descriptor and a mode (the same as those
   used for unix-chmod), unix-fchmod changes the permission mode
   for that file to the one specified. T is returned if the call
   was successful."
  (declare (type unix-fd fd)
	   (type unix-file-mode mode))
  (multiple-value-bind (result errno)
      (%unix-fchmod fd mode)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-chown (path uid gid)
  "Given a file path, an integer user-id, and an integer group-id,
   unix-chown changes the owner of the file and the group of the
   file to those specified.  Either the owner or the group may be
   left unchanged by specifying them as -1.  Note: Permission will
   fail if the caller is not the superuser."
  (declare (type unix-pathname path)
	   (type (or unix-uid (integer -1 -1)) uid)
	   (type (or unix-gid (integer -1 -1)) gid))
  (multiple-value-bind (result errno)
      (%unix-chown path uid gid)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-fchown is exactly the same as unix-chown except that the file
;;; is specified by a file-descriptor ("fd") instead of a pathname.

(defun unix-fchown (fd uid gid)
  "Unix-fchown is like unix-chown, except that it accepts an integer
   file descriptor instead of a file path name."
  (declare (type unix-fd fd)
	   (type (or unix-uid (integer -1 -1)) uid)
	   (type (or unix-gid (integer -1 -1)) gid))
  (multiple-value-bind (result errno)
      (%unix-fchown fd uid gid)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-close accepts a file descriptor and attempts to close the file
;;; associated with it.

(defun unix-close (fd)
  "Unix-close takes an integer file descriptor as an argument and
   closes the file associated with it.  T is returned upon successful
   completion, otherwise NIL and an error number."
  (declare (type unix-fd fd))
  (multiple-value-bind (result errno)
      (%unix-close fd)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-creat accepts a file name and a mode.  It creates a new file
;;; with name and sets it mode to mode (as for chmod).

(defun unix-creat (name mode)
  "Unix-creat accepts a file name and a mode (same as those for
   unix-chmod) and creates a file by that name with the specified
   permission mode.  It returns a file descriptor on success,
   or NIL and an error  number otherwise.
   This interface is made obsolete by UNIX-OPEN."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (multiple-value-bind (result errno)
      (with-syscall-safe
          (%unix-open name (logior o_creat o_trunc o_wronly) mode))
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values result 0))))

;;; Unix-dup returns a duplicate copy of the existing file-descriptor
;;; passed as an argument.

(defun unix-dup (fd)
  "Unix-dup duplicates an existing file descriptor (given as the
   argument) and return it.  If FD is not a valid file descriptor, NIL
   and an error number are returned."
  (declare (type unix-fd fd))
  (multiple-value-bind (result errno)
      (%unix-dup fd)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values result 0))))

;;; Unix-dup2 makes the second file-descriptor describe the same file
;;; as the first. If the second file-descriptor points to an open
;;; file, it is first closed. In any case, the second should have a 
;;; value which is a valid file-descriptor.

(defun unix-dup2 (fd1 fd2)
  "Unix-dup2 duplicates an existing file descriptor just as unix-dup
   does only the new value of the duplicate descriptor may be requested
   through the second argument.  If a file already exists with the
   requested descriptor number, it will be closed and the number
   assigned to the duplicate."
  (declare (type unix-fd fd1 fd2))
  (multiple-value-bind (result errno)
      (%unix-dup2 fd1 fd2)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-fcntl takes a file descriptor, an integer command
;;; number, and optional command arguments.  It performs
;;; operations on the associated file and/or returns inform-
;;; ation about the file.

(defun unix-fcntl (fd cmd arg)
  "Unix-fcntl manipulates file descriptors according to the
   argument CMD which can be one of the following:

   F-DUPFD         Duplicate a file descriptor.
   F-GETFD         Get file descriptor flags.
   F-SETFD         Set file descriptor flags.
   F-GETFL         Get file flags.
   F-SETFL         Set file flags.
   F-GETOWN        Get owner.
   F-SETOWN        Set owner.

   The flags that can be specified for F-SETFL are:

   FNDELAY         Non-blocking reads.
   FAPPEND         Append on each write.
   FASYNC          Signal pgrp when data ready.
   FCREAT          Create if nonexistant.
   FTRUNC          Truncate to zero length.
   FEXCL           Error if already created.
   "
  (declare (type unix-fd fd)
	   (type (unsigned-byte 16) cmd arg))
  (multiple-value-bind (result errno)
      (%unix-fcntl fd cmd arg)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values result errno))))

;;; Unix-link creates a hard link from name2 to name1.

(defun unix-link (name1 name2)
  "Unix-link creates a hard link from the file with name1 to the
   file with name2."
  (declare (type unix-pathname name1 name2))
  (multiple-value-bind (result errno)
      (%unix-link name1 name2)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-lseek accepts a file descriptor, an offset, and whence value.

;;; This is a 32-bit version of lseek, zero-filling the upper 32-bits
;;; of 64-bit offsets.
(defun unix-lseek (fd offset whence)
  "Unix-lseek accepts a file descriptor and moves the file pointer ahead
  a certain offset for that file.  Whence can be any of the following:
   l_set        Set the file pointer.
   l_incr       Increment the file pointer.
   l_xtnd       Extend the file size."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) offset)
	   (type (integer 0 2) whence))
  (multiple-value-bind (offset-high offset-low errno)
      (%unix-lseek fd 0 offset whence)
    (if (minusp offset-high)
	(values nil errno)
	(values offset-low errno))))

;;; This version of unix-lseek handles a 64-bit offset.
#+nil
(defun unix-lseek (fd offset whence)
  "Unix-lseek accepts a file descriptor and moves the file pointer ahead
  a certain offset for that file.  Whence can be any of the following:
   l_set        Set the file pointer.
   l_incr       Increment the file pointer.
   l_xtnd       Extend the file size."
  (declare (type unix-fd fd)
	   (type (integer 0 2) whence))
  (let ((offset-high 0)
	(offset-low 0))
    (declare (type (signed-byte 32) offset-high)
	     (type (unsigned-byte 32) offset-low))
    (etypecase offset
      (fixnum
       (setf offset-low (bignum:%fixnum-to-digit offset))
       (when (minusp offset)
	 (setf offset-high -1)))	; Sign extend.
      (bignum
       (setf offset-low (bignum:%bignum-ref offset 0))
       (setf offset-high (bignum:%fixnum-digit-with-correct-sign
			  (bignum:%bignum-ref offset 1)))))
    (multiple-value-bind (offset-high offset-low errno)
	(%unix-lseek fd offset-high offset-low whence)
      (if (minusp offset-high)
	  (values nil errno)
	  (if (zerop offset-high)
	      (values offset-low errno)
	      (values (logior (ash offset-high 32) offset-low) errno))))))

;;; Unix-mkdir accepts a name and a mode and attempts to create the
;;; corresponding directory with mode mode.

(defun unix-mkdir (name mode)
  "Unix-mkdir creates a new directory with the specified name and mode.
   (Same as those for unix-fchmod.)  It returns T upon success, otherwise
   NIL and an error number."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (multiple-value-bind (result errno)
      (%unix-mkdir name mode)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-open accepts a pathname (a simple string), flags, and mode and
;;; attempts to open file with name pathname.

(defun unix-open (path flags mode)
  "Unix-open opens the file whose pathname is specified by path
   for reading and/or writing as specified by the flags argument.
   The flags argument can be:

     o_rdonly        Read-only flag.
     o_wronly        Write-only flag.
     o_rdwr          Read-and-write flag.
     o_append        Append flag.
     o_creat         Create-if-nonexistant flag.
     o_trunc         Truncate-to-size-0 flag.

   If the o_creat flag is specified, then the file is created with
   a permission of argument mode if the file doesn't exist.  An
   integer file descriptor is returned by unix-open."
  (declare (type unix-pathname path)
	   (type (signed-byte 32) flags)
	   (type unix-file-mode mode))
  (multiple-value-bind (fd errno)
      (%unix-open path flags mode)
    (if (minusp fd)
	(values nil errno)
	(values fd 0))))

(defun unix-pipe ()
  "Unix-pipe sets up a unix-piping mechanism consisting of
  an input pipe and an output pipe.  Unix-Pipe returns two
  values: if no error occurred the first value is the pipe
  to be read from and the second is can be written to.  If
  an error occurred the first value is NIL and the second
  the unix error code."
  (with-alien ((fds (array int 2)))
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-pipe (alien-sap (addr fds))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  (values (deref fds 0) (deref fds 1))))))

;;; Unix-read accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer.  It returns the actual number of
;;; bytes read.

(defun unix-read (fd buf len)
  "Unix-read attempts to read from the file described by fd into
   the buffer buf until it is full.  Len is the length of the buffer.
   The number of bytes actually read is returned or NIL and an error
   number if an error occured."
  (declare (type unix-fd fd)
	   (type sys:system-area-pointer buf)
	   (type (unsigned-byte 32) len))

  (vm::system-area-fill 0 buf 0 len)

  (multiple-value-bind (bytes errno)
      (%unix-read fd buf len)
    (if (minusp bytes)
	(values nil errno)
	(values bytes errno))))

(defun unix-readlink (path)
  "Unix-readlink invokes the readlink system call on the file name
  specified by the simple string path.  It returns up to two values:
  the contents of the symbolic link if the call is successful, or
  NIL and the Unix error number."
  (declare (type unix-pathname path))
  (with-alien ((buf (array char 1024)))
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-readlink path (alien-sap (addr buf)) 1024))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  (let ((string (make-string result)))
	    (kernel:copy-from-system-area
	     (alien-sap (addr buf)) 0
	     string (* vm:vector-data-offset vm:word-bits)
	     (* result vm:byte-bits))
	    string)))))

;;; Unix-rename accepts two files names and renames the first to the second.

(defun unix-rename (name1 name2)
  "Unix-rename renames the file with string name1 to the string
   name2.  NIL and an error code is returned if an error occured."
  (declare (type unix-pathname name1 name2))
  (multiple-value-bind (result errno)
      (%unix-rename name1 name2)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-rmdir accepts a name and removes the associated directory.

(defun unix-rmdir (name)
  "Unix-rmdir attempts to remove the directory name.  NIL and
   an error number is returned if an error occured."
  (declare (type unix-pathname name))
  (multiple-value-bind (result errno)
      (%unix-rmdir name)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; UNIX-FAST-SELECT -- public.
;;;
(defmacro unix-fast-select (num-descriptors
			    read-fds write-fds exception-fds
			    timeout-secs &optional (timeout-usecs 0))
  "Perform the UNIX select(2) system call.
  (declare (type (integer 0 #.FD-SETSIZE) num-descriptors)
	   (type (or (alien (* (struct fd-set))) null)
		 read-fds write-fds exception-fds)
	   (type (or null (unsigned-byte 31)) timeout-secs)
	   (type (unsigned-byte 31) timeout-usecs)
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))"
  `(let ((timeout-secs ,timeout-secs)
	 (read-fds ,read-fds)
	 (write-fds ,write-fds)
	 (exception-fds ,exception-fds))
     (with-alien ((tv (struct timeval)))
       (when timeout-secs
	 (setf (slot tv 'tv-sec) timeout-secs)
	 (setf (slot tv 'tv-usec) ,timeout-usecs))
       (multiple-value-bind (nfds-ready errno)
           (with-syscall-safe
               (%unix-select ,num-descriptors
                             (if read-fds (alien-sap read-fds) (int-sap 0))
                             (if write-fds (alien-sap write-fds) (int-sap 0))
                             (if exception-fds
                                 (alien-sap exception-fds)
                                 (int-sap 0))
                             (if timeout-secs (alien-sap (addr tv)) (int-sap 0))))
	 (if (minusp nfds-ready)
	     (values nil errno)
	     (values nfds-ready errno))))))

(defun unix-select (nfds rdfds wrfds xpfds to-secs &optional (to-usecs 0))
  "Unix-select examines the sets of descriptors passed as arguments
   to see if they are ready for reading and writing.  See the UNIX
   Programmers Manual for more information."
  (declare (type (integer 0 #.FD-SETSIZE) nfds)
	   (type unsigned-byte rdfds wrfds xpfds)
	   (type (or (unsigned-byte 31) null) to-secs)
	   (type (unsigned-byte 31) to-usecs)
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (with-alien ((tv (struct timeval))
	       (rdf (struct fd-set))
	       (wrf (struct fd-set))
	       (xpf (struct fd-set)))
    (when to-secs
      (setf (slot tv 'tv-sec) to-secs)
      (setf (slot tv 'tv-usec) to-usecs))
    (num-to-fd-set rdf rdfds)
    (num-to-fd-set wrf wrfds)
    (num-to-fd-set xpf xpfds)
    (macrolet ((frob (lispvar alienvar)
		 `(if (zerop ,lispvar)
		      (int-sap 0)
		      (alien-sap (addr ,alienvar)))))
      (multiple-value-bind (nfds-ready errno)
          (with-syscall-safe
              (%unix-select nfds (frob rdfds rdf) (frob wrfds wrf) (frob xpfds xpf)
                            (if to-secs (alien-sap (addr tv)) (int-sap 0))))
	(if (minusp nfds-ready)
	    (values nil errno)
	    (values nfds-ready
		    (fd-set-to-num nfds rdf)
		    (fd-set-to-num nfds wrf)
		    (fd-set-to-num nfds xpf)))))))

;;; Unix-sync writes all information in core memory which has been modified
;;; to permanent storage (i.e. disk).

(defun unix-sync ()
  "Unix-sync writes all information in core memory which has been
   modified to disk.  It returns NIL and an error code if an error
   occured."
  (multiple-value-bind (result errno)
      (%unix-sync)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-fsync writes the core-image of the file described by "fd" to
;;; permanent storage (i.e. disk).

(defun unix-fsync (fd)
  "Unix-fsync writes the core image of the file described by
   fd to disk."
  (declare (type unix-fd fd))
  (multiple-value-bind (result errno)
      (%unix-fsync fd)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-truncate accepts a file name and a new length.  The file is
;;; truncated to the new length.

(defun unix-truncate (name len)
  "Unix-truncate truncates the named file to the length (in
   bytes) specified by len.  NIL and an error number is returned
   if the call is unsuccessful."
  (declare (type unix-pathname name)
	   (type (unsigned-byte 32) len))
  (multiple-value-bind (result errno)
      (%unix-truncate name len)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-ftruncate (fd len)
  "Unix-ftruncate is similar to unix-truncate except that the first
   argument is a file descriptor rather than a file name."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))
  (multiple-value-bind (result errno)
      (%unix-ftruncate fd len)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-symlink (name1 name2)
  "Unix-symlink creates a symbolic link named name2 to the file
   named name1.  NIL and an error number is returned if the call
   is unsuccessful."
  (declare (type unix-pathname name1 name2))
  (multiple-value-bind (result errno)
      (%unix-symlink name1 name2)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-unlink accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link.

(defun unix-unlink (name)
  "Unix-unlink removes the directory entry for the named file.
   NIL and an error code is returned if the call fails."
  (declare (type unix-pathname name))
  (multiple-value-bind (result errno)
      (%unix-unlink name)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-write accepts a file descriptor, a buffer, an offset, and the
;;; length to write.  It attempts to write len bytes to the device
;;; associated with fd from the buffer starting at offset.  It returns
;;; the actual number of bytes written.

(defun unix-write (fd buf offset len)
  "Unix-write attempts to write a character buffer (buf) of length
   len to the file described by the file descriptor fd.  NIL and an
   error is returned if the call is unsuccessful."
  (declare (type unix-fd fd)
	   (type (or (simple-array * (*)) sys:system-area-pointer) buf)
	   (type (unsigned-byte 31) offset len))
  (let ((buf (sys:sap+ (etypecase buf
			 ((simple-array * (*))
			  (sys:vector-sap buf))
			 (sys:system-area-pointer
			  buf))
		       offset)))
    (multiple-value-bind (bytes errno)
	(%unix-write fd buf len)
      (if (minusp bytes)
	  (values nil errno)
	  (values bytes errno)))))

;;; Unix-ioctl is used to change parameters of devices in a device
;;; dependent way.

(defun unix-ioctl (fd cmd arg)
  "Unix-ioctl performs a variety of operations on open i/o
   descriptors.  See the UNIX Programmer's Manual for more
   information."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) cmd)
	   (type system-area-pointer arg))
  (multiple-value-bind (result errno)
      (%unix-ioctl fd cmd arg)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-raw-exit terminates a program without flushing C file
;;; streams, this is _exit.

(defun unix-raw-exit (&optional (code 0))
  "Unix-exit terminates the current process with an optional
   error code.  If successful, the call doesn't return.  If
   unsuccessful, the call returns NIL and an error number."
  (declare (type (signed-byte 32) code))
  (multiple-value-bind (result errno)
      (%unix-exit code)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-stat (name)
  "Unix-stat retrieves information about the specified
   file returning them in the form of multiple values.
   See the UNIX Programmer's Manual for a description
   of the values returned.  If the call fails, then NIL
   and an error number is returned instead."
  (declare (type unix-pathname name))
  (when (string= name "")
    (setf name "."))
  (with-alien ((buf (struct #-linux stat #+linux kernel-stat)))
    (multiple-value-bind (result errno)        
        (with-syscall-safe
            (%unix-stat name (alien-sap (addr buf))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  #-linux (extract-stat-results buf)
	  #+linux (extract-kernel-stat-results buf)))))


(defun unix-lstat (name)
  "Unix-lstat is similar to unix-stat except the specified
   file must be a symbolic link."
  (declare (type unix-pathname name))
  (with-alien ((buf (struct #-linux stat #+linux kernel-stat)))
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-lstat name (alien-sap (addr buf))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  #-linux (extract-stat-results buf)
	  #+linux (extract-kernel-stat-results buf)))))

(defun unix-fstat (fd)
  "Unix-fstat is similar to unix-stat except the file is specified
   by the file descriptor fd."
  (declare (type unix-fd fd))
  (with-alien ((buf (struct #-linux stat #+linux kernel-stat)))
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-fstat fd (alien-sap (addr buf))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  #-linux (extract-stat-results buf)
	  #+linux (extract-kernel-stat-results buf)))))

(declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
  "Like call getrusage, but return only the system and user time, and returns
   the seconds and microseconds as separate values."
  (declare (type (member #.rusage_self #.rusage_children) who)
	   (values (member t)
		   (unsigned-byte 31) (mod 1000000)
		   (unsigned-byte 31) (mod 1000000)))
  (with-alien ((usage (struct rusage)))
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-getrusage who (alien-sap (addr usage))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (error "Getrusage failed: ~A" (get-unix-error-msg errno))
	  (values t
		  (slot (slot usage 'ru-utime) 'tv-sec)
		  (slot (slot usage 'ru-utime) 'tv-usec)
		  (slot (slot usage 'ru-stime) 'tv-sec)
		  (slot (slot usage 'ru-stime) 'tv-usec))))))

(defun unix-getrusage (who)
  "Unix-getrusage returns information about the resource usage
   of the process specified by who.  Who can be either the
   current process (rusage_self) or all of the terminated
   child processes (rusage_children).  NIL and an error number
   is returned if the call fails."
  (declare (type (member #.rusage_self #.rusage_children) who))
  (with-alien ((usage (struct rusage)))
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-getrusage who (alien-sap (addr usage))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  (values t
		  (+ (* (slot (slot usage 'ru-utime) 'tv-sec) 1000000)
		     (slot (slot usage 'ru-utime) 'tv-usec))
		  (+ (* (slot (slot usage 'ru-stime) 'tv-sec) 1000000)
		     (slot (slot usage 'ru-stime) 'tv-usec))
		  (slot usage 'ru-maxrss)
		  (slot usage 'ru-ixrss)
		  (slot usage 'ru-idrss)
		  (slot usage 'ru-isrss)
		  (slot usage 'ru-minflt)
		  (slot usage 'ru-majflt)
		  (slot usage 'ru-nswap)
		  (slot usage 'ru-inblock)
		  (slot usage 'ru-oublock)
		  (slot usage 'ru-msgsnd)
		  (slot usage 'ru-msgrcv)
		  (slot usage 'ru-nsignals)
		  (slot usage 'ru-nvcsw)
		  (slot usage 'ru-nivcsw))))))

(declaim (inline unix-gettimeofday))
(defun unix-gettimeofday ()
  "If it works, unix-gettimeofday returns 5 values: T, the seconds and
   microseconds of the current time of day, the timezone (in minutes west
   of Greenwich), and a daylight-savings flag.  If it doesn't work, it
   returns NIL and the errno."
  (with-alien ((tv (struct timeval))
	       (tz (struct timezone)))
    (multiple-value-bind (res errno)
        (with-syscall-safe
            (%unix-gettimeofday (alien-sap (addr tv)) (alien-sap (addr tz))))
      (if (minusp res)
	  (error "Gettimeofday failed: ~A" (get-unix-error-msg errno))
	  (values T (slot tv 'tv-sec) (slot tv 'tv-usec)
		  (slot tz 'tz-minuteswest) (slot tz 'tz-dsttime))))))

#-linux
(defun unix-utimes (file atime-sec atime-usec mtime-sec mtime-usec)
  "Unix-utimes sets the 'last-accessed' and 'last-updated'
   times on a specified file.  NIL and an error number is
   returned if the call is unsuccessful."
  (declare (type unix-pathname file)
	   (type (alien unsigned-long)
		 atime-sec atime-usec
		 mtime-sec mtime-usec))
  (with-alien ((tvp (array (struct timeval) 2)))
    (setf (slot (deref tvp 0) 'tv-sec) atime-sec)
    (setf (slot (deref tvp 0) 'tv-usec) atime-usec)
    (setf (slot (deref tvp 1) 'tv-sec) mtime-sec)
    (setf (slot (deref tvp 1) 'tv-usec) mtime-usec)
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-utimes file (alien-sap (addr tvp))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
	  (values nil errno)
	  (values t 0)))))

#+linux
(defun unix-utimes (file atime-sec atime-usec mtime-sec mtime-usec)
  "Unix-utimes sets the 'last-accessed' and 'last-updated'
   times on a specified file.  NIL and an error number is
   returned if the call is unsuccessful."
  (declare (type unix-pathname file)
	   (type (alien unsigned-long) atime-sec mtime-sec)
	   (ignore mtime-usec atime-usec))
  (with-alien ((buf (struct utimbuf)))
    (setf (slot buf 'actime) atime-sec)
    (setf (slot buf 'modtime) mtime-sec) 
    (multiple-value-bind (result errno)
        (with-syscall-safe
            (%unix-utime file (alien-sap (addr buf))))
      (when (minusp result)  (sanity-check-errno errno))
      (if (minusp result)
          (values nil errno)
          (values t 0)))))

;;; Unix-setreuid sets the real and effective user-id's of the current
;;; process to the arguments "ruid" and "euid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "ruid" or
;;; "euid" to -1 makes the system use the current id instead.

(defun unix-setreuid (ruid euid)
  "Unix-setreuid sets the real and effective user-id's of the current
   process to the specified ones.  NIL and an error number is returned
   if the call fails."
  (multiple-value-bind (result errno)
      (%unix-setreuid ruid euid)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

;;; Unix-setregid sets the real and effective group-id's of the current
;;; process to the arguments "rgid" and "egid", respectively.  Usage is
;;; restricted for anyone but the super-user.  Setting either "rgid" or
;;; "egid" to -1 makes the system use the current id instead.

(defun unix-setregid (rgid egid)
  "Unix-setregid sets the real and effective group-id's of the current
   process process to the specified ones.  NIL and an error number is
   returned if the call fails."
  (multiple-value-bind (result errno)
      (%unix-setregid rgid egid)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-getpid ()
  "Unix-getpid returns the process-id of the current process."
  (%unix-getpid))

(defun unix-getppid ()
  "Unix-getppid returns the process-id of the parent of the current process."
  (%unix-getppid))

(defun unix-getgid ()
  "Unix-getgid returns the real group-id of the current process."
  (%unix-getgid))

(defun unix-getegid ()
  "Unix-getegid returns the effective group-id of the current process."
  (%unix-getegid))

;;; Unix-getpgrp returns the group-id associated with the
;;; current process.

(defun unix-getpgrp ()
  "Unix-getpgrp returns the group-id of the calling process."
  (%unix-getpgrp))

;;; Unix-setpgid sets the group-id of the process specified by 
;;; "pid" to the value of "pgrp".  The process must either have
;;; the same effective user-id or be a super-user process.

;;; setpgrp(int int)[freebsd] is identical to setpgid and is retained
;;; for backward compatibility. setpgrp(void)[solaris] is being phased
;;; out in favor of setsid().

(defun unix-setpgrp (pid pgrp)
  "Unix-setpgrp sets the process group on the process pid to
   pgrp.  NIL and an error number are returned upon failure."
  (multiple-value-bind (result errno)
      (%unix-setpgid pid pgrp)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-setpgid (pid pgrp)
  "Unix-setpgid sets the process group of the process pid to
   pgrp. If pgid is equal to pid, the process becomes a process
   group leader. NIL and an error number are returned upon failure."
  (multiple-value-bind (result errno)
      (%unix-setpgid pid pgrp)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))

(defun unix-getuid ()
  "Unix-getuid returns the real user-id associated with the
   current process."
  (%unix-getuid))

;;; For linux need to use %unix-uname.
#+(and linux not-yet)
(defun unix-gethostname ()
  "Unix-gethostname returns the name of the host machine as a string."
  (with-alien ((buf (array char 256)))
    (syscall ("gethostname" (* char) int)
	     (cast buf c-string)
	     (cast buf (* char)) 256)))

(defun unix-fork ()
  "Executes the unix fork system call.  Returns 0 in the child and the pid
   of the child in the parent if it works, or NIL and an error number if it
   doesn't work."
  (multiple-value-bind (result errno)
      (%unix-fork)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values result 0))))

;;;; UNIX-EXECVE

(defun sub-unix-execve (program arg-list env-list)
  (let ((argv nil)
	(argv-bytes 0)
	(envp nil)
	(envp-bytes 0))
    (unwind-protect
         (progn
           ;; Blast the stuff into the proper format
           (multiple-value-setq
               (argv argv-bytes)
             (string-list-to-c-strvec arg-list))
           (multiple-value-setq
               (envp envp-bytes)
             (string-list-to-c-strvec env-list))
           ;;
           ;; Now do the system call
           (multiple-value-bind (result errno)
               (%unix-execve program argv envp)
             (when (minusp result)  (sanity-check-errno errno))
             (if (minusp result)
                 (values nil errno)
                 (values t 0))))
      ;;
      ;; Deallocate memory
      (when argv
	(system:deallocate-system-memory argv argv-bytes))
      (when envp
	(system:deallocate-system-memory envp envp-bytes)))))

(defun unix-getitimer (which)
  "Unix-getitimer returns the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). On success,
   unix-getitimer returns 5 values,
   T, it-interval-secs, it-interval-usec, it-value-secs, it-value-usec."
  (declare (type (member :real :virtual :profile) which))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itv (struct itimerval)))
      (multiple-value-bind (result errno)
          (with-syscall-safe
              (%unix-getitimer which (alien-sap (addr itv))))
        (when (minusp result)  (sanity-check-errno errno))
        (if (minusp result)
            (values nil errno)
            (values T
                    (slot (slot itv 'it-interval) 'tv-sec)
                    (slot (slot itv 'it-interval) 'tv-usec)
                    (slot (slot itv 'it-value) 'tv-sec)
                    (slot (slot itv 'it-value) 'tv-usec)))))))

(defun unix-setitimer(which int-secs int-usec val-secs val-usec)
  "Unix-setitimer sets the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). A SIGALRM signal
   will be delivered VALUE <seconds+microseconds> from now. INTERVAL,
   when non-zero, is <seconds+microseconds> to be loaded each time
   the timer expires. Setting INTERVAL and VALUE to zero disables
   the timer. See the Unix man page for more details. On success,
   unix-setitimer returns the old contents of the INTERVAL and VALUE
   slots as in unix-getitimer."
  (declare (type (member :real :virtual :profile) which)
	   (type (unsigned-byte 29) int-secs val-secs)
	   (type (integer 0 (1000000)) int-usec val-usec))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itvn (struct itimerval))
		 (itvo (struct itimerval)))
      (setf (slot (slot itvn 'it-interval) 'tv-sec ) int-secs
	    (slot (slot itvn 'it-interval) 'tv-usec) int-usec
	    (slot (slot itvn 'it-value   ) 'tv-sec ) val-secs
	    (slot (slot itvn 'it-value   ) 'tv-usec) val-usec)
      (multiple-value-bind (result errno)
          (with-syscall-safe
              (%unix-setitimer which
                               (alien-sap (addr itvn))
                               (alien-sap (addr itvo))))
        (when (minusp result)  (sanity-check-errno errno))
        (if (minusp result)
            (values nil errno)
            (values T
                    (slot (slot itvo 'it-interval) 'tv-sec)
                    (slot (slot itvo 'it-interval) 'tv-usec)
                    (slot (slot itvo 'it-value) 'tv-sec)
                    (slot (slot itvo 'it-value) 'tv-usec)))))))


#+linux
(defun unix-sched-yield ()
  "Yield the current processs. It returns NIL and an error code if an error
   occured."
  (multiple-value-bind (result errno)
      (%unix-sched-yield)
    (when (minusp result)  (sanity-check-errno errno))
    (if (minusp result)
	(values nil errno)
	(values t 0))))
