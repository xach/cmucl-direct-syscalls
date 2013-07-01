;;; -*- Mode: Lisp; Package: X86 -*-
;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /home/dtc/cvs/src/cmucl/p86/compiler/x86/syscall-linux.lisp,v 1.12 1999/09/13 16:39:03 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Support for direct system calls. These have the advantage that the
;;; errno is handled safely.
;;;
;;; Contributions from Peter Van Eynde 1999.

(in-package "X86")



;;; Linux systems call arguments are passed in registers.
;;;
;;; String arguments are kept live until after the sycall to prevent
;;; the object being moved in case of a garbage collection.
;;; 
(defmacro define-syscall-vop (&rest arguments)
  "Defines a Linux syscall vop for given arguments. The arguments are
  either :signed, :unsigned, :string, or :sap."
  (collect ((vop-suffixes) (args) (arg-types) (temps) (loads))
    (do ((arguments arguments (rest arguments))
	 (arg-num 0 (1+ arg-num))
	 (arg-regs '(ebx ecx edx esi edi) (rest arg-regs)))
	((endp arguments))
      (let* ((argument (first arguments))
	     (name (symbolicate "ARG" (format nil "~D" arg-num)))
	     (arg-reg (first arg-regs))
	     (scs (ecase argument
		    (:signed 'signed-reg)
		    (:unsigned 'unsigned-reg)
		    (:string 'descriptor-reg)
		    (:sap 'sap-reg))))
	(vop-suffixes (ecase argument
			(:signed '-signed)
			(:unsigned '-unsigned)
			(:string '-string)
			(:sap '-sap)))
	(args (if (eq argument :string)
		  `(,name :scs (,scs) :to :result)
		  `(,name :scs (,scs) :target ,arg-reg)))
	(arg-types (ecase argument
		     (:signed 'signed-num)
		     (:unsigned 'unsigned-num)
		     (:string 'simple-string)
		     (:sap 'system-area-pointer)))
	(temps `(:temporary (:sc ,(ecase argument
				    (:signed 'signed-reg)
				    (:unsigned 'unsigned-reg)
				    (:string 'unsigned-reg)
				    (:sap 'sap-reg))
			     :offset ,(symbolicate arg-reg "-OFFSET")
			     :from (:argument ,arg-num)
			     :to :eval)
			    ,arg-reg))
	(loads (if (eq argument :string)
		   `(inst lea ,arg-reg
			  (make-ea :byte :base ,name
				   :disp (- (* vector-data-offset word-bytes)
					    other-pointer-type)))
		   `(move ,arg-reg ,name)))))
    `(define-vop (,(apply #'symbolicate "%UNIX-SYSCALL"
			  (or (vop-suffixes) '("-VOID"))))
       (:policy :fast-safe)
       (:args ,@(args))
       (:arg-types ,@(arg-types))
       ,@(temps)
       (:temporary (:sc signed-reg :offset eax-offset
			:from :eval :to :result :target result) eax)
       (:results (result :scs (signed-reg))
		 (errno :scs (unsigned-reg)))
       (:result-types signed-num unsigned-num)
       (:variant-vars syscall-number)
       (:generator 1
	  (let ((error (gen-label))
		(done (gen-label)))
	    ,@(loads)
	    (inst mov eax syscall-number)
	    (inst int #x80)
	    (inst cmp eax -4095)
	    (inst jmp :ae error)
	    (move result eax)
	    (inst xor errno errno)
	    (emit-label done)

	    (assemble (*elsewhere*)
	      (emit-label error)
	      (inst neg eax)
	      (move errno eax)
	      (inst mov result -1)
	      (inst jmp done)))))))

(defmacro define-syscall (vop-name number result-type &rest arguments)
  "Define a syscall with the given number, result type, and
  arguments which may be :signed, :unsigned, :sap, or :string."
  (collect ((vop-suffixes) (arg-types))
    (do ((arguments arguments (rest arguments))
	 (arg-num 0 (1+ arg-num)))
	((endp arguments))
      (let ((argument (first arguments)))
	(vop-suffixes (ecase argument
			(:signed '-signed)
			(:unsigned '-unsigned)
			(:string '-string)
			(:sap '-sap)))
	(arg-types (ecase argument
		     (:signed '(signed-byte 32))
		     (:unsigned '(unsigned-byte 32))
		     (:string 'simple-string)
		     (:sap 'system-area-pointer)))))
    (let ((name (intern (symbol-name vop-name) "UNIX")))
      `(progn
	 (defknown ,name ,(arg-types) (values ,result-type (unsigned-byte 12)))
	 (define-vop (,vop-name ,(apply #'symbolicate "%UNIX-SYSCALL"
					(or (vop-suffixes) '("-VOID"))))
	   (:translate ,name)
	   (:variant ,number))))))

;;; Linux socket systems call arguments are passed on the stack.
;;; 
;;; String arguments are kept live until after the sycall to prevent
;;; the object being moved in case of a garbage collection.
;;; 
(defmacro define-socketcall-vop (&rest arguments)
  "Defines a Linux socket call vop for given arguments. The arguments are
  either :signed, :unsigned, :string, or :sap."
  (let ((loads ()))
    (collect ((vop-suffixes) (args) (arg-types))
      (do ((arguments arguments (rest arguments))
	   (arg-num 0 (1+ arg-num)))
	  ((endp arguments))
	(let* ((argument (first arguments))
	       (name (symbolicate "ARG" (format nil "~D" arg-num)))
	       (scs (ecase argument
		      (:signed 'signed-reg)
		      (:unsigned 'unsigned-reg)
		      (:string 'descriptor-reg)
		      (:sap 'sap-reg))))
	  (vop-suffixes (ecase argument
			  (:signed '-signed)
			  (:unsigned '-unsigned)
			  (:string '-string)
			  (:sap '-sap)))
	  (args (if (eq argument :string)
		    `(,name :scs (,scs) :to :result)
		    `(,name :scs (,scs) :to :eval)))
	  (arg-types (ecase argument
		       (:signed 'signed-num)
		       (:unsigned 'unsigned-num)
		       (:string 'simple-string)
		       (:sap 'system-area-pointer)))
	  (when (eq argument :string)
	    (push `(inst add (make-ea :dword :base esp-tn) 
			 (- (* vector-data-offset word-bytes)
			    other-pointer-type))
		   loads))
	  (push `(inst push ,name) loads)))
      `(define-vop (,(apply #'symbolicate "%UNIX-SOCKETCALL" (vop-suffixes)))
	 (:policy :fast-safe)
	 (:args ,@(args))
	 (:arg-types ,@(arg-types))
	 (:temporary (:sc unsigned-reg :offset ebx-offset
			  :from :eval :to :result) ebx)
	 (:temporary (:sc unsigned-reg :offset ecx-offset
			  :from :eval :to :result) ecx)
	 (:temporary (:sc signed-reg :offset eax-offset
			  :from :eval :to :result :target result) eax)
	 (:results (result :scs (signed-reg))
		   (errno :scs (unsigned-reg)))
	 (:result-types signed-num unsigned-num)
	 (:variant-vars subcode)
	 (:generator 1
	    (let ((error (gen-label))
		  (done (gen-label)))
	      ,@loads
	      (inst mov ecx esp-tn)
	      (inst mov ebx subcode)
	      (inst mov eax 102)
	      (inst int #x80)
	      (inst add esp-tn ,(* (length arguments) word-bytes))
	      (inst cmp eax -4095)
	      (inst jmp :ae error)
	      (move result eax)
	      (inst xor errno errno)
	      (emit-label done)

	      (assemble (*elsewhere*)
	        (emit-label error)
		(inst neg eax)
		(move errno eax)
		(inst mov result -1)
		(inst jmp done))))))))

(defmacro define-socketcall (vop-name number result-type &rest arguments)
  "Define a Linux socket call with the given number, result type, and
  arguments which may be :signed, :unsigned, :sap, or :string."
  (collect ((vop-suffixes) (arg-types))
    (do ((arguments arguments (rest arguments))
	 (arg-num 0 (1+ arg-num)))
	((endp arguments))
      (let ((argument (first arguments)))
	(vop-suffixes (ecase argument
			(:signed '-signed)
			(:unsigned '-unsigned)
			(:string '-string)
			(:sap '-sap)))
	(arg-types (ecase argument
		     (:signed '(signed-byte 32))
		     (:unsigned '(unsigned-byte 32))
		     (:string 'simple-string)
		     (:sap 'system-area-pointer)))))
    (let ((name (intern (symbol-name vop-name) "UNIX")))
      `(progn
	 (defknown ,name ,(arg-types) (values ,result-type (unsigned-byte 12)))
	 (define-vop (,vop-name ,(apply #'symbolicate "%UNIX-SOCKETCALL"
					(vop-suffixes)))
	   (:translate ,name)
	   (:variant ,number))))))
	

;;; Syscalls.

(define-syscall-vop)
(define-syscall %unix-fork 2 (signed-byte 32))
(define-syscall %unix-getpid 20 (signed-byte 32))
(define-syscall %unix-getuid 24 (signed-byte 32))
(define-syscall %unix-sync 36 (member 0))
(define-syscall %unix-getgid 47 (signed-byte 32))
(define-syscall %unix-geteuid 49 (signed-byte 32))
(define-syscall %unix-getegid 50 (signed-byte 32))
(define-syscall %unix-getppid 64 (signed-byte 32))
(define-syscall %unix-getpgrp 65 (signed-byte 32))
(define-syscall %unix-setsid 66 (signed-byte 32))
(define-syscall %unix-sched-yield 158 (member -1 0))

(define-syscall-vop :signed)
(define-syscall %unix-exit 1 (member -1 0) :signed)
(define-syscall %unix-close 6 (member -1 0) :signed)
(define-syscall %unix-dup 41 fixnum :signed)
(define-syscall %unix-fsync 118 (member -1 0) :signed)

(define-syscall-vop :signed :signed)
;;; setpgid; implements setpgrp as setpgid(0, 0).
(define-syscall %unix-setpgid 57 (member -1 0) :signed :signed)
(define-syscall %unix-dup2 63 fixnum :signed :signed)
(define-syscall %unix-setreuid 70 (member -1 0) :signed :signed)
(define-syscall %unix-setregid 71 (member -1 0) :signed :signed)

(define-syscall-vop :signed :unsigned)
(define-syscall %unix-ftruncate 93 (member -1 0) :signed :unsigned)
(define-syscall %unix-fchmod 94 (member -1 0) :signed :unsigned)

(define-syscall-vop :signed :signed :signed)
(define-syscall %unix-fcntl 55 (signed-byte 32) :signed :signed :signed)
(define-syscall %unix-fchown 95 (member -1 0) :signed :signed :signed)

(define-syscall-vop :signed :unsigned :sap)
(define-syscall %unix-ioctl 54 (member -1 0) :signed :unsigned :sap)

(define-syscall-vop :signed :sap)
(define-syscall %unix-getrusage 77 (member -1 0) :signed :sap)
(define-syscall %unix-getitimer 105 (member -1 0) :signed :sap)
(define-syscall %unix-fstat 108 (member -1 0) :signed :sap)

(define-syscall-vop :signed :sap :sap)
(define-syscall %unix-setitimer 104 (member -1 0) :signed :sap :sap)

(define-syscall-vop :signed :sap :unsigned)
(define-syscall %unix-read 3 (signed-byte 32) :signed :sap :unsigned)
(define-syscall %unix-write 4 (signed-byte 32) :signed :sap :unsigned)

(define-syscall-vop :signed :sap :sap :sap :sap)
(define-syscall %unix-select 142 fixnum :signed :sap :sap :sap :sap)

(define-syscall-vop :sap)
(define-syscall %unix-pipe 42 (member -1 0) :sap)
;;; uname; implements gethostname.
(define-syscall %unix-uname 122 (member -1 0) :sap)

(define-syscall-vop :sap :sap)
(define-syscall %unix-gettimeofday 78 (member -1 0) :sap :sap)

(define-syscall-vop :string)
(define-syscall %unix-unlink 10 (member -1 0) :string)
(define-syscall %unix-chdir 12 (member -1 0) :string)
(define-syscall %unix-rmdir 40 (member -1 0) :string)

(define-syscall-vop :string :signed)
(define-syscall %unix-access 33 (member -1 0) :string :signed)

(define-syscall-vop :string :unsigned)
(define-syscall %unix-chmod 15 (member -1 0) :string :unsigned)
(define-syscall %unix-mkdir 39 (member -1 0) :string :unsigned)
(define-syscall %unix-truncate 92 (member -1 0) :string :unsigned)

(define-syscall-vop :string :signed :signed)
(define-syscall %unix-chown 182 (member -1 0) :string :signed :signed)

(define-syscall-vop :string :signed :unsigned)
(define-syscall %unix-open 5 fixnum :string :signed :unsigned)

(define-syscall-vop :string :string)
(define-syscall %unix-link 9 (member -1 0) :string :string)
(define-syscall %unix-rename 38 (member -1 0) :string :string)
(define-syscall %unix-symlink 83 (member -1 0) :string :string)

(define-syscall-vop :string :sap)
;;; utime; used to implement utimes.
(define-syscall %unix-utime 30 (member -1 0) :string :sap)
(define-syscall %unix-stat 106 (member -1 0) :string :sap)
(define-syscall %unix-lstat 107 (member -1 0) :string :sap)

(define-syscall-vop :string :sap :signed)
(define-syscall %unix-readlink 85 fixnum :string :sap :signed)

(define-syscall-vop :string :sap :sap)
(define-syscall %unix-execve 11 (member -1 0) :string :sap :sap)


;;; Socket calls.

(define-socketcall-vop :signed :signed :signed)
(define-socketcall %unix-socket 1 fixnum :signed :signed :signed)

(define-socketcall-vop :signed :sap :signed)
(define-socketcall %unix-bind 2 (member -1 0) :signed :sap :signed)
(define-socketcall %unix-connect 3 (member -1 0) :signed :sap :signed)

(define-socketcall-vop :signed :signed)
(define-socketcall %unix-listen 4 (member -1 0) :signed :signed)

(define-socketcall-vop :signed :sap :sap)
(define-socketcall %unix-accept 5 fixnum :signed :sap :sap)
(define-socketcall %unix-getsockname 6 (member -1 0) :signed :sap :sap)
(define-socketcall %unix-getpeername 7 (member -1 0) :signed :sap :sap)

(define-socketcall-vop :signed :string :signed :unsigned)
(define-socketcall %unix-send 9 (signed-byte 32)
		   :signed :string :signed :unsigned)
(define-socketcall %unix-recv 10 (signed-byte 32)
		   :signed :string :signed :unsigned)


;;; Lseek.

;;; This version of %unix-lseek handles 64-bit offsets by passing and
;;; returning the high and low 32-bit words of the offset.
;;;
(defknown unix::%unix-lseek ((signed-byte 32)
			     (signed-byte 32) (unsigned-byte 32)
			     (signed-byte 32))
  (values (signed-byte 32) (unsigned-byte 32) (unsigned-byte 32)))

(define-vop (%unix-lseek)
  (:translate unix::%unix-lseek)
  (:policy :fast-safe)
  (:args (fd :scs (signed-reg) :target ebx)
	 (offset-high :scs (signed-reg) :target ecx)
	 (offset-low :scs (unsigned-reg) :target edx)
	 (whence :scs (signed-reg) :target edi))
  (:arg-types signed-num signed-num unsigned-num signed-num)
  (:temporary (:sc signed-reg :offset ebx-offset
		   :from (:argument 0) :to :eval) ebx)
  (:temporary (:sc signed-reg :offset ecx-offset
		   :from (:argument 1) :to :eval) ecx)
  (:temporary (:sc unsigned-reg :offset edx-offset
		   :from (:argument 2) :to :eval) edx)
  (:temporary (:sc unsigned-reg :offset esi-offset
		   :from :eval :to :result) esi)
  (:temporary (:sc signed-reg :offset edi-offset
		   :from (:argument 4) :to :eval) edi)
  (:temporary (:sc signed-reg :offset eax-offset
		   :from :eval :to :result :target errno) eax)
  (:results (res-offset-high :scs (signed-reg))
	    (res-offset-low :scs (unsigned-reg))
	    (errno :scs (unsigned-reg)))
  (:result-types signed-num unsigned-num unsigned-num)
  (:generator 1
    (let ((error (gen-label))
	  (done (gen-label)))
      (move ebx fd)
      (move ecx offset-high)
      (move edx offset-low)
      (move edi whence)
      (inst sub esp-tn 8)	; Room for the 64 bit result.
      (inst mov esi esp-tn)
      (inst mov eax 140)
      (inst int #x80)
      (inst cmp eax -125)
      (inst jmp :ae error)
      (inst pop res-offset-low)
      (inst pop res-offset-high)
      (inst xor errno errno)
      (emit-label done)

      (assemble (*elsewhere*)
         (emit-label error)
	 (inst neg eax)
	 (move errno eax)
	 (inst mov res-offset-high -1)
	 (inst mov res-offset-low -1)
	 (inst jmp done)))))

