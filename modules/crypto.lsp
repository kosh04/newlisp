;; @module crypto.lsp 
;; @description SSL crypto functions for MD5 and SHA-1 hashing
;; @version 1.01 - initial release
;; @version 1.02 - renamed to crypto, new lib detection
;; @version 1.04 - added hmac encryption from amazon.com query API
;; @version 1.05 - added added gnuwin32/bin/libeay32.dll for crypto on Win32
;; @version 1.06 - added ripemd160
;; @version 1.07 - added libcrypto for OpenBSD and tested for 64-bit
;; @version 1.08 - help text corrections
;; @version 1.09 - added lib path for Windows 7
;; @version 1.10 - added SHA256
;; @version 1.11 - added  path for UBUNTU Linux 13.04
;; @version 1.12 - added  path for UBUNTU Linux 12.04 and CentOS, removed old
;; @version 1.13 - fix for crypto:hmac. Thanks Cormullion, March 2014
;; @author Lutz Mueller 2007, Martin Quiroga 2007, Norman Deppenbroek 2009, 
;; @author Marc Hildman, 2011
;;
;; <h2>Module for SSL lib crypto  bindings</h2>
;; This modules imports functions for the MD5 and SHA-1 hashing algorithms described
;; here: @link http://www.ietf.org/rfc/rfc3174.txt http://www.ietf.org/rfc/rfc3174.txt .
;; The crypto library is part of the @link http://www.openssl.org/ OpenSSL libraries.
;; 
;; To use this module include the following 'load' statement at the
;; beginning of the program file:
;; <pre>
;; (load "/usr/share/newlisp/modules/crypto.lsp")
;; ; or as a shorter alternative
;; (module "crypto.lsp")
;; </pre>
;; <h2>Requirements:</h2> 
;; On Mac OS X, UBUNTU and many other Linux, BSDs and other UNIX installations
;; <tt>libcrypto.so</tt> is installed by default as part of the OpenSSL
;; libraries in <tt>usr/lib/libcrypto.so</tt>. If loading this module 
;; finishes with an error message the path of the library should be corrected.
;; For MS Windows a library is available at 
;; @link http://gnuwin32.sourceforge.net/ http://gnuwin32.sourceforge.net/ .
;; 
;; This module has been tested on Mac OS X, UBUNTU Linux and FreeBSD.

(context 'crypto)

; set library to path-name of the library on your platform OS
;
(set 'files '(
              "C:/Program Files/gnuwin32/bin/libeay32.dll" ; XP
              "C:/Program Files (x86)/gnuwin32/bin/libeay32.dll" ; 7
              "/usr/lib/x86_64-linux-gnu/libcrypto.so" ; Ubuntu 12.04 LTS
              "/usr/lib/i386-linux-gnu/libcrypto.so"; Ubuntu 12.04
              "/lib/i386-linux-gnu/libcrypto.so.1.0.0" ; UBUNTU Linux 13.04
              "/usr/lib64/libcrypto.so" ; Fedora, CentOS 6.x
              "/usr/lib/libcrypto.so"
              "/usr/lib/libcrypto.so.4"
              "/usr/lib/libcrypto.so.18.0" ; OpenBSD 4.6
              "/usr/lib/libcrypto.so.19.0" ; OpenBSD 5.0
              "/usr/lib/libcrypto.dylib"
              ))

(set 'library (files (or
                      (find true (map file? files))
                      (throw-error "cannot find crypto library"))))

(import library "MD5")
(import library "RIPEMD160")
(import library "SHA1")
(import library "SHA256")

;; @syntax (crypto:md5 <string> <bool-raw>)
;; @param <string> The string buffer for which to calculate a MD5 hash
;; @param <bool-raw> Return the raw binay buffer when 'true'.
;; @return The 16 Byte MD5 hash as a 32 Byte long hex string or as a 16 byte binary buffer.
;; @example
;; (crypto:md5 "ABC") => "902fbdd2b1df0c4f70b4a5d23525e932"
;;
;; (crypto:md5 (read-file "newlisp-9.1.0.tgz")) => "46c79c93e904df35c6a8474ace406c92"

(define (md5 str raw-flag)
  (if raw-flag
      (let (buff (dup "\000" 16))
        (cpymem (MD5 str (length str) 0) buff 16)
        buff)
    (join
     (map (lambda (x) (format "%02x" (& x 0xff))) 
          (unpack (dup "c" 16) (MD5 str (length str) 0))))
    )
  )

;; @syntax (crypto:sha1 <string> <bool-raw>)
;; @param <string> The string buffer for which to calculate a SHA-1 hash
;; @param <bool-raw> Return the raw binay buffer when 'true'.
;; @return The 20 Byte SHA-1 hash as a 40 Byte long hex string or as a 20 byte binary buffer.
;; @example
;; (crypto:sha1 "ABC") => "3c01bdbb26f358bab27f267924aa2c9a03fcfdb8"
;;
;; (crypto:sha1 (read-file "newlisp-9.1.0.tgz")) => "2127a9c487f338b00f36cfd60b5f33d27b8d0010"

(define (sha1 str raw-flag)
  (if raw-flag
      (let (buff (dup "\000" 20))
        (cpymem (SHA1 str (length str) 0) buff 20)
        buff)
    (join
     (map (lambda (x) (format "%02x" (& x 0xff))) 
          (unpack (dup "c" 20) (SHA1 str (length str) 0)))
     )
    )
  )

;; @syntax (crypto:sha256 <string> <bool-raw>)
;; @param <string> The string buffer for which to calculate a SHA-256 hash
;; @param <bool-raw> Return the raw binay buffer when 'true'.
;; @return The 32 Byte SHA-1 hash as a 64 Byte long hex string or as a 32 byte binary buffer.
;; @example
;; (crypto:sha256 "ABC") => "b5d4045c3f466fa91fe2cc6abe79232a1a57cdf104f7a26e716e0a1e2789df78" 
;;
(define (sha256 str raw-flag)
  (if raw-flag
      (let (buff (dup "\000" 32))
        (cpymem (SHA256 str (length str) 0) buff 32)
        buff)
    (join
     (map (lambda (x) (format "%02x" (& x 0xff))) 
          (unpack (dup "c" 32) (SHA256 str (length str) 0)))
     )
    )
  )


;; @syntax (crypto:hmac <func-hash> <str-message> <str-key>)
;; @param <func-hash> The hash function to use.
;; @param <str-message> The message to encrypt.
;; @param <str-key> The encryption key.
;;
;; This function is part of the amazon.com
;; @link http://docs.amazonwebservices.com/AWSEC2/2007-08-29/DeveloperGuide/using-query-api.html Query-API
;; and based on @link http://www.faqs.org/rfcs/rfc2104.html RFC2104 - HMAC: Keyed-Hashing for Message Authentication.
;;
;; @example
;; (set 'output (crypto:hmac crypto:md5 "Hello World" "secret"))
;; (unpack (dup "c" (length output)) output)
;; => (107 59 -76 66 117 -119 -35 -31 -7 -121 90 55 -109 -68 32 98)

(define (hmac hash_fn msg_str key_str , blocksize opad ipad)
  (set 'blocksize 64)
  (set 'opad (dup "\x5c" blocksize))
  (set 'ipad (dup "\x36" blocksize))
  (if (> (length key_str) blocksize)
      ;; (set 'key_str (get-true-str (hash_fn key_str))) 
      (set 'key_str (hash_fn key_str true))
    )
  (set 'key_str (append key_str (dup "\000" (- blocksize (length key_str))))) ;; padding key with binary zeros
  (set 'opad (encrypt opad key_str))
  (set 'ipad (encrypt ipad key_str))
  (hash_fn (append opad (hash_fn (append ipad msg_str) true)) true)
  )


;; @syntax (crypto:ripemd160 <string> <bool-raw>) 
;; @param <string> The string buffer for which to calculate a RIPEMD160 hash 
;; @param <bool-raw> Return the raw binay buffer when 'true'. 
;; @return The 20 Byte RIPEMD160 hash as a 40 Byte long hex string or as a 20 byte binary buffer. 
;; @example 
;; (crypto:ripemd160 "ABC") => "df62d400e51d3582d53c2d89cfeb6e10d32a3ca6" 
;; 
;; (crypto:ripemd160 (read-file "newlisp.exe")) => "9c1185a5c5e9fc54612808977ee8f548b2258d31" 

(define (ripemd160 str raw-flag) 
  (if raw-flag 
    (let (buff (dup "\000" 20)) 
      (cpymem (RIPEMD160 str (length str) 0) buff 20) 
      buff) 
    (join 
     (map (lambda (x) (format "%02x" (& x 0xff))) 
          (unpack (dup "c" 20) (RIPEMD160 str (length str) 0))) 
     ) 
    ) 
  )

; eof ;
