(setq url "https://example.com/")

(when (starts-with (main-args -1) "https?://" "x")
  (setq url (main-args -1)))

(println "Testing URL: " url)
(println (get-url url "debug list"))
(exit)

;; https://securitytraning.com/
;; TLS 1.2 connection using TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256
;; sslv3 alert handshake failure
;; -> require :cipher option

;; https://httpbin.org/ip
;; tlsv1 alert internal error

;; openssl s_server -key key.pem -cert cert.pem -accept 44330 -www
;; https://localhost:44330
;; dh key too small

;; https://dic.nicovideo.jp
;; redirect HTTPS to HTTP

;; https://thepiratebay.gd/
;; -> 301 http://thepiratebay.gd/
;; -> 307 https://thepiratebay.org/
