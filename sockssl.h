#ifndef _SOCKSSL_H
#define _SOCKSSL_H

#ifndef SSL_DEBUG
#define SSL_DEBUG
#endif

/* FIXME: avoid conflicting types for 'UUID' when #include <openssl/ssl.h> */
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#endif

#ifdef HAVE_OPENSSL
//#include <openssl/crypto.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

#define ssl_do_connect(socket)          SSL_get_error(socket->ssl, SSL_connect(socket->ssl))
#define ssl_do_write(socket, data, len) SSL_write(socket->ssl, data, len)
#define ssl_do_read(socket, data, len)  SSL_read(socket->ssl, data, len)
#define ssl_do_close(socket)            SSL_shutdown(socket->ssl)

#define CERTFILE "/usr/local/etc/openssl/cert.pem"
#endif

//typedef struct socket socket_t;
struct socket {
  int fd;
#ifdef HAVE_OPENSSL
  void *ssl;
  //unsigned need_ssl:1;
#endif
};

struct ssl_option {
  char *sslcert;
  char *sslkey;
  int sslverify;
  char *ssltrustfile;
  char *sslchiphers;
  char *sslalpn;
};

struct socket *socket_new(int fd, int need_ssl);
void socket_close(struct socket *);
int socket_read(struct socket *, void *, int);
int socket_write(struct socket *, void *, int);

#endif  /* _SOCKSSL_H */
