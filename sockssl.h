#ifndef NEWLISP_SSL_H
#define NEWLISP_SSL_H

/* TODO
 * - rename socket.h or ssl.h
 * - work in server mode, etc
 */

#define USE_OPENSSL

#include <assert.h>
#include <openssl/crypto.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/bio.h>

//typedef struct socket socket_t;
struct socket {
  int fd;
  void *ssl;
  unsigned int need_ssl:1;
};

struct socket *socket_new(int fd, int need_ssl);
void socket_close(struct socket *);

int ssl_connect(struct socket *);
void ssl_close(struct socket *);

#endif  /* NEWLISP_SSL_H */
