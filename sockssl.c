#include <unistd.h>
#include <sys/socket.h>
#include "sockssl.h"

#define CERTFILE "/usr/local/etc/openssl/cert.pem"

#ifdef USE_OPENSSL
#define ssl_do_connect(socket)          SSL_get_error(socket->ssl, SSL_connect(socket->ssl))
#define ssl_do_write(socket, data, len) SSL_write(socket->ssl, data, len)
#define ssl_do_read(socket, data, len)  SSL_read(socket->ssl, data, len)
#define ssl_do_close(socket)            SSL_shutdown(socket->ssl)
#endif

/* SSL Context */

static SSL_CTX *context = NULL;

static void ssl_ctx_init() {
  const SSL_METHOD *method;
  if (context) return;

  SSL_library_init();
  SSL_load_error_strings();

  method = SSLv23_method();
  context = SSL_CTX_new(method);
  if (!context) {
    perror("Unable to create SSL context");
  }

  /* configure */
  SSL_CTX_set_mode(context, SSL_MODE_AUTO_RETRY);
  SSL_CTX_set_options(context, SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3);
  SSL_CTX_set_default_verify_paths(context);
  //SSL_CTX_set_verify(context, SSL_VERIFY_NONE, NULL);
  //ssl_CTX_set_verify_depth(context, 0);
}

static void ssl_ctx_free() {
  if (!context) return;
  ERR_free_strings();
  SSL_CTX_free(context);
  context = NULL;
}

/* SSL Method */

int ssl_connect(struct socket *s) {
  s->ssl = SSL_new(context);
  SSL_set_fd(s->ssl, s->fd);
  SSL_set_connect_state(s->ssl); // swich client mode

  return SSL_connect(s->ssl);
}

void ssl_close(struct socket *s) {
  SSL_shutdown(s->ssl);
  SSL_free(s->ssl);
  s->ssl = NULL;
}

/* Socket */

struct socket *socket_new(int fd, int need_ssl) {
  struct socket *s;

  if ((s = malloc(sizeof(*s))) == NULL) {
    return NULL;
  }
  s->fd  = fd;
  s->need_ssl = need_ssl;

  if (s->need_ssl) {
    ssl_ctx_init();
  }
  return s;
}

void socket_close(struct socket *s) {
  if (s->need_ssl) {
    ssl_ctx_free();      
  }
  close(s->fd);
  free(s);  
}
