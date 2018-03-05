/*
 * sockssl.c -- socket and SSL support function
 */

#include <unistd.h>
#include <stdlib.h>
#include <sys/socket.h>
#include "sockssl.h"

#ifdef HAVE_OPENSSL
/* SSL Context */

static SSL_CTX *context = NULL;

static void ssl_ctx_init()
{
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
  //SSL_CTX_set_verify(context, SSL_VERIFY_NONE, NULL);
  //SSL_CTX_set_verify_depth(context, 0);
  SSL_CTX_set_mode(context, SSL_MODE_AUTO_RETRY);
  SSL_CTX_set_options(context, SSL_OP_ALL | SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3);
  SSL_CTX_set_default_verify_paths(context);
}

static void ssl_ctx_free()
{
  if (context == NULL) return;
  ERR_free_strings();
  SSL_CTX_free(context);
  context = NULL;
}

/* SSL Method */

static int ssl_connect(struct socket *s)
{
  s->ssl = SSL_new(context);
  SSL_set_fd(s->ssl, s->fd);
  SSL_set_connect_state(s->ssl); // swich client mode

  return SSL_connect(s->ssl);
}

static void ssl_close(struct socket *s)
{
  SSL_shutdown(s->ssl);
  SSL_free(s->ssl);
  s->ssl = NULL;
}
#endif

/* Socket */

struct socket *socket_new(int fd, int need_ssl)
{
  struct socket *s;

  if ((s = malloc(sizeof(*s))) == NULL)
    return NULL;

  s->fd  = fd;

#ifdef HAVE_OPENSSL
  s->ssl = NULL;

  if (need_ssl) {
    ssl_ctx_init();
    if (ssl_connect(s) != 1) {
      ERR_print_errors_fp(stderr);
      socket_close(s);
      return NULL;
    }
#ifdef SSL_DEBUG
    fprintf(stderr, "SSL_version: %s\n", SSL_get_version(s->ssl));
    fprintf(stderr, "SSL_state:   %s\n", SSL_state_string(s->ssl));
#endif
  }
#endif

  return s;
}

void socket_close(struct socket *s)
{
#ifdef HAVE_OPENSSL
  if (s->ssl) {
    ssl_close(s);
    ssl_ctx_free();
  }
#endif
  close(s->fd);
  free(s);
}

int socket_read(struct socket *s, void *buf, int num) {
#ifdef HAVE_OPENSSL
  if (s->ssl)
    return SSL_read(s->ssl, buf, num);
#endif
  return recv(s->fd, buf, num, 0);
}

int socket_write(struct socket *s, void *buf, int num) {
#ifdef HAVE_OPENSSL
  if (s->ssl)
    return SSL_write(s->ssl, buf, num);
#endif
  return send(s->fd, buf, num, 0);  
}
