/* UDP sockets. */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>

typedef struct socket_s
{
  int handle;
  struct sockaddr_in address; /* TODO unused */
} socket_t;

/* return a socket_t * on success, 0 on error */
socket_t *
socket_open (unsigned short port)
{
  socket_t * s = (socket_t *) malloc (sizeof(socket_t));
  s->handle = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);

  if (s->handle <= 0)
  {
    free (s);
    return 0;
  }

  struct sockaddr_in address;
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = INADDR_ANY; /* TODO select an appropriate address */
  address.sin_port = htons ((unsigned short) port);

  if (bind (s->handle, (const struct sockaddr*) &address,
    sizeof(struct sockaddr_in)) < 0)
  {
    free (s);
    return 0;
  }

  /* Set the socket in non-blocking mode. */
  if (fcntl (s->handle, F_SETFL, O_NONBLOCK, 1) == -1)
  {
    free (s);
    return 0;
  }

  return s;
}

/* return 1 on success, 0 on error */
int
socket_send (
  socket_t * s,
  unsigned char a,
  unsigned char b,
  unsigned char c,
  unsigned char d,
  unsigned short port,
  void * data,
  int data_size)
{
  unsigned int ip = ( a << 24 ) | ( b << 16 ) | ( c << 8 ) | d;

  struct sockaddr_in address;
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl (ip);
  address.sin_port = htons (port);

  int sent_bytes = sendto (s->handle, (const char*)data, data_size,
    0, (const struct sockaddr*)&address,
    sizeof(struct sockaddr_in));

  if (sent_bytes != data_size)
    return 0;

  return 1;
}


/* return the number of received bytes. */
int
socket_receive (
  socket_t * s,
  unsigned char * a,
  unsigned char * b,
  unsigned char * c,
  unsigned char * d,
  unsigned short * port,
  void * data,
  int data_size)
{
    struct sockaddr_in address;
    socklen_t address_length = sizeof(address);
    int received_bytes = recvfrom (s->handle, (char*)data, data_size,
                    0, (struct sockaddr *)&address, &address_length);

    if (received_bytes <= 0)
      return 0;

    unsigned int ip = ntohl (address.sin_addr.s_addr);
    *port = ntohs (address.sin_port);

    *a = (unsigned char) (ip >> 24);
    *b = (unsigned char) (ip >> 16);
    *c = (unsigned char) (ip >>  8);
    *d = (unsigned char) ip;

    return received_bytes;
}

void
socket_close (socket_t * s)
{
  close (s->handle);
  free (s);
}

