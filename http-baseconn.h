#ifndef HTTP_BASECONN_H
#define HTTP_BASECONN_H

class BaseConn {

public:

#define HEADERFIELD_MAXSIZE  256
  char method[HEADERFIELD_MAXSIZE];
  char uri[HEADERFIELD_MAXSIZE];
  char version[HEADERFIELD_MAXSIZE];
  int event_count;

  BaseConn();
  ~BaseConn();

protected:


};

#endif
