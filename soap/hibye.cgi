  #!perl -w

  use SOAP::Transport::HTTP;

  SOAP::Transport::HTTP::CGI   
    -> dispatch_to('Demo')     
    -> handle;

  package Demo;

  sub hi {                     
    return "hello, world";     
  }

  sub bye {                    
    return "goodbye, cruel world";
  }

