## The Plan

- Implement a dead simple reverse HTTP proxy (similar to https://github.com/skx/node-reverse-proxy.js/blob/master/node-reverse-proxy.js).
- Use as few libraries as possible to get a feel for the standard library and language itself.
- No server frameworks allowed

## Features

### v1

- HTTP only
- Single backend server hardcoded in

### v2

- Configurable backend server via config file / cmd line args

`ProxyPass "/"  "http://www.example.com/"`

### v3

- Load balancing (BalancerMember etc)

## Resources
- http://www.catonmat.net/blog/simple-haskell-tcp-server/
- http://httpd.apache.org/docs/current/howto/reverse_proxy.html
- https://bitbucket.org/bos/attoparsec/src/tip/examples/RFC2616.hs?fileviewer=file-view-default
