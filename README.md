# Haskell Reverse HTTP Proxy

A dead simple reverse HTTP proxy. Not intended for production usage, just a learning experience in Haskell.

### Getting started

Fire up the server, ensuring that you have a backend server running on port 3000 (currently hardcoded)

```
stack build
stack exec proxy-exe 4000
```

and fire a HTTP request at it.

```
curl http://localhost:4000/example/path
```

### Dependencies

Currently has no HTTP-specific dependencies:

 - network
 - attoparsec
 - bytestring
 - HUnit (for testing)

### Future features

To be added in no particular order (if I feel like it)

 - Config files - apache nginx compatible is a good starting point for features
 - Benchmarking - compare to mighty to see how much room for improvement there is (http://www.mew.org/~kazu/proj/mighttpd/en/)
 - Multiple backend servers with load balancing algorithm
 - Logging
 - Header replacement
 - Timeouts (send back 504)
