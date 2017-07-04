# Haskell Reverse HTTP Proxy

A learning experience.

### Getting started

Fire up the server,

```
stack build
stack exec proxy-exe 4000
```

and fire a HTTP request at it.

```
curl http://localhost:4000/example/path
```

