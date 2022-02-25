# HaskTTP-Server
HaskTTP-Server is a (hopefully) conditionally compliant HTTP/1.1 server for static content, similar in functionality to Python's `http.server` module.  The program does not support the full HTTP/1.1 specification, handling only GET and POST requests.  

[`attoparsec`][https://hackage.haskell.org/package/attoparsec] parser combinators are used for parsing incoming HTTP request headers.  Receiving this data is handled using TCP socket connections from the [`network`][https://hackage.haskell.org/package/network] library.  

## Socketeer
HaskTTP-Server is built using the included socketeer library, which provides TCP servers for arbitrary "Handlers," `ReaderT`/`WriterT` monad transformer stacks over `IO` which "handle" an individual connection.  Basic handlers for sending, receiving, logging, and parsing HTTP requests are provided.  The TCP servers in socketeer also expect a "Logger," a function that formats and prints the log that is accumulated while running a handler.  Loggers for writing to stdout and files are included, as well as generic Logger patterns to ease adding new Loggers for varied outputs.  

# Installation
HaskTTP-Server is a Cabal project.  To build the project, simply run:
```bash
cabal build
```
in the base directory.  For installation:
```bash
cabal install
```

If you would like to generate documentation for the associated socketeer library, the command:
```bash
cabal haddock
```
will generate the package's documentation as HTML.  This documentation can, if desired, be served for viewing by the web server itself.  
