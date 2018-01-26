# TWOBA

## Build and Run Locally
```
$ stack build
$ stack exec twoba-server
```

Open http://localhost:33000/ in a browser.

## Build and Run Inside a Docker Container

```
$ docker build -t twoba .
$ docker run --rm -t -p=33000:33000 twoba
```

Open http://localhost:33000/ in a browser.


