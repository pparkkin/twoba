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

## Run in AWS

```
$ terraform apply

An execution plan has been generated and is shown below.
...
twoba-name = <instance dns name>
```

Open http://<instance dns name>:33000/ in a browser.

