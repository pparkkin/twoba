# TWOBA

## Build and Run Locally
```
$ stack build
$ stack exec twoba-server
```

Open http://localhost:33000/game/ in a browser.

## Build and Run Inside a Docker Container

```
$ docker build -t twoba .
$ docker run --rm -t -p=33000:33000 twoba
```

Open http://localhost:33000/game/ in a browser.

## Run in AWS

```
$ terraform apply

An execution plan has been generated and is shown below.
...
lb-name = <instance dns name>
```

Open http://&lt;lb-name&gt;/game/ in a browser.

