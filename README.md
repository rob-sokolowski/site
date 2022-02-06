## site
code that powers my website, [robsoko.tech](http://robsoko.tech)




###testing flow
everything below assumes you are sitting in this directory, `ls` should list this file

dev is done via
```shell script
elm-spa server
```

to build:
```shell script
./scripts/build.sh
```

this builds the Docker image `site-nginx:latest`

to test that build locally:
```shell script
docker-compose up
```

full site should be available at `localhost` (implicit port `80`)

if things look good, ship it via
```shell script
./scripts/depoy-to-cloud-run.sh
```
