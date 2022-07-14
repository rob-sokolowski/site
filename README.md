Code that powers my website, [robsoko.tech](http://robsoko.tech)


## Local dev
Assuming reasonably up-to-date node and npm, you should be good to run the dev server on your local machine without Docker.

Install elm prerequisites:
 ```shell script
npm install -g elm elm-test elm-review elm-spa
```

Clone this repo, and `cd` into it. Then generate types used by elm-spa Pages code:
```shell script
elm-spa gen
```

Run local dev server:
```shell script
elm-spa server
```

elm-spa owns client-side routing, and build steps for deployment. Refer to [elm-spa's guide](https://www.elm-spa.dev/guide) for more information.

I usually open another console to run `elm-test`
```shell script
elm-test --watch
```


#### Local nginx debugging
To mimic the build process used by the deployment script do the following:
```shell script
./scripts/build.sh
```

To run the nginx server:
```shell script
docker-compose up site
```

You should be able to view the site in your browser at `localhost` (implicit port 80).
If something is off you can run a `sh` shell (not `bash`, alpine is tiny!) by `docker exec`-ing into the running container


## Deployment

This app runs on GCP CloudRun, which is deployed to via CloudBuild triggers. All CloudBuild related code is located in `.cloudbuild/`.
In order to keep CloudRun wakeup time quick, an nginx alpine image is used to serve the compiled code. This 
isn't necessary for the current deployment, but I am anticipating need for more complex deployments soon.


Triggers:
 - `pr-build`: triggered on all PRs issued in this repo
 - `deploy-to-cloud-run`: triggered on all cut tags for this repo  
