#!/bin/bash

# this script is only intended for local debugging of optimized build process served via nginx
# refer to .cloudbuild/ directory for build/deployment scripts used by CloudBuild

docker build \
    -t site-nginx:local \
    .
