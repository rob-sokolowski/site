#!/bin/bash

# this script is intended for local debugging of build processes, see the .cloudbuild/ directory of this project
# for the actual build scripts

docker build \
    -t site-ci-build-image:local-only \
    --file Dockerfile-Ci \
    .
