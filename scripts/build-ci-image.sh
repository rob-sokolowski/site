#!/bin/bash

# this script is intended for local debugging of build processes, see the .cloudbuild/ directory of this project
# for the actual build scripts


# tagging to gcr labeling for local testing.. don't push, let CloudBuild handle that!
docker build \
    -t gcr.io/fir-sandbox-326008/site-ci-builder:latest \
    --file Dockerfile-Ci \
    .
