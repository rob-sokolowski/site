#!/bin/bash

elm-spa build

docker build \
    -t site-nginx:latest \
    .
