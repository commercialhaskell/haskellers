#!/usr/bin/env bash

set -eux

TAG=$(git rev-parse --short HEAD)

rm -rf docker/app
mkdir -p docker/app
stack build
cp $(stack exec which haskellers) docker/app
cp -r static config docker/app

(
cd docker
docker build -t snoyberg/haskellers:$TAG .
docker push snoyberg/haskellers:$TAG
)
