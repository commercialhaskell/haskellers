FROM ubuntu:14.04

RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y libpq5 libgmp10 git libicu52 libyaml-0-2

ADD app /app
WORKDIR /app

ENTRYPOINT ./haskellers production
EXPOSE 3000
