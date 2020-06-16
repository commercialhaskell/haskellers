FROM snoyberg/haskellers-build-image:e17739d1c2c043aae11924fee66c9ee4304ad37d as build-app

RUN mkdir -p /artifacts/bin
COPY . /src
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM snoyberg/haskellers-build-image:e17739d1c2c043aae11924fee66c9ee4304ad37d
RUN mkdir -p /app

WORKDIR /app
CMD ["/usr/local/bin/haskellers"]
ENV PORT 3000

COPY --from=build-app /artifacts/bin/haskellers /usr/local/bin
COPY --from=build-app /src/static /app/static
COPY --from=build-app /src/config /app/config
