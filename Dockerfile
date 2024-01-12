FROM snoyberg/haskellers-build-image:e17739d1c2c043aae11924fee66c9ee4304ad37d as build-app

RUN mkdir -p /artifacts/bin
COPY . /src
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM snoyberg/haskellers-build-image:e17739d1c2c043aae11924fee66c9ee4304ad37d
RUN mkdir -p /app

COPY --from=build-app /artifacts/bin/haskellers /usr/local/bin
COPY --from=build-app /src/static /app/static
COPY --from=build-app /src/config /app/config

WORKDIR /app

COPY ./run.sh /app/run.sh

ADD https://github.com/fpco/pid1-rs/releases/download/v0.1.0/pid1-x86_64-unknown-linux-musl /usr/bin/pid1
RUN chmod +x /usr/bin/pid1

ADD https://github.com/fpco/amber/releases/download/v0.1.5/amber-x86_64-unknown-linux-musl /usr/bin/amber
RUN chmod +x /usr/bin/amber

ENTRYPOINT [ "pid1" ]

CMD ["/app/run.sh"]

ENV PORT 3000
