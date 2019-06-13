FROM fpco/stack-build:lts-12.10 as build
RUN stack upgrade
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc --copy-bins --local-bin-path /opt/bin

FROM snoyberg/haskellers-build-image:e17739d1c2c043aae11924fee66c9ee4304ad37d
RUN mkdir -p /app
WORKDIR /app
COPY --from=build /opt/bin/haskellers /usr/local/bin/haskellers
COPY --from=build /opt/build/static /app/static
COPY --from=build /opt/build/config /app/config
CMD ["/usr/local/bin/haskellers"]
