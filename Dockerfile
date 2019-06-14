FROM snoyberg/haskellers-build-image:e17739d1c2c043aae11924fee66c9ee4304ad37d
RUN mkdir -p /app
WORKDIR /app
COPY etc/docker/_artifacts/haskellers /usr/local/bin/haskellers
COPY static /app/static
COPY config /app/config
CMD ["/usr/local/bin/haskellers"]
