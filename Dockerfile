ARG SCHEME=chibi
FROM schemers/${SCHEME}:head
RUN apt-get update && apt-get install -y make gauche git ca-certificates
RUN git clone https://codeberg.org/retropikzel/compile-scheme.git --depth=1
WORKDIR /compile-scheme
RUN make build-gauche
RUN make install
WORKDIR /workdir