FROM fpco/stack-build:lts-19.19 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A4B469963BF863CC
# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-19.19 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:20.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .
CMD ["/opt/app/app"]
