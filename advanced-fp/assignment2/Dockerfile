FROM haskell:8.6.5

WORKDIR /usr/src/app

COPY stack.yaml stack.yaml
RUN stack setup

COPY . .
RUN stack build

CMD stack exec logo