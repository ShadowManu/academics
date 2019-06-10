FROM haskell:8
RUN stack setup
RUN stack update

WORKDIR /usr/src/app
COPY . .
RUN stack build

CMD stack exec haskinator