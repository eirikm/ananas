#!/usr/bin/env bash

set -x

(cd frontend && elm-app build) \
    && rm -r backend/src/main/resources/elm-dist \
    && mv frontend/dist backend/src/main/resources/elm-dist
