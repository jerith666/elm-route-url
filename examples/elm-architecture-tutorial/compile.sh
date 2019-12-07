#! /bin/sh

mkdir -p build
elm make MainWithFullUrl.elm --output build/MainWithFullUrl.html
elm make MainWithJustHash.elm --output build/MainWithJustHash.html
elm make MainWithOldAPI.elm --output build/MainWithOldAPI.html
