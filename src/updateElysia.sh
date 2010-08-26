#!/bin/sh

if [ ! -d "ElysiaBot" ]; then
  git clone git://github.com/dom96/ElysiaBot.git >> update.log 2>&1
  cd ElysiaBot
else
  cd ElysiaBot
  git pull origin master >> ../update.log 2>&1
fi

cd src
ghc --make elysia.hs >> ../../update.log 2>&1

if [ -f "elysia" ]; then
  cp elysia ../../elysia >> update.log 2>&1
  echo "Success!"
  echo "Killing ElysiaBot just in case"
  pkill elysia
  cd ..
  cd ..
  echo "Executing ElysiaBot"
  ./elysia
else
  echo "!!-Error-!!< Compilation failed."
fi
