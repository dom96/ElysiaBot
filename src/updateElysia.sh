#!/bin/sh
rm update.log

echo "Killing ElysiaBot just in case"
pkill elysia

git pull origin master >> update.log 2>&1

rm elysia

ghc -fforce-recomp --make elysia.hs >> update.log 2>&1

if [ -f "elysia" ]; then
  echo "Success!"
  echo "Executing ElysiaBot"
  ./elysia
else
  echo "!!-Error-!!< Compilation failed."
fi
