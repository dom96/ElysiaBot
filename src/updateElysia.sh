#!/bin/sh
git pull origin master

mv elysia elysiaBCK

ghc --make elysia.hs

if [ -f "elysia" ]; then
  echo "Success!"
  rm elysiaBCK
  echo "Executing ElysiaBot"

  ./elysia
else
  echo "!!-Error-!! Compilation failed."
fi
