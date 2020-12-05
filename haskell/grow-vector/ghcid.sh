if [ -z "$@" ]
then
  ARGS=""
else
  ARGS="\"$@\""
fi
ghcid -c "cabal new-repl $ARGS"
