# Haskell Stack commands

## Get `ghc` version installed via stack

```sh
stack ghc -- --numeric-version
```

## Haskell stack `ghc` binary path

```sh
stack path | grep compiler-bin | sed -e 's/compiler-bin: //'
```

## Haskell stack installed programs

```sh
ls $(stack path --programs)/*.installed
```

## List all the stack installed packages

We may list all the installed packages from stack with the `ghc-pkg list` command
as follows:

```sh
echo "   package,version"
stack exec ghc-pkg -- list | grep -v -e '/' -e "^$" -e "(no packages)" | sort | \
  sed 's/-\([0-9]\)/,\1/'
```

## Build a `haskell` file using the `make`

A standards `haskell` file with `.hs` or `.lhs` may be built using the below:

```sh
stack ghc -- --make Abc.lhs
```
