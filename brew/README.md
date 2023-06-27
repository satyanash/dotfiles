# Brew

Only need `brew` to bootstrap. Install that from the GitHub readme.
To create a list of installed packages, casks and taps, run:

```shell
brew bundle dump
```

This should create a `Brewfile` in the current directory.

Then on another machine, run:

```shell
brew bundle install
```

Then go grab a coffee!
