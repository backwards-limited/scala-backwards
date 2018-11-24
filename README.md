# Scala by Backwards

- Functional Programming: Custom code for Category Theory

- FS2: Functional Streaming for Scala

- Variance: Proofs

- Cats: TODO

## Setup

Apologies I shall only cover **Mac** - One day I may include Linux and Windows.

Install [Homebrew](https://brew.sh) for easy package management on Mac:

```bash
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Installation essentials:

```bash
brew update
brew install scala
brew install sbt
```

## SBT

To check for updated dependencies:

```bash
sbt dependencyUpdates
```

## Release

Currently released versions of this module can be added as a dependency from [JitPack](https://jitpack.io).

```bash
$ sbt "release with-defaults"
```