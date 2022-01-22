# Setup

Apologies I shall only cover **Mac** - One day I may include Linux and Windows.

Install [Homebrew](https://brew.sh) for easy package management on Mac:

```bash
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

## Required installations

**Scala and Java:**

```shell
brew tap adoptopenjdk/openjdk

brew search jdk
# Gives a list of JDKs we can install (we'll install version 15, the latest at this current time)

brew cask install adoptopenjdk15
```

```shell
brew install jenv
brew install scala
brew install sbt
```

**Configure Java environment:**

To manage your Java environment, let's use **jenv** (there are other managers out there). We add whatever Java versions we wish to manage:

```shell
jenv add /Library/Java/JavaVirtualMachines/adoptopenjdk-15.jdk/Contents/Home/
openjdk64-15 added
15 added
```

and then choose one of the managed versions to use with our code:

```shell
jenv versions
  system
  1.8.0.202
  15
  openjdk64-15
  oracle64-1.8.0.202

jenv local 15
```

## Optional Installations

```shell
brew install curl
brew install httpie

brew tap anchore/grype
brew install grype
```

The last installation is to scan your project for vulnerabilities:
```shell
grype dir:.
```

However, for this `sbt` project [sbt-dependency-check](https://github.com/albuch/sbt-dependency-check) is installed, so instead of relying on `grype` run:
```shell
sbt dependencyCheck
```

And you should double check for any secrets you may have checked into Git:
```shell
git secrets scan
```