# Haskell Universal Config Manager

This package helps updating config files. It aims to support every
config file syntax and when applicable provides semantic updates.

It provides a CLI executable so it can be easily integrated in bash
scripts, for example. It also provides a library to allow users to
create more abstract config changes using the library.

# Building

Building and running the app is done with:

```
stack build --haddock --haddock-deps
stack run
```

The haddock arguments are used to generate the documentation.

Running the tests is done with:

```
stack test
```

Looking at the documentation is best done through using hoogle, to do
that, run:

```
stack hoogle -- server --local --port=65000
```

Then point your browser at http://localhost:65000.

## Github Action

This template project includes a github action workflow which builds
and tests the project as well as uploading the binary as a github
workflow artifact.

To download it, follow these steps:
1. Go to the latest github action run
2. Download the artifact.
3. Unzip it with `unzip hs-template-project.zip`
4. set the executable permission bit with `chmod a+x hs-template-project/hs-template-project`
5. run it with `./hs-template-project/hs-template-project` and you should see printed `"Hello World"`.
