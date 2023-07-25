# bcc-cli tests

This tree contains tests for the `bcc-cli` executable.

The whole `bcc-cli` test suite can be run from the top level directory of the `bcc-node`
repository using:

```
cabal test bcc-cli:bcc-cli-test
```

Individual tests are just shell scripts and as long as `bcc-cli` has been built, individual
tests can be run like for instance:
```
bcc-cli/test/cli/version/run
```

All tests should work both within the Haskell test runner and as standalone scripts.

Currently these tests do not run on Windows.

# Writing CLI tests

New tests can be added, by copying the directory for an existing test (eg `bcc-cli/test/cli/version`)
as a template to a new directory and modifying the `run` shell script in the new directory as needed.

When a new directory is added, it will be found by the `bcc-cli-test` test runner (written in
Haskell) and run.

New test scripts should be minimal and should be [shellcheck][shellcheck] clean and all
functionality that is common between scripts should be moved to the library file
`bcc-cli/test/cli/core/common`.

Scripts that need input and expected output data can store that data in the same directory as the
test script itself.


[shellcheck]: https://github.com/koalaman/shellcheck/wiki
