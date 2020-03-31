The `.nix` files were generated with `cabal2nix`, e.g.:

```bash
cabal2nix https://github.com/folq/haskell-to-elm.git > haskell-to-elm.nix
```

`doCheck = false;` was added for individual packages where the test suite
failed due to an incomplete environment (e.g. PostgREST exprects a built
binary to execute query performance tests, which we don't need).
