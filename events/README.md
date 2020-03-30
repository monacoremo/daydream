
To update project.nix, run in nix-shell:

```bash
cabal2nix . > project.nix
```

To build the package, run:

```bash
nix-build release.nix
```
