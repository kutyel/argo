# argo

[![CI Status](https://github.com/kutyel/argo/workflows/CI/badge.svg)](https://github.com/kutyel/argo/actions)

📇 Zero-dep JSON parser with Haskell and Nix

## Try it out!

```bash
$ nix-shell --pure shell.nix --run "cabal repl"
```

### Known Issues (TODO's)

- [ ] Support for Float (Scientific numbers  [IEEE 754-1985](https://en.m.wikipedia.org/wiki/Double-precision_floating-point_format))
- [ ] Support parsing errors (`Either` instead of `Maybe`)
- [ ] Support escaped strings
