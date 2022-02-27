# Articles

- https://byorgey.wordpress.com/2019/04/30/code-style-and-moral-absolutes/
- https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/
# How to use ormolu

```
$ stack install ormolu
$ ormolu --mode inplace $(git ls-files '*.hs')
```

To check if files are are already formatted:
```
`$ ormolu --mode check $(find . -name '*.hs')
```
# TODO
- [x] use properly the `package.yaml` file instead of always updating the `.cabal` file
- [x] introduce [tasty](https://hackage.haskell.org/package/tasty)
- [x] introduce [doctest](https://hackage.haskell.org/package/doctest)
- [ ] introduce [criterion](https://hackage.haskell.org/package/criterion)
- [ ] use [Haskell language server](https://github.com/haskell/haskell-language-server)
- [x] use [ormolu](https://hackage.haskell.org/package/ormolu)
- [x] add ci based on Github Actions

- [ ] introduce [ReadP](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-ParserCombinators-ReadP.html) for parsing
- [ ] or introduce [MegaParsec](https://hackage.haskell.org/package/megaparsec) for parsing
- [ ] introduce [orthotope](https://hackage.haskell.org/package/orthotope) for situations when a multidimensional array is needed
