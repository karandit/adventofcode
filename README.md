# Articles

- https://byorgey.wordpress.com/2019/04/30/code-style-and-moral-absolutes/
- https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/

# How to install Haskell Language Server

Install [prebuilt binaries](https://github.com/haskell/haskell-language-server/releases) and put them in the PATH
```
curl -O https://github.com/haskell/haskell-language-server/releases/download/1.6.1.0/haskell-language-server-wrapper-Linux.gz
curl -O https://github.com/haskell/haskell-language-server/releases/download/1.6.1.0/haskell-language-server-Linux-8.8.4.gz
```
Install [coc.nvim](https://github.com/neoclide/coc.nvim) as HLS client

Install nodejs >= 12.12:
```
curl -sL install-node.vercel.app/lts | bash
```

Add coc.nvim via vim-plug
```
Plug 'neoclide/coc.nvim', {'branch': 'release'}
```
and install it with `:PlugInstall`

Add HLS to coc by editing coc's configuration `:CocConfig`
```
"languageserver": {
  "haskell": {
    "command": "haskell-language-server-wrapper",
    "args": ["--lsp"],
    "rootPatterns": ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"],
    "filetypes": ["haskell", "lhaskell"]
  }
}
```

# How to use ormolu

```
$ stack install ormolu
$ ormolu --mode inplace $(git ls-files '*.hs')
```

To check if files are already formatted:
```
$ ormolu --mode check $(git ls-files '*.hs')
```

# TODO
- [x] use properly the `package.yaml` file instead of always updating the `.cabal` file
- [x] introduce [tasty](https://hackage.haskell.org/package/tasty)
- [x] introduce [doctest](https://hackage.haskell.org/package/doctest)
- [ ] ~~introduce [criterion](https://hackage.haskell.org/package/criterion)~~ *Tasty is good enough to display the duration of each test*
- [x] use [Haskell Language Server](https://github.com/haskell/haskell-language-server)
- [x] use [ormolu](https://hackage.haskell.org/package/ormolu)
- [x] add ci based on Github Actions

- [ ] introduce [ReadP](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-ParserCombinators-ReadP.html) for parsing
- [ ] or introduce [MegaParsec](https://hackage.haskell.org/package/megaparsec) for parsing
- [ ] introduce [orthotope](https://hackage.haskell.org/package/orthotope) for situations when a multidimensional array is needed
