leprosorium-captcha-recognizer
==============================

Leprosorium.ru captcha recognizer.

How to build:

1. Install Haskell Platform https://www.haskell.org/platform/
2. `git clone ... && cd ...`
3. `cabal sandbox init`
4. `cabal install`

Usage:

```
./.cabal-sandbox/bin/leprosorium-captcha-recognizer +RTS -N -RTS ./data/captcha2_qkxzko.png
```

For more details see:

* https://eax.me/captcha-recognition/
* https://eax.me/haskell-genetic-algorithm/
* https://eax.me/haskell-neural-networks/
