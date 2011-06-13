ObjectiveJ&#955;ib
================================

ObjectiveJ parsing and printing library for Haskell.

Features:

- Fast parser
- Full syntax parsing (including types, unescaping strings)
- Optional semicolons
- Nicely formatted, human readable, pretty printing output

Build instructions
------------------

1.  Make sure all dependencies are present:

    `cabal install uuagc parsec array pretty containers`

2.  Build ObjectiveJ&#955;ib:

    `runhaskell Setup.hs configure`

    `runhaskell Setup.hs build`

3.  Install ObjectiveJ&#955;ib:

    `runhaskell Setup.hs install`
