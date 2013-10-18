A haskell program that parses the bitcoin blockchain files

Currently reads from stdin and outputs to stdout.

But this could be changed to do something more interesting

Usage:

$> cabal-dev install
$> cat ~/.bitcoin/block/blk* | cabal-dev/bin/bitcoin-blockchain
