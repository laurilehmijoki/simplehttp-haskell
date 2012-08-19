# Simple HTTP server

Useful for serving HTML documents from localhost.

## Setup

Install <http://www.haskell.org/ghc/>.

## Run

Run the following command in the directory where you have your HTML files:

`runhaskell Main.hs 9999`

This will launch you an HTTP server on port 9999.

## Compile

`ghc -o simplehttp \`ls \*.hs\``

Now you can run the compiled binary with `simplehttp 9999`.
