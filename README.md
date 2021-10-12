# haskell-gitpod-template

This setup installs all the [ghcup](https://www.haskell.org/ghcup/) components (ghc, cabal, stack, hls) using the recommended version,
exposes the installed version of `ghc`, and globally sets up `stack` to use the system ghc.
(As ghcup says, you should not use `stack upgrade` to upgrade stack (use `ghcup install stack <newer_version>` instead), but you won't normally need it anyway.)

## New stack project instructions

* Install `Haskell Syntax Highlighting` and `Haskell` VS Code extensions
* `stack new yourproject`
* `cd yourproject`
* `stack build`
* `gen-hie > hie.yaml` (uses [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) to tell the project structure to HLS)
