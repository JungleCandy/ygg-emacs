# Ygg-Emacs

Personal Emacs configuration

## Why

There comes a time when one needs to start again with one's Emacs configuration.

I've been a happy user of [Prelude](https://github.com/bbatsov/prelude) since I
started using Emacs, but it's large and supports languages I don't use and
doesn't support some of the ones I do. So I've declared Emacs Bankruptcy and
I'm starting again.

## Installation

#### Install Emacs

Install Emacs. For the macOS, this can be done via a homebrew cask

    brew cask install emacs
    
This seems to be the recommended way to install emacs. For formulae that build .app targets linkapps
doesn't work so well.

#### Clone The Repo

Clone the repo and make the output be the `.emacs.d` directory.

    git clone https://github.com/JungleCandy/ygg-emacs.git .emacs.d

#### Run emacs

Launching Emacs, while there is an active network connection will allow the initial set of packages to be downloaded. After that it's use as normal.

## Inspiration

Of course, we all stand on the shoulders of giants, so I'm currently looking at these dotfiles and I'm ~~stealing~~ taking inspiration from their approaches:

- [Prelude](https://github.com/bbatsov/prelude)
- [rajeep/emacs](https://github.com/rejeep/emacs)
- [Magnars/.emacs.d](https://github.com/magnars/.emacs.d)
- [ohai-emacs](https://github.com/bodil/ohai-emacs)
- [technomancy/better-defaults](https://github.com/technomancy/better-defaults)

## Licence

GPL v3, obviously.
