EAPI=5

CABAL_FEATURES="lib profile haddock hoogle hscolour"
inherit haskell-cabal git-r3

DESCRIPTION="Josh Thibodaux's XMonad extensions."
HOMEPAGE="https://github.com/shaggyshoggoth/xmonad-jutil"
EGIT_REPO_URI="https://github.com/shaggyshoggoth/xmonad-jutil.git"

LICENSE="BSD"
SLOT="0/${PV}"
KEYWORDS=""
IUSE=""

RDEPEND="dev-haskell/extensible-exceptions:=[profile?]
        >=dev-haskell/mtl-1:=[profile?] <dev-haskell/mtl-3:=[profile?]
        dev-haskell/old-locale:=[profile?]
        dev-haskell/random:=[profile?]
        dev-haskell/utf8-string:=[profile?]
        >=dev-haskell/x11-1.6.1:=[profile?]
        >=dev-lang/ghc-7.4.1:=
        >=x11-wm/xmonad-0.13:=[profile?]
        >=x11-wm/xmonad-contrib-0.13:=[profile?]
        >=dev-haskell/x11-xft-0.2:=[profile?]
        "
DEPEND="${RDEPEND}
        >=dev-haskell/cabal-1.6
        "
