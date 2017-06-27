# Copyright 1999-2017 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2

# Adapted from the dev-scheme/chibi-scheme-0.7.ebuild in the lisp
# overlay.

#
# FIXME:
#
#    * Handle shared library symlinks properly.
#    * Multilib support.
#    * Install in /usr/lib64, etc., at least.
#    * Check that the src_prepare() is correct.
#    * static-libs support.
#

EAPI=6

DESCRIPTION="A very tiny Scheme implementation with decent speed and native hygienic macros."
SRC_URI="http://synthcode.com/scheme/chibi/${P}.tgz"
HOMEPAGE="http://synthcode.com/wiki/chibi-scheme"

LICENSE="BSD"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="boehm-gc"

DEPEND="boehm-gc? ( dev-libs/boehm-gc )"
RDEPEND="${DEPEND}"

src_prepare() {
	default

	# Upstream uses a D variable in its Makefile which conflicts with
	# the variable predifined by ebuild D="${PORTAGE_BUILDDIR}/image"
	#
	# ********* FIXME: I don’t think this is true anymore.
	#
	sed 's,\([^[:alpha:]]\)D\([^[:alpha:]]\),\1chibiD\2,' -i Makefile \
		|| die "sed Makefile failed"

	# Upstream calls ldconfig in the install target, which goes outside
	# of the ebuild Sandbox.  We'll call ldconfig ourselves after merging
	# the package.
	#
	# ********* FIXME: This is not correct anymore.
	#
	#sed '/ldconfig/d' -i Makefile \
	#	|| die "sed Makefile failed"

	# Incorporate local system's LDFLAGS into the chibi-scheme executable
	sed 's/$(CC) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme/& $(LDFLAGS)/' -i Makefile \
		|| die "sed Makefile failed"

	# Incorporate local system's LDFLAGS into the chibi-scheme executable
	sed 's/$(CC) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. -lchibi-scheme/& $(LDFLAGS)/' -i Makefile \
		|| die "sed Makefile failed"

	# Set SONAME in libchibi-scheme.so
	sed '/^libchibi-scheme/{ n; s/$(CC) $(CLIBFLAGS) -o $@ $^ $(XLDFLAGS)/& -Wl,-soname,libchibi-scheme.so/}' -i Makefile \
		|| die "sed Makefile failed"

	# Incorporate local system's LDFLAGS into Chibi's compiled modules
	sed 's/$(CC) $(CLIBFLAGS) $(XCPPFLAGS) $(XCFLAGS) -o $@ $< -L. $(XLIBS) -lchibi-scheme/& $(LDFLAGS)/' -i Makefile.libs \
		|| die "sed Makefile.libs failed"

#	# Force soname symlinks to be relative to installation directory
#	sed 's/$(LN) -s -f /$(LN) -s -f -r /' -i Makefile \
#		|| die "sed Makefile failed"

}

# When the number of jobs > 1, the Makefile lets things get ahead of themselves
# and sometimes later targets will fail because libchibi-scheme.so doesn't yet
# exist.
src_compile() {
	# FIXME: Make the $(use ...) be a shell function.
	emake -j1 $(use boehm-gc && echo "SEXP_USE_BOEHM=1")
}

src_test() {
	env LD_LIBRARY_PATH="${S}" emake test $(use boehm-gc && echo "SEXP_USE_BOEHM=1")
}

src_install() {
	emake install DESTDIR="${D}" LD="ln -s -f -r" PREFIX=/usr $(use boehm-gc && echo "SEXP_USE_BOEHM=1")
	einstalldocs
}
