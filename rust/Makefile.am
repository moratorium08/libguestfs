# libguestfs golang bindings
# Copyright (C) 2019 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

include $(top_srcdir)/subdir-rules.mk

generator_built = \
	src/bin/bindtests.rs \
	src/lib.rs

EXTRA_DIST = \
	.gitignore \
	$(generator_built) \
	tests/*.rs \
	Cargo.toml \
	Cargo.lock \
	run-bindtests \
	run-tests

if HAVE_RUST

all: src/lib.rs
	$(top_builddir)/run $(CARGO) build --release

TESTS = run-bindtests run-tests

CLEANFILES += target/*~

endif

