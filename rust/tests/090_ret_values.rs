/* libguestfs Rust bindings
Copyright (C) 2009-2019 Red Hat Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

extern crate guestfs;

#[test]
fn rint() {
    let g = guestfs::Handle::create().expect("create");
    assert_eq!(g.internal_test_rint("10").unwrap(), 10);
    assert!(g.internal_test_rinterr().is_err())
}

#[test]
fn rint64() {
    let g = guestfs::Handle::create().expect("create");
    assert_eq!(g.internal_test_rint64("10").unwrap(), 10);
    assert!(g.internal_test_rint64err().is_err())
}

#[test]
fn rbool() {
    let g = guestfs::Handle::create().expect("create");
    assert!(g.internal_test_rbool("true").unwrap());
    assert!(!g.internal_test_rbool("false").unwrap());
    assert!(g.internal_test_rboolerr().is_err())
}

#[test]
fn rconststring() {
    let g = guestfs::Handle::create().expect("create");
    assert_eq!(
        g.internal_test_rconststring("test").unwrap(),
        "static string"
    );
    assert!(g.internal_test_rconststringerr().is_err())
}

#[test]
fn rconstoptstring() {
    let g = guestfs::Handle::create().expect("create");
    assert_eq!(
        g.internal_test_rconstoptstring("test").unwrap(),
        Some("static string")
    );
    assert_eq!(g.internal_test_rconstoptstringerr().unwrap(), None)
}
