/* libguestfs Python bindings
 Copyright (C) 2009-2019 Red Hat Inc.

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or
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

use guestfs::*;

#[test]
fn create_flags() {
    let _h = Handle::create_flags(CreateFlags::none()).expect("create_flags fail");
    // TODO: Add parse_environment to check the flag is created correctly
    let flags = CreateFlags::new()
        .create_no_environment(true);
    let _h = Handle::create_flags(flags).expect("create_flags fail");
    // TODO: Add parse_environment to check the flag is created correctly
}
