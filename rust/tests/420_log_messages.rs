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

use std::str;
use std::sync::{Arc, Mutex};

#[test]
fn log_messages() {
    let close_invoked = Arc::new(Mutex::new(0));
    {
        let mut g = guestfs::Handle::create().expect("create");
        g.set_event_callback(
            |ev, _, buf, array| {
                let mut data = (&close_invoked).lock().unwrap();
                *data += 1;

                let event = guestfs::event_to_string(&[ev]).unwrap();

                let buf = str::from_utf8(buf).unwrap();
                let array = array
                    .into_iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join(",");

                eprintln!("event logged: event={} buf={} array={}", event, buf, array)
            },
            &[
                guestfs::Event::Appliance,
                guestfs::Event::Library,
                guestfs::Event::Warning,
                guestfs::Event::Trace,
            ],
        )
        .unwrap();

        g.set_trace(true).unwrap();
        g.set_verbose(true).unwrap();
        g.add_drive_ro("/dev/null").unwrap();
        g.set_autosync(true).unwrap();
    }
    assert!(*((&close_invoked).lock().unwrap()) > 0);
}