/* libguestfs generated file
 * WARNING: THIS FILE IS GENERATED
 *          from the code in the generator/ subdirectory.
 * ANY CHANGES YOU MAKE TO THIS FILE WILL BE LOST.
 *
 * Copyright (C) 2009-2019 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

use crate::event;
use std::collections;
use std::str;
use std::sync;

#[allow(non_camel_case_types)]
#[repr(C)]
pub(crate) struct guestfs_h {
    _unused: [u32; 0],
}

#[link(name = "guestfs")]
extern "C" {
    fn guestfs_create() -> *mut guestfs_h;
    fn guestfs_create_flags(flags: i64) -> *mut guestfs_h;
    fn guestfs_close(g: *mut guestfs_h);
}

const GUESTFS_CREATE_NO_ENVIRONMENT: i64 = 1;
const GUESTFS_CREATE_NO_CLOSE_ON_EXIT: i64 = 2;

pub struct Handle {
    pub(crate) g: *mut guestfs_h,
    pub(crate) callbacks: collections::HashMap<event::EventHandle, sync::Arc<event::Callback>>,
}

impl Handle {
    pub fn create() -> Result<Handle, &'static str> {
        let g = unsafe { guestfs_create() };
        if g.is_null() {
            Err("failed to create guestfs handle")
        } else {
            let callbacks = collections::HashMap::new();
            Ok(Handle { g, callbacks })
        }
    }

    pub fn create_flags(flags: CreateFlags) -> Result<Handle, &'static str> {
        let g = unsafe { guestfs_create_flags(flags.to_libc_int()) };
        if g.is_null() {
            Err("failed to create guestfs handle")
        } else {
            let callbacks = collections::HashMap::new();
            Ok(Handle { g, callbacks })
        }
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        unsafe { guestfs_close(self.g) }
    }
}

pub struct CreateFlags {
    create_no_environment_flag: bool,
    create_no_close_on_exit_flag: bool,
}

impl CreateFlags {
    pub fn none() -> CreateFlags {
        CreateFlags {
            create_no_environment_flag: false,
            create_no_close_on_exit_flag: false,
        }
    }

    pub fn new() -> CreateFlags {
        CreateFlags::none()
    }

    pub fn create_no_environment(mut self, flag: bool) -> CreateFlags {
        self.create_no_environment_flag = flag;
        self
    }

    pub fn create_no_close_on_exit_flag(mut self, flag: bool) -> CreateFlags {
        self.create_no_close_on_exit_flag = flag;
        self
    }

    unsafe fn to_libc_int(self) -> i64 {
        let mut flag = 0;
        flag |= if self.create_no_environment_flag {
            GUESTFS_CREATE_NO_ENVIRONMENT
        } else {
            0
        };
        flag |= if self.create_no_close_on_exit_flag {
            GUESTFS_CREATE_NO_CLOSE_ON_EXIT
        } else {
            0
        };
        flag
    }
}

pub struct UUID {
    uuid: [u8; 32],
}

impl UUID {
    pub(crate) fn new(uuid: [u8; 32]) -> UUID {
        UUID { uuid }
    }
    pub fn to_bytes(self) -> [u8; 32] {
        self.uuid
    }
}
