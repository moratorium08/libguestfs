(* libguestfs
 * Copyright (C) 2019 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

(* Please read generator/README first. *)

open Std_utils
open Types
open Utils
open Pr
open Docstrings
open Optgroups
open Actions
open Structs
open C
open Events

let rec indent n = match n with
  | x when x > 0 -> pr "    "; indent (x - 1)
  | _ -> ()

let generate_rust () =
  generate_header CStyle LGPLv2plus;

  pr "
use std::ffi;
use std::slice;
use std::os::raw::c_char;


#[allow(non_camel_case_types)]
enum guestfs_h {}  // opaque struct

#[link(name = \"guestfs\")]
extern \"C\" {
    fn guestfs_create() -> *mut guestfs_h;
    fn guestfs_create_flags(flags: i64) -> *mut guestfs_h;
    fn guestfs_close(g: *mut guestfs_h);
}

const GUESTFS_CREATE_NO_ENVIRONMENT: i64 = 1;
const GUESTFS_CREATE_NO_CLOSE_ON_EXIT: i64 = 2;

pub struct Handle {
    g: *mut guestfs_h,
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

impl Handle {
    pub fn create() -> Result<Handle, &'static str> {
        let g = unsafe { guestfs_create() };
        if g.is_null() {
            Err(\"failed to create guestfs handle\")
        } else {
            Ok(Handle { g })
        }
    }

    pub fn create_flags(flags: CreateFlags) -> Result<Handle, &'static str> {
        let g = unsafe { guestfs_create_flags(flags.to_libc_int()) };
        if g.is_null() {
            Err(\"failed to create guestfs handle\")
        } else {
            Ok(Handle { g })
        }
    }
}

pub struct UUID {
    uuid: [u8; 32]
}

impl UUID {
    fn new(uuid: [u8; 32]) -> UUID {
        UUID { uuid }
    }
}
";
  List.iter (
    fun { s_camel_name = name; s_name = c_name; s_cols = cols } ->
      pr "\n";
      pr "pub struct %s {\n" name;
      List.iter (
        function
        | n, FChar -> pr "    %s: i8,\n" n
        | n, FString -> pr "    %s: String,\n" n
        | n, FBuffer -> pr "    %s: Vec<u8>,\n" n
        | n, FUInt32 -> pr "    %s: u32,\n" n
        | n, FInt32 -> pr "    %s: i32,\n" n
        | n, (FUInt64 | FBytes) -> pr "    %s: u64,\n" n
        | n, FInt64 -> pr "    %s: i64,\n" n
        | n, FUUID -> pr "    %s: UUID,\n" n
        | n, FOptPercent -> pr "    %s: Option<f32>,\n" n
      ) cols;
      pr "}\n";
      pr "#[repr(C)]\n";
      pr "struct Raw%s {\n" name;
      List.iter (
        function
        | n, FChar -> pr "    %s: c_char,\n" n
        | n, FString -> pr "    %s: *const c_char,\n" n
        | n, FBuffer ->
          pr "    %s_len: usize,\n" n;
          pr "    %s: *const c_char,\n" n;
        | n, FUUID -> pr "    %s: [u8; 32],\n" n
        | n, FUInt32 -> pr "    %s: u32,\n" n
        | n, FInt32 -> pr "    %s: i32,\n" n
        | n, (FUInt64 | FBytes) -> pr "    %s: u64,\n" n
        | n, FInt64 -> pr "    %s: i64,\n" n
        | n, FOptPercent -> pr "    %s: f32,\n" n
      ) cols;
      pr "}\n";
      pr "\n";
      pr "impl %s {\n" name;
      pr "    fn new(raw: *const Raw%s) -> %s {\n" name name;
      pr "        unsafe { %s {\n" name;
      List.iter (
        fun x ->
          indent 3;
          match x with
          | n, FChar ->
            pr "%s: (*raw).%s as i8,\n" n n;
          | n, FString ->
            pr "%s: {\n" n;
            indent 4;
            pr "let s = ffi::CStr::from_ptr((*raw).%s);\n" n;
            indent 4;
            pr "s.to_str().unwrap().to_string()\n";
            indent 3;
            pr "},\n"
          | n, FBuffer ->
            pr "%s: slice::from_raw_parts((*raw).%s as *const u8, (*raw).%s_len).to_vec(),\n" n n n
          | n, FUUID ->
            pr "%s: UUID::new((*raw).%s),\n" n n
          | n, (FUInt32 | FInt32 | FUInt64 | FInt64 | FBytes) ->
            pr "%s: (*raw).%s,\n" n n
          | n, FOptPercent ->
            pr "%s: if (*raw).%s < 0.0 { None } else { Some((*raw).%s) },\n" n n n
      ) cols;
      pr "        } }\n";
      pr "    }\n";
      pr "}\n"
  ) external_structs;

