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

(* Utilities for Rust *)
(* Are there corresponding functions to them? *)
(* Should they be placed in utils.ml? *)
let rec indent n = match n with
  | x when x > 0 -> pr "    "; indent (x - 1)
  | _ -> ()

(* split_on_char exists since OCaml 4.04 *)
(* but current requirements: >=4.01 *)
let split_on_char c = Str.split (Str.regexp (String.make 1 c))

let snake2caml name =
  let l = split_on_char '_' name in
  let l = List.map (fun x -> String.capitalize_ascii x) l in
  String.concat "" l


(* because there is a function which contains 'unsafe' field *)
let black_list = ["unsafe"]

let translate_bad_symbols s =
  if List.exists (fun x -> s = x) black_list then
    s ^ "_"
  else
    s

let generate_rust () =
  generate_header CStyle LGPLv2plus;

  pr "
use std::collections;
use std::ffi;
use std::slice;
use std::ptr;
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

fn arg_string_list (v: &Vec<String>) -> Vec<*const i8> {
    let length = v.len();
    let mut w = Vec::new();
    for x in v.iter() {
        let y: &str = x;
        let s = ffi::CString::new(y).unwrap();
        w.push(s.as_ptr());
    }
    w.push(ptr::null());
    w
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
}\

pub struct Error {
    // TODO
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

  (* generate structs for optional arguments *)
  List.iter (
    fun ({ name = name; shortdesc = shortdesc;
          style = (ret, args, optargs) }) ->
      let cname = snake2caml name in
      if optargs <> [] then (
        pr "\n";
        pr "/* Optional Structs */\n";
        pr "#[derive(Default)]\n";
        pr "pub struct OptArgs%s {\n" cname;
        List.iter (
          fun optarg ->
            let n = translate_bad_symbols (name_of_optargt optarg) in
            match optarg with
            | OBool _ ->
              pr "    _%s: Option<bool>,\n" n
            | OInt _ ->
              pr "    _%s: Option<i32>,\n" n
            | OInt64 _ ->
              pr "    _%s: Option<i64>,\n" n
            | OString _ ->
              pr "    _%s: Option<String>,\n" n
            | OStringList _ ->
              pr "    _%s: Option<Vec<String>>,\n" n
        ) optargs;
        pr "}\n\n";
        pr "impl OptArgs%s {\n" cname;
        List.iter (
          fun optarg ->
            let n = translate_bad_symbols (name_of_optargt optarg) in
            pr "    pub fn %s(self, %s: " n n;
            (match optarg with
            | OBool _ ->
              pr "bool"
            | OInt _ ->
              pr "i32"
            | OInt64 _ ->
              pr "i64"
            | OString _ ->
              pr "String"
            | OStringList _ ->
              pr "Vec<String>"
            );
            pr ") -> OptArgs%s {\n" cname;
            pr "        OptArgs%s { _%s: Some(%s), ..self }\n" cname n n;
            pr "    }\n"
        ) optargs;
        pr "}\n\n";
      );
  ) (actions |> external_functions |> sort);

  pr "impl Handle {\n";
  List.iter (
    fun ({ name = name; shortdesc = shortdesc;
          style = (ret, args, optargs) } as f) ->
      let cname = snake2caml name in
      pr "    /// %s \n" shortdesc;
      pr "    pub fn %s" name;

      (* generate arguments *)
      pr "(&self, ";
      let comma = ref false in
      List.iter (
        fun arg ->
          if !comma then pr ", ";
          comma := true;
          match arg with
          | Bool n -> pr "%s: bool" n
          | Int n -> pr "%s: i32" n
          | Int64 n -> pr "%s: i64" n
          | String (_, n) -> pr "%s: String" n
          | OptString n -> pr "%s: Option<String>" n
          | StringList (_, n) -> pr "%s: Vec<String>" n
          | BufferIn n -> pr "%s: Vec<u8>" n
          | Pointer (_, n) -> pr "%s: usize" n
      ) args;
      if optargs <> [] then (
        if !comma then pr ", ";
        comma := true;
        pr "optargs: OptArgs%s" cname
      );
      pr ")";

      (* generate return type *)
      pr " -> Result<";
      (match ret with
      | RErr -> pr "()"
      | RInt _ -> pr "i32"
      | RInt64 _ -> pr "i64"
      | RBool _ -> pr "bool"
      | RConstString _
      | RString _ -> pr "String"
      | RConstOptString _ -> pr "Option<String>"
      | RStringList _ -> pr "Vec<String>"
      | RStruct (_, sn) ->
        let sn = camel_name_of_struct sn in
        pr "%s" sn
      | RStructList (_, sn) ->
        let sn = camel_name_of_struct sn in
        pr "Vec<%s>" sn
      | RHashtable _ -> pr "collections::HashMap<String, String>"
      | RBufferOut _ -> pr "Vec<u8>");
      pr ", Error> {\n";

      let _pr = pr in
      let pr fs = indent 2; pr fs in
      List.iter (
        function
        | Bool n ->
          pr "let c_%s = if %s { 1 } else { 0 };\n" n n
        | String (_, n) ->
          (* TODO: handle errors *)
          pr "let c_%s = \n" n;
          pr "    ffi::CString::new(%s).expect(\"CString::new failed\").as_ptr();\n" n;
        | OptString n ->
          pr "let c_%s = match %s {" n n;
          pr "    Some(s) => \n";
          pr "        ffi::CString::new(s)\n";
          pr "            .expect(\"CString::new failed\")\n";
          pr "            .as_ptr(),\n";
          pr "    None => ptr::null(),\n";
          pr "};\n";
        | StringList (_, n) ->
          pr "let c_%s_v = arg_string_list(%s);\n" n n;
        | BufferIn n ->
          pr "let c_%s = ffi::CString::new(%s)\n" n n;
          pr "            .expect(\"CString::new failed\")\n";
          pr "            .as_ptr();\n";
          pr "let c_%s_len = %s.len();\n" n n;
        | Int _ | Int64 _ | Pointer _ -> ()
      ) args;

      (* TODO: handle optargs *)
      if optargs <> [] then (
      );

      (match ret with
       | RBufferOut _ ->
         pr "let mut size = 0;\n"
       | _ -> ()
      );

      pr "\n";

      pr "let r = unsafe { %s(self.g" f.c_function;
      List.iter (
        fun arg ->
          pr ", ";
          match arg with
          | Bool n | String (_, n) | OptString n -> pr "c_%s" n
          | Int n | Int64 n -> pr "%s" n
          | Pointer _ -> pr "ptr::null()" (* XXX: what is pointer? *)
          | StringList (_, n) -> pr "&c_%s as *const *const c_char" n
          | BufferIn n -> pr "c_%s, c_%s_len" n n
      ) args;
      (match ret with
       | RBufferOut _ -> pr ", &size as *const usize"
       | _ -> ()
      );
      pr ") };\n";

      pr "unimplemented!()\n";
      let pr = _pr in

      pr "    }\n\n"
  ) (actions |> external_functions |> sort);
  pr "}\n"
