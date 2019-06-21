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
use std::convert;
use std::ffi;
use std::slice;
use std::ptr;
use std::os::raw::{c_char, c_int, c_void};


#[allow(non_camel_case_types)]
enum guestfs_h {}  // opaque struct

#[link(name = \"guestfs\")]
extern \"C\" {
    fn guestfs_create() -> *mut guestfs_h;
    fn guestfs_create_flags(flags: i64) -> *mut guestfs_h;
    fn guestfs_close(g: *mut guestfs_h);
    fn guestfs_last_error(g: *mut guestfs_h) -> *const c_char;
    fn guestfs_last_errno(g: *mut guestfs_h) -> c_int;
}

extern \"C\" {
    fn free(buf: *const c_void);
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

struct NullTerminatedIter<T: Copy + Clone> {
    p: *const T
}

impl<T: Copy + Clone> NullTerminatedIter<T> {
    fn new(p: *const T) -> NullTerminatedIter<T> {
        NullTerminatedIter{ p }
    }
}

impl<T: Copy + Clone> Iterator for NullTerminatedIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.p.is_null() {
            None
        } else {
            let r = unsafe { *(self.p) };
            self.p = unsafe { self.p.offset(1) };
            Some(r)
        }
    }
}

#[repr(C)]
struct RawList<T> {
    size: u32,
    ptr: *const T,
}

struct RawListIter<'a, T> {
    current: u32,
    list: &'a RawList<T>
}

impl<T> RawList<T>  {
    fn iter<'a>(&'a self) -> RawListIter<'a, T> {
        RawListIter{ current: 0, list: self }
    }
}

impl<'a, T> Iterator for RawListIter<'a, T> {
    type Item = *const T;
    fn next(&mut self) -> Option<*const T> {
        if self.current >= self.list.size {
            None
        } else {
            let elem = unsafe { self.list.ptr.offset(self.current as isize) };
            self.current += 1;
            Some(elem)
        }
    }
}


fn arg_string_list (v: &Vec<&str>) -> Vec<*const i8> {
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

fn hashmap (l: *const *const c_char) -> collections::HashMap<String, String> {
    let mut map = collections::HashMap::new();
    let mut iter = NullTerminatedIter::new(l);
    while let Some(key) = iter.next() {
        if let Some(val) = iter.next() {
            let key = unsafe { ffi::CStr::from_ptr(key) }.to_str().unwrap();
            let val = unsafe { ffi::CStr::from_ptr(val) }.to_str().unwrap();
            map.insert(key.to_string(), val.to_string());
        } else {
            panic!(\"odd number of items in hash table\");
        }
    }
    map
}

fn struct_list<T, S: convert::From<*const T>>(l: *const RawList<T>) -> Vec<S> {
    let mut v = Vec::new();
    for x in unsafe {&*l}.iter() {
        v.push(S::from(x));
    }
    v
}

fn string_list (l: *const *const c_char) -> Vec<String> {
    let mut v = Vec::new();
    for x in NullTerminatedIter::new(l) {
        let s = unsafe { ffi::CStr::from_ptr(x) }.to_str().unwrap();
        v.push(s.to_string());
    }
    v
}

fn free_string_list(l: *const *const c_char) {
    // TODO
}

#[derive(Debug)]
pub struct Error {
    operation: &'static str,
    message: String,
    errno: i32
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

    fn get_error_from_handle(&self, operation: &'static str) -> Error {
        let c_msg = unsafe { guestfs_last_error(self.g) };
        let message = unsafe { ffi::CStr::from_ptr(c_msg).to_str().unwrap().to_string() };
        let errno = unsafe { guestfs_last_errno(self.g) } ;
        Error { operation, message, errno }
    }
}

pub struct UUID {
    uuid: [u8; 32]
}

impl UUID {
    fn new(uuid: [u8; 32]) -> UUID {
        UUID { uuid }
    }
    pub fn to_bytes(self) -> [u8; 32] {
        self.uuid
    }
}

";
  List.iter (
    fun { s_camel_name = name; s_name = c_name; s_cols = cols } ->
      pr "\n";
      pr "pub struct %s {\n" name;
      List.iter (
        function
        | n, FChar -> pr "    pub %s: i8,\n" n
        | n, FString -> pr "    pub %s: String,\n" n
        | n, FBuffer -> pr "    pub %s: Vec<u8>,\n" n
        | n, FUInt32 -> pr "    pub %s: u32,\n" n
        | n, FInt32 -> pr "    pub %s: i32,\n" n
        | n, (FUInt64 | FBytes) -> pr "    pub %s: u64,\n" n
        | n, FInt64 -> pr "    pub %s: i64,\n" n
        | n, FUUID -> pr "    pub %s: UUID,\n" n
        | n, FOptPercent -> pr "    pub %s: Option<f32>,\n" n
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
      pr "impl convert::From<*const Raw%s> for %s {\n" name name;
      pr "    fn from(raw: *const Raw%s) -> Self {\n" name;
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

  (* generate free functionf of structs *)
  pr "\n";
  pr "extern \"C\" {\n";
  List.iter (
    fun {  s_camel_name = name; s_name = c_name; } ->
      pr "fn guestfs_free_%s(v: *const Raw%s);\n" c_name name;
      pr "fn guestfs_free_%s_list(l: *const RawList<Raw%s>);\n" c_name name;
  ) external_structs;
  pr "}\n";


  (* generate structs for optional arguments *)
  List.iter (
    fun ({ name = name; shortdesc = shortdesc;
          style = (ret, args, optargs) }) ->
      let cname = snake2caml name in
      let rec contains_ptr args = match args with
        | [] -> false
        | OString _ ::_
        | OStringList _::_ -> true
        | _::xs -> contains_ptr xs
      in
      let opt_life_parameter = if contains_ptr optargs then "<'a>" else "" in
      if optargs <> [] then (
        pr "\n";
        pr "/* Optional Structs */\n";
        pr "#[derive(Default)]\n";
        pr "pub struct OptArgs%s%s {\n" cname opt_life_parameter;
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
              pr "    _%s: Option<&'a str>,\n" n
            | OStringList _ ->
              pr "    _%s: Option<Vec<&'a str>>,\n" n
        ) optargs;
        pr "}\n\n";

        (* raw struct for C bindings *)
        pr "#[repr(C)]\n";
        pr "struct RawOptArgs%s {\n" cname;
        pr "    bitmask: u64,\n";
        List.iter (
          fun optarg ->
            let n = translate_bad_symbols (name_of_optargt optarg) in
            match optarg with
            | OBool _ ->
              pr "    %s: c_int,\n" n
            | OInt _ ->
              pr "    %s: c_int,\n" n
            | OInt64 _ ->
              pr "    %s: i64,\n" n
            | OString _ ->
              pr "    %s: *const c_char,\n" n
            | OStringList _ ->
              pr "    %s: *const *const c_char,\n" n
        ) optargs;
        pr "}\n\n";

        pr "impl%s convert::From<OptArgs%s%s> for RawOptArgs%s {\n"
          opt_life_parameter cname opt_life_parameter cname;
        pr "    fn from(optargs: OptArgs%s%s) -> Self {\n" cname opt_life_parameter;
        pr "        let mut bitmask = 0;\n";
        pr "         RawOptArgs%s {\n" cname;
        List.iteri (
          fun index optarg ->
            let n = translate_bad_symbols (name_of_optargt optarg) in
            match optarg with
            | OBool _ ->
              pr "        %s: if let Some(v) = optargs._%s {\n" n n;
              pr "            bitmask |= 1 << %d;\n" index;
              pr "            if v { 1 } else { 0 }\n";
              pr "        } else {\n";
              pr "            0\n";
              pr "        },\n";
            | OInt _ | OInt64 _  ->
              pr "        %s: if let Some(v) = optargs._%s {\n" n n;
              pr "            bitmask |= 1 << %d;\n" index;
              pr "            v\n";
              pr "        } else {\n";
              pr "            0\n";
              pr "        },\n";
            | OString _ ->
              pr "        %s: if let Some(v) = optargs._%s {\n" n n;
              pr "            bitmask |= 1 << %d;\n" index;
              pr "            let y: &str = &v;\n";
              pr "            ffi::CString::new(y).unwrap().as_ptr()\n";
              pr "        } else {\n";
              pr "            ptr::null()\n";
              pr "        },\n";
            | OStringList _ ->
              pr "        %s: if let Some(v) = optargs._%s {\n" n n;
              pr "            bitmask |= 1 << %d;\n" index;
              pr "            arg_string_list(&v).as_ptr()";
              pr "        } else {\n";
              pr "            ptr::null()\n";
              pr "        },\n";
        ) optargs;
        pr "              bitmask,\n";
        pr "         }\n";
        pr "    }\n";
        pr "}\n";

        pr "impl%s OptArgs%s%s {\n" opt_life_parameter cname opt_life_parameter;
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
              pr "&'a str"
            | OStringList _ ->
              pr "Vec<&'a str>"
            );
            pr ") -> OptArgs%s%s {\n" cname opt_life_parameter;
            pr "        OptArgs%s { _%s: Some(%s), ..self }\n" cname n n;
            pr "    }\n"
        ) optargs;
        pr "}\n\n";
      );
  ) (actions |> external_functions |> sort);

  (* extern C APIs *)
  pr "extern \"C\" {\n";
  List.iter (
    fun ({ name = name; shortdesc = shortdesc;
          style = (ret, args, optargs) } as f) ->
      let cname = snake2caml name in
      pr "fn %s(g: *const guestfs_h" f.c_function;
      List.iter (
        fun arg ->
          pr ", ";
          match arg with
          | Bool n -> pr "%s: c_int" n
          | String (_, n) -> pr "%s: *const c_char" n
          | OptString n -> pr "%s: *const c_char" n
          | Int n -> pr "%s: c_int" n
          | Int64 n -> pr "%s: i64" n
          | Pointer (_, n) -> pr "%s: *const ffi::c_void" n
          | StringList (_, n) -> pr "%s: *const *const c_char" n
          | BufferIn n -> pr "%s: *const c_char, %s_len: usize" n n
      ) args;
      (match ret with
       | RBufferOut _ -> pr ", size: *const usize"
       | _ -> ()
      );
      if optargs <> [] then
        pr ", optarg: *const RawOptArgs%s" cname;

      pr ") -> ";

      (match ret with
      | RErr | RInt _ | RBool _ -> pr "c_int"
      | RInt64 _ -> pr "i64"
      | RConstString _ | RString _ | RConstOptString _  -> pr "*const c_char"
      | RBufferOut _ -> pr "*const u8"
      | RStringList _ | RHashtable _-> pr "*const *const c_char"
      | RStruct (_, n) ->
        let n = camel_name_of_struct n in
        pr "*const Raw%s" n
      | RStructList (_, n) ->
        let n = camel_name_of_struct n in
        pr "*const RawList<Raw%s>" n
      );
      pr ";\n";

  ) (actions |> external_functions |> sort);
  pr "}\n";


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
          | String (_, n) -> pr "%s: &str" n
          | OptString n -> pr "%s: Option<&str>" n
          | StringList (_, n) -> pr "%s: Vec<&str>" n
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
      | RConstString _ -> pr "&'static str"
      | RString _ -> pr "String"
      | RConstOptString _ -> pr "Option<&'static str>"
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
          pr "let c_%s = arg_string_list(&%s).as_ptr();\n" n n;
        | BufferIn n ->
          pr "let c_%s_len = (&%s).len();\n" n n;
          pr "let c_%s = ffi::CString::new(%s)\n" n n;
          pr "            .expect(\"CString::new failed\")\n";
          pr "            .as_ptr();\n";
        | Int _ | Int64 _ | Pointer _ -> ()
      ) args;

      (* TODO: handle optargs *)
      if optargs <> [] then (
        pr "let optargs_raw = RawOptArgs%s::from(optargs);\n" cname;
      );

      (match ret with
       | RBufferOut _ ->
         pr "let mut size = 0usize;\n"
       | _ -> ()
      );

      pr "\n";

      pr "let r = unsafe { %s(self.g" f.c_function;
      let pr = _pr in
      List.iter (
        fun arg ->
          pr ", ";
          match arg with
          | Bool n | String (_, n) | OptString n -> pr "c_%s" n
          | Int n | Int64 n -> pr "%s" n
          | Pointer _ -> pr "ptr::null()" (* XXX: what is pointer? *)
          | StringList (_, n) -> pr "c_%s as *const *const c_char" n
          | BufferIn n -> pr "c_%s, c_%s_len" n n
      ) args;
      (match ret with
       | RBufferOut _ -> pr ", &size as *const usize"
       | _ -> ()
      );
      if optargs <> [] then (
        pr ", &optargs_raw as *const RawOptArgs%s" cname;
      );
      pr ") };\n";

      let _pr = pr in
      let pr fs = indent 2; pr fs in
      (match errcode_of_ret ret with
       | `CannotReturnError -> ()
       | `ErrorIsMinusOne ->
         pr "if r == -1 {\n";
         pr "    return Err(self.get_error_from_handle (\"%s\"));\n" name;
         pr "}\n"
       | `ErrorIsNULL ->
         pr "if r.is_null() {\n";
         pr "    return Err(self.get_error_from_handle (\"%s\"));\n" name;
         pr "}\n"
      );
      pr "Ok(";
      let pr = _pr in
      (match ret with
       | RErr -> pr "()"
       | RInt _ | RInt64 _ -> pr "r"
       | RBool _ -> pr "r != 0"
       | RConstString _ ->
         pr "unsafe{ ffi::CStr::from_ptr(r) }.to_str().unwrap()\n"
       | RString _ ->
         pr "{";
         pr "    let s = unsafe {ffi::CStr::from_ptr(r)}\n";
         pr "       .to_str().unwrap().to_string();\n";
         pr "    unsafe { free(r as * const c_void) };\n";
         pr "    s\n";
         pr "}\n";
       | RConstOptString _ ->
         indent 3; pr "if r.is_null() {\n";
         indent 3; pr "    None\n";
         indent 3; pr "} else {\n";
         indent 3; pr "    Some(unsafe { ffi::CStr::from_ptr(r) }.to_str().unwrap())\n";
         indent 3; pr "}";
       | RStringList _ ->
         pr "{\n";
         pr "    let s = string_list(r);\n";
         pr "    free_string_list(r);\n";
         pr "    s\n";
         pr "}\n";
       | RStruct (_, n) ->
         let sn = camel_name_of_struct n in
         pr "{\n";
         pr "    let s = %s::from(r);\n" sn;
         pr "    unsafe { guestfs_free_%s(r) };\n" n;
         pr "    s\n";
         pr "}\n";
       | RStructList (_, n) ->
         let sn = camel_name_of_struct n in
         pr "{\n";
         pr "    let l = struct_list::<Raw%s, %s>(r);\n" sn sn;
         pr "    unsafe { guestfs_free_%s_list(r) };" n;
         pr "    l\n";
         pr "}\n";
       | RHashtable _ ->
         pr "{\n";
         pr "    let h = hashmap(r);\n";
         pr "    free_string_list(r);\n";
         pr "    h\n";
         pr "}\n";
       | RBufferOut _ ->
         pr "{\n";
         pr "    let s = unsafe { slice::from_raw_parts(r, size) }.to_vec();\n";
         pr "    unsafe { free(r as * const c_void) } ;\n";
         pr "    s\n";
         pr "}\n";
      );
      pr ")\n";
      pr "    }\n\n"
  ) (actions |> external_functions |> sort);
  pr "}\n"
