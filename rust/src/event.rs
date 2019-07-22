use crate::base;
use crate::error;
use crate::guestfs;
use std::ffi;
use std::os::raw::{c_char, c_void};
use std::slice;
use std::sync;

type GuestfsEventCallback = extern "C" fn(
    *const base::guestfs_h,
    *const c_void,
    u64,
    i32,
    i32,
    *const i8,
    usize,
    *const u64,
    usize,
);

#[link(name = "guestfs")]
extern "C" {
    fn guestfs_set_event_callback(
        g: *const base::guestfs_h,
        cb: GuestfsEventCallback,
        event_bitmask: u64,
        flags: i32,
        opaque: *const c_void,
    ) -> i32;
    fn guestfs_delete_event_callback(g: *const base::guestfs_h, eh: i32);
    fn guestfs_event_to_string(bitmask: u64) -> *const c_char;
    fn free(buf: *const c_void);
}

#[derive(Hash, PartialEq, Eq)]
pub struct EventHandle {
    eh: i32,
}

pub type Callback = Fn(guestfs::Event, EventHandle, &[i8], &[u64]);

fn events_to_bitmask(v: &[guestfs::Event]) -> u64 {
    let mut r = 0u64;
    for x in v.iter() {
        r |= x.to_u64();
    }
    r
}

/* -- Why Not Box<Callback> but Arc<Callback> (in struct base::Handle)?  --
 *  Assume that there are more than threads. While callback is running,
 *  if a thread frees the handle, automatically the buffer is freed if Box<Callback>
 *  is used. Therefore Arc<Callback> is used.
 */

impl base::Handle {
    pub fn set_event_callback<'a, C>(
        &'a mut self,
        callback: C,
        events: &[guestfs::Event],
    ) -> Result<EventHandle, error::Error>
    where
        C: Fn(guestfs::Event, EventHandle, &[i8], &[u64]) + 'static,
    {
        extern "C" fn trampoline<C>(
            g: *const base::guestfs_h,
            opaque: *const c_void,
            event: u64,
            event_handle: i32,
            flags: i32,
            buf: *const c_char,
            buf_len: usize,
            array: *const u64,
            array_len: usize,
        ) where
            C: Fn(guestfs::Event, EventHandle, &[i8], &[u64]) + 'static,
        {
            // trampoline function
            // c.f. https://s3.amazonaws.com/temp.michaelfbryan.com/callbacks/index.html

            let event = match guestfs::Event::from_bitmask(event) {
                Some(x) => x,
                None => panic!("Failed to parse bitmask: {}", event),
            };
            let eh = EventHandle { eh: event_handle };
            let buf = unsafe { slice::from_raw_parts(buf, buf_len) };
            let array = unsafe { slice::from_raw_parts(array, array_len) };

            let callback_ptr = unsafe { &*(opaque as *const sync::Arc<C>) };
            let callback = sync::Arc::clone(&callback_ptr);
            callback(event, eh, buf, array)
        }
        let callback = sync::Arc::new(callback);
        let event_bitmask = events_to_bitmask(events);

        let eh = {
            // Weak::into_raw is nightly.
            // In order to make sure that callback is freed when handle is freed,
            // lifetime is explicitly declared.
            let ptr: &'a sync::Arc<C> = Box::leak(Box::new(callback.clone()));
            unsafe {
                guestfs_set_event_callback(
                    self.g,
                    trampoline::<C>,
                    event_bitmask,
                    0,
                    ptr as *const sync::Arc<C> as *const c_void,
                )
            }
        };
        if eh == -1 {
            return Err(self.get_error_from_handle("set_event_callback"));
        }
        self.callbacks.insert(EventHandle { eh }, callback);

        Ok(EventHandle { eh })
    }

    pub fn delete_event_callback(&mut self, eh: EventHandle) -> Result<(), error::Error> {
        unsafe {
            guestfs_delete_event_callback(self.g, eh.eh);
        }
        self.callbacks.remove(&eh);
        Ok(())
    }

    pub fn event_to_string(&self, events: &[guestfs::Event]) -> Result<String, error::Error> {
        let bitmask = events_to_bitmask(events);

        let r = unsafe { guestfs_event_to_string(bitmask) };
        if r.is_null() {
            Err(error::unix_error("event_to_string"))
        } else {
            let s = unsafe { ffi::CStr::from_ptr(r) };
            unsafe { free(r as *const c_void) };
            Ok(s.to_str()?.to_string())
        }
    }
}
