use crate::*;

use std::sync;

extern "C" {}


fn events_to_bitmask(v: &[Event]) -> u64 {
    let mut r = 0u64;
    for x in v.iter() {
        r |= x.to_u64();
    }
    r
}

pub fn event_to_string(events: &[Event]) -> String {
    "".to_string()
}

extern "C" fn event_callback_wrapper(
    g: *const guestfs_h,
    opaque: *const c_void,
    event: u64,
    event_handle: i32,
    flags: i32,
    buf: *const c_char,
    buf_len: usize,
    array: *const u64,
    array_len: usize,
) {
    // trampoline function
    // c.f. https://s3.amazonaws.com/temp.michaelfbryan.com/callbacks/index.html

    let event = match Event::from_bitmask(event) {
        Some(x) => x,
        None => panic!("Failed to parse bitmask: {}", event),
    };
    let eh = EventHandle{ eh: event_handle };
    let buf = slice::from_raw_parts(buf, buf_len);
    let array = slice::from_raw_parts(array, array_len);

    g.callbacks(eh)(event, eh, buf, array)
}

pub fn set_event_callback<C>(g: &Handle, callback: C, events: &[Event]) -> Result<EventHandle, Error>
where
    C: Fn(Event, EventHandle, String, Vec<u64>),
{
    let callback = Box::new(callback);
    let event_bitmask = events_to_bitmask(events);

    let handle_weak = sync::Arc::downgrade(&g.g);

    let f = move |event, eh, buf, array| {
        let handle_inner = match handle_weak.upgrade() {
            Some(x) => x,
            None => {
                // this should not occur.
                panic!("Program Error. failed to unpack weak pointer")
            }
        };
        let handle = Handle::from_arc(handle_inner);
        let callbacks = handle.handle.callbacks.lock().unwrap();
        match callbacks.get(&eh) {
            Some(callback) => {
                callback(event, )
            }
        }
    }

    let eh = guestfs_set_event_callback(
        g.g,
        event_callback_wrapper,
        event_bitmask,
        0,
        ptr::null(),
    );

    let eh = EventHandle { eh };
    g.callbacks.insert(eh, callback);

    Ok(eh)
}

pub fn delete_event_callback(g: Handle, h: EventHandle) {
    g.callbacks.remove(h);
}
