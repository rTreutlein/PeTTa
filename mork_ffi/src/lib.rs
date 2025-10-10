use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::sync::{Mutex, OnceLock};
use std::collections::{BTreeSet, HashSet};
use std::time::Instant;
use mork_frontend::bytestring_parser::{Parser, ParserError, Context};
use mork::{expr, prefix, sexpr};
use mork::prefix::Prefix;
use mork::space::{transitions, unifications, writes, Space, ParDataParser};
use mork_bytestring::{item_byte, Tag, Expr, ExprZipper};

// ---------- Global space (init once) ----------
static GLOBAL_SPACE: OnceLock<Mutex<Space>> = OnceLock::new();

//Get it in the commands with a mutex:
fn get_space() -> &'static Mutex<Space>
{
    GLOBAL_SPACE.get_or_init(||
    {
        let mut s = Space::new();
        Mutex::new(s)
    })
}

fn parse_sexpr(s: &Space, r: &[u8], buf: *mut u8) -> Result<(Expr, usize), ParserError> {
    let mut it = Context::new(r);
    let mut parser = ParDataParser::new(&s.sm);
    let mut ez = ExprZipper::new(Expr { ptr: buf });
    parser.sexpr(&mut it, &mut ez).map(|_| (Expr { ptr: buf }, ez.loc))
}

// ---------- FFI ----------
#[unsafe(no_mangle)]
pub extern "C" fn rust_mork(command: *const c_char, input: *const c_char) -> *mut c_char
{
    if command.is_null() || input.is_null()
    {
        return std::ptr::null_mut();
    }
    let cmd = match unsafe { CStr::from_ptr(command) }.to_str()
    {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };
    let inp = match unsafe { CStr::from_ptr(input) }.to_str()
    {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };
    // Ensure global space exists (lazy)
    let _ = get_space();
    let mut out = String::new();
    let mut handled = false;
    if cmd.eq_ignore_ascii_case("add-atoms") {
        // Load more S-exprs into the existing Space
        let space = get_space();
        let res = {
            let mut s = match space.lock() { Ok(g) => g, Err(_) => {
                return CString::new("ERR: space poisoned").unwrap().into_raw();
            }};
            s.load_all_sexpr(inp.as_bytes())
        };
        out = match res {
            Ok(_)  => "OK: loaded".to_string(),
            Err(e) => format!("ERR: {e:?}"),
        };
        handled = true;
    }
    else if cmd.eq_ignore_ascii_case("mm2-exec")
    {
        let space = get_space();
        let mut s = match space.lock() { Ok(g) => g, Err(_) =>
        {
                return CString::new("ERR: space poisoned").unwrap().into_raw();
        }};
        let num: usize = match inp.trim().parse()
        {
            Ok(n) => n,
            Err(_) => 1,
        };
        s.metta_calculus(num);
    }
    else if cmd.eq_ignore_ascii_case("get-atoms")
    {
        out = "space geting queried:".to_string();
        let space = get_space();
        let res = {
            let guard = match space.lock()
            {
                Ok(g) => g,
                Err(_) => return CString::new("ERR: space poisoned").unwrap().into_raw(),
            };
            let s: &Space = &*guard;
            let mut buf: Vec<u8> = Vec::new();
            // Dump every expression as S-expr into buf
            match s.dump_all_sexpr(&mut buf)
            {
                Ok(_)  => String::from_utf8(buf).map_err(|_| "utf8 decode failed".to_string()),
                Err(e) => Err(format!("dump error: {e:?}")),
            }
        };
        out = res.unwrap_or_else(|e| format!("ERR: {e}"));
        handled = true;
    }
    else if cmd.eq_ignore_ascii_case("match") {
        let space = get_space();
        let res = {
            let guard = match space.lock() {
                Ok(g) => g,
                Err(_) => return CString::new("ERR: space poisoned").unwrap().into_raw(),
            };
            let s: &Space = &*guard;
            // ---- Parse the input string into an Expr ----
            let mut vbuf = [0u8; 4096];
            let (parsed_expr, used) = match parse_sexpr(s, inp.as_bytes(), vbuf.as_mut_ptr()) {
                Ok(ok) => ok,
                Err(_) => return CString::new("ERR: parse failed").unwrap().into_raw(),
            };
            // ---- Now dump the query result ----
            let mut v: Vec<u8> = Vec::new();
            let _written: usize = s.dump_sexpr(parsed_expr, parsed_expr, &mut v);
            // ---- Convert to UTF-8 ----
            match String::from_utf8(v) {
                Ok(s) => s,
                Err(_) => return CString::new("ERR: utf8 decode failed").unwrap().into_raw(),
            }
        };
        out = res;
        handled = true;
    }
    if !handled
    {
        out = inp.to_string();
    }
    CString::new(out).unwrap().into_raw()
}

#[unsafe(no_mangle)]
pub extern "C" fn rust_string_free(ptr: *mut c_char)
{
    if !ptr.is_null()
    {
        unsafe { drop(CString::from_raw(ptr)); }
    }
}
