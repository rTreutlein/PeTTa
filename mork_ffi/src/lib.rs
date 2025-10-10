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

#[inline]
fn get_space() -> &'static Mutex<Space>
{
    GLOBAL_SPACE.get_or_init(||
    {
        let mut s = Space::new();
        Mutex::new(s)
    })
}

pub fn parse_sexpr(r: &[u8], buf: *mut u8) -> Result<(Expr, usize), ParserError> {
    let space = get_space();
    let mut s = match space.lock() {
        Ok(g) => g,
        Err(_) => panic!("Space poisoned"),
    };

    let mut it = Context::new(r);
    let mut parser = ParDataParser::new(&s.sm);
    let mut ez = ExprZipper::new(Expr { ptr: buf });

    parser.sexpr(&mut it, &mut ez)
        .map(|_| (Expr { ptr: buf }, ez.loc))
}

// Optional explicit (re)init helper if you want to override contents.
fn init_space() -> Result<(), String>
{
    let space = get_space();
    let mut s = space.lock().map_err(|_| "space poisoned")?;
    *s = Space::new();
    Ok(())
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
    // ---- commands that use/affect the global Space ----
    if cmd.eq_ignore_ascii_case("init") {
        // Reinitialize global Space with provided S-exprs
        match init_space() {
            Ok(_)  => out = "OK: space (re)initialized".to_string(),
            Err(e) => out = format!("ERR: {e}"),
        }
        handled = true;
    }
    else if cmd.eq_ignore_ascii_case("load") {
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
    else if cmd.eq_ignore_ascii_case("exec")
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
    else if cmd.eq_ignore_ascii_case("getatoms")
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
    else if cmd.eq_ignore_ascii_case("query")
    {
        let space = get_space();
        let res = {
            let guard = match space.lock()
            {
                Ok(g) => g,
                Err(_) => return CString::new("ERR: space poisoned").unwrap().into_raw(),
            };
            let s: &Space = &*guard;
            let mut v: Vec<u8> = Vec::new();
            // pattern from `inp`, project `_1`
            let (e, used) = parse_sexpr(inp.as_bytes(), v.as_mut_ptr()).unwrap();
            let _written: usize = s.dump_sexpr(e, expr!(s, "$V1"), &mut v);
            
            
            
            // Convert to UTF-8 String
            String::from_utf8(v).map_err(|_| "utf8 decode failed".to_string())
        };
        out = res.unwrap_or_else(|e| format!("ERR: {e}"));
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
