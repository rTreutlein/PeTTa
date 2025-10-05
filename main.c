// main.c
#include <SWI-Prolog.h>
#include <string.h>
#include <dlfcn.h>

// Preload Rust lib so its symbols are globally visible
__attribute__((constructor))
static void preload_mork(void) {
  dlopen("/home/nartech/MORK/target/release/libmork_ffi.so",
         RTLD_LAZY | RTLD_GLOBAL);
}

// Rust functions we call
extern char *rust_mork(const char *command, const char *input);
extern void  rust_string_free(char *ptr);

// pl_mork(+In, -Out)
static foreign_t pl_mork(term_t a0, term_t a1, term_t a2) {
    char *command;
    size_t lenc;
    if (!PL_get_nchars(a0, &lenc, &command,
                       CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|REP_UTF8))
        return FALSE;
    char *in;
    size_t leni;
    if (!PL_get_nchars(a1, &leni, &in,
                       CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|REP_UTF8))
        return FALSE;
    char *res = rust_mork(command, in);
    if (!res)
        return FALSE;
    int ok = PL_unify_chars(a2, PL_STRING|REP_UTF8, (size_t)-1, res);
    rust_string_free(res);
    return ok;
}

// Called by SWI-Prolog on load
install_t install(void) {
    PL_register_foreign("mork", 3, pl_mork, 0);
}
