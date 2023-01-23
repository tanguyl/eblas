#ifndef EWB_INCLUDED
#define EWB_INCLUDED
#include "erl_nif.h"
#include <cblas.h>

// Types translator
typedef enum types {e_int, e_lint, e_float, e_double, e_ptr, e_cste_ptr, e_end} etypes;
int translate(ErlNifEnv* env, const ERL_NIF_TERM* terms, const etypes* format, ...);


// C binary definition
// --------------------------------------------

typedef enum sizes {s_bytes=4, d_bytes=8, c_bytes=8, z_bytes=16} bytes_sizes;

typedef struct{
    unsigned int size;
    unsigned int offset;
    unsigned char* ptr;
} c_binary;
inline void* get_ptr(c_binary cb){return (void*) cb.ptr + cb.offset;}

typedef struct{
    unsigned int size;
    unsigned int offset;
    const unsigned char* ptr;
} cste_c_binary;
inline const void* get_cste_ptr(cste_c_binary cb){return (void*) cb.ptr + cb.offset;}

int get_c_binary(ErlNifEnv* env, const ERL_NIF_TERM term, c_binary* result);
int get_cste_binary(ErlNifEnv* env, const ERL_NIF_TERM term, cste_c_binary* result);

// Private stuff
int debug_write(const char* fmt, ...);
ErlNifResourceType *c_binary_resource;
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
int unload(ErlNifEnv* caller_env, void* priv_data);

ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);
ERL_NIF_TERM copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);
ERL_NIF_TERM to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);



// Blas wrapper
// --------------------------------------------

unsigned long hash(char *str);
int load_ebw(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
ERL_NIF_TERM unwrapper(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv);

typedef enum BLAS_NAMES {saxpy=210727551034,daxpy=210709762219,caxpy=210708576298,zaxpy=210735852481} blas_names;

#endif