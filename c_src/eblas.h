#ifndef EWB_INCLUDED
#define EWB_INCLUDED
#include "erl_nif.h"
#include <cblas.h>

// Types translator
typedef enum types {e_int, e_double, e_ptr, e_none} etypes;
int translate(ErlNifEnv* env, const ERL_NIF_TERM* terms, const etypes* format, ...);


// C binary definition
// --------------------------------------------

typedef enum sizes {s_bytes: 4, d_bytes:8, c_bytes:8, z_bytes:16} bytes_sizes;

typedef struct{
    unsigned int size;
    unsigned int offset;
    char* ptr;
} c_binary;

typedef struct{
    const unsigned int size;
    char* const ptr;
} const_binary;


int get_c_binary(ErlNifEnv* env, const ERL_NIF_TERM term, c_binary* result);

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

typedef enum BLAS_NAMES {daxpy=210709762219} blas_names;

#endif