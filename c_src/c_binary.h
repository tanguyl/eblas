#ifndef C_BINARY_INCLUDED
#define C_BINARY_INCLUDED

#include "erl_nif.h"


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



#endif