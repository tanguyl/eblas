#include "eblas.h"
#include "string.h"
#include <cblas.h>

#include "string.h"

//Various utility functions
//--------------------------------

int translate(ErlNifEnv* env, const ERL_NIF_TERM* terms, const etypes* format, ...){
    va_list valist;
    va_start(valist, format);
    int valid = 1;

    for(int curr=0; format[curr] != e_end; curr++){
        debug_write("Unwrapping term number %i\n", curr);
        switch(format[curr]){
            case e_int:
                valid = enif_get_int(env, terms[curr], va_arg(valist, int*));
            break;
            case e_lint:
                valid = enif_get_int64(env, terms[curr], va_arg(valist, ErlNifSInt64*));
            break;
            
            case e_float:
                double val;
                float* dest = va_arg(valist, float*);
                valid = enif_get_double(env, terms[curr], &val);
                if(valid)
                    *dest = (float) val;    
            break;
            case e_double:
                valid = enif_get_double(env, terms[curr], va_arg(valist, double*));
            break;

            case e_ptr:
                valid = get_c_binary(env, terms[curr], va_arg(valist, c_binary*));
            break;
            case e_cste_ptr:
                valid = get_cste_binary(env, terms[curr], va_arg(valist, cste_c_binary*));
            break;

            default:
                valid = 0;
            break;
        }

        if(!valid){
            va_end(valist);
            return curr + 1;
        }
    }
    
    va_end(valist);
    return 0;
}

// C_binary definitions/functions

//Used for debug purpose.
//Likely thread unsafe.
//Usage: debug_write("A double: %lf, an int:%d", double_val, int_val);
int debug_write(const char* fmt, ...){
    FILE* fp = fopen("debug.txt", "a");
    va_list args;

    va_start(args, fmt);
    vfprintf(fp, fmt, args);
    va_end(args);

    fclose(fp);
    return 1;
}

int get_c_binary(ErlNifEnv* env, const ERL_NIF_TERM term, c_binary* result){
    int arity;
    const ERL_NIF_TERM* terms;
    void* resource;

    // Return false if: incorect record size, resource size does not match
    int b =  enif_get_tuple(env, term, &arity, &terms)
                && arity == 4
                && enif_get_uint(env, terms[1], &result->size)
                && enif_get_uint(env, terms[2], &result->offset)
                && enif_get_resource(env, terms[3], c_binary_resource, &resource)
                && result->size == enif_sizeof_resource(resource);

    result->ptr = (unsigned char*) resource;
    
    return b;
}

int get_cste_binary(ErlNifEnv* env, const ERL_NIF_TERM term, cste_c_binary* result){
    if(enif_is_binary(env, term)){
        ErlNifBinary ebin;
        if(!enif_inspect_binary(env, term, &ebin))
            return 0;
        
        result->size    = ebin.size;
        result->offset  = 0;
        result->ptr     = ebin.data;
    }
    else{
        c_binary cbin;
        if(!get_c_binary(env, term, &cbin))
            return 0;

        result->size    = cbin.size;
        result->offset  = cbin.offset;
        result->ptr     = (const unsigned char*) cbin.ptr;
    }
    return 1;
}


ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    int size = 0;
    if(!enif_get_int(env, argv[0], &size)) return enif_make_badarg(env);

    void* ptr = enif_alloc_resource(c_binary_resource, size);
    ERL_NIF_TERM resource = enif_make_resource(env, ptr);
    enif_release_resource(ptr);
    return resource; 
}

ERL_NIF_TERM copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    ErlNifBinary bin;
    c_binary cbin;

    if(!enif_inspect_binary(env, argv[0], &bin)|| !get_c_binary(env, argv[1], &cbin))
         return enif_make_badarg(env);

    memcpy(cbin.ptr + cbin.offset, bin.data, bin.size);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    c_binary cbin;
    ErlNifBinary bin;
    unsigned size;

    if(!enif_get_uint(env, argv[0], &size)
        || !get_c_binary(env, argv[1], &cbin)
        || !enif_alloc_binary(size, &bin))
        return enif_make_badarg(env);

    memcpy(bin.data, cbin.ptr + cbin.offset, size);

    return enif_make_binary(env, &bin);
}

// UNWRAPPER

// https://stackoverflow.com/questions/7666509/hash-function-for-string
unsigned long hash(char *str){
    unsigned long hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}


ERL_NIF_TERM unwrapper(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    int narg;
    const ERL_NIF_TERM* elements;
    char name[20];

    if(!enif_get_tuple(env, *argv, &narg, &elements)
        || !enif_get_atom(env, elements[0], name, 20, ERL_NIF_LATIN1)
    ){
        return enif_make_badarg(env);
    }

    
    int error;
    switch(hash(name)){
        case saxpy: {
            int n; float da; cste_c_binary dx; int incx; c_binary dy; int incy;
            if( !(error = translate(env, elements+1, (etypes[]) {e_int, e_float, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &da, &dx, &incx, &dy, &incy)))  
                cblas_saxpy(n, da, get_cste_ptr(dx), incx, get_ptr(dy), incy);
        break; }
        case daxpy: {
            int n; double da; cste_c_binary dx; int incx; c_binary dy; int incy;
            if( !(error = translate(env, elements+1, (etypes[]) {e_int, e_double, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &da, &dx, &incx, &dy, &incy)))  
                cblas_daxpy(n, da, get_cste_ptr(dx), incx, get_ptr(dy), incy);
        break; }
        case caxpy: {
            int n; cste_c_binary da; cste_c_binary dx; int incx; c_binary dy; int incy;
            if( !(error = translate(env, elements+1, (etypes[]) {e_int, e_cste_ptr, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &da, &dx, &incx, &dy, &incy)))  
                cblas_caxpy(n, get_cste_ptr(da), get_cste_ptr(dx), incx, get_ptr(dy), incy);
        break; }
        case zaxpy: {
            int n; cste_c_binary da; cste_c_binary dx; int incx; c_binary dy; int incy;
            if( !(error = translate(env, elements+1, (etypes[]) {e_int, e_cste_ptr, e_cste_ptr, e_int, e_ptr, e_int, e_end}, &n, &da, &dx, &incx, &dy, &incy)))  
                cblas_zaxpy(n, get_cste_ptr(da), get_cste_ptr(dx), incx, get_ptr(dy), incy);
        break; }

        default:
            debug_write("%s:  %lu,\n", name, hash(name));
            error = -1;
        break;
    }

    return error? enif_make_badarg(env) : enif_make_atom(env, "ok");
}

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    c_binary_resource = enif_open_resource_type(env, "c_binary", "c_binary_resource", NULL, ERL_NIF_RT_CREATE, NULL);
    debug_write("\nNew session\n-----------\n");
    return 0;
}

ErlNifFunc nif_funcs[] = { 
    {"new_nif", 1, new},
    {"copy_nif", 2, copy},
    {"bin_nif", 2, to_binary},

    {"dirty_unwrapper", 1, unwrapper},
    {"clean_unwrapper", 1, unwrapper}
};


ERL_NIF_INIT(eblas, nif_funcs, load, NULL, NULL, NULL)