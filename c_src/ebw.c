#include "ebw.h"
#include "c_binary.h"
#include "string.h"

int load_ebw(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    debug_write("\nNew session\n-----------\n");
    return 0;
}


ERL_NIF_TERM unwrapper(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    int n;
    const ERL_NIF_TERM* elements;
    char name[20];

    if(!enif_get_tuple(env, argv, &n, &elements)
        || !enif_get_atom(env, elements[0], name,  ERL_NIF_LATIN1)
    )
        return enif_make_badarg(env);

    if(!strcmp("daxpy", name)){
        int n; double da; const c_binary dx; int incx; c_binary dy; double incy;
        if (!read_args(env, "int,double,c_binary,incx,c_binary,double", &n, &da, &dx, &incx, &dy, &incy)
        ) return enif_make_badarg(env); 
        daxpy(n, da, dx, incx, dy, incy);
    }

}

ErlNifFunc nif_funcs[] = { 
    {"dirty_unwrapper", 1, unwrapper},
    {"clean_unwrapper", 1, unwrapper}
};


ERL_NIF_INIT(ebw, nif_funcs, load_ebw, NULL, NULL, NULL)