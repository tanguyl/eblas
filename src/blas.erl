-module(ebw).
-include("blas_enums.hrl").

-export([run/1, run/2]).

-on_load(on_load/0).

on_load()->
    % Again, from https://github.com/dgud/blas/blob/master/src/blasd_raw.erl
    LibBaseName = "c_binary",
    PrivDir = code:priv_dir(blas),
    Lib = filename:join([PrivDir, LibBaseName]),
    erlang:load_nif(Lib, {0.1}).

run(Wrapped)->
    dirty_unwrapper(Wrapped).


run(Wrapped, DirtyTest) when is_tuple(Wrapped) ->
    Dirty = DirtyTest(Wrapped),
    if  Dirty -> dirty_unwrapper(Wrapped);
        true -> clean_unwrapper(Wrapped)
    end.

dirty_unwrapper(_) -> nif_not_loaded.
clean_unwrapper(_) -> nif_not_loaded.

