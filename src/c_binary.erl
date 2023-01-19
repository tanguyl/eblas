-module(c_binary).

-record(c_binary, {size, offset, resource}).

-export([new/1, shift/2, copy/2, to_bin/1, to_bin/2]).

-on_load(on_load/0).

on_load()->
    % Again, from https://github.com/dgud/blas/blob/master/src/blasd_raw.erl
    LibBaseName = "c_binary",
    PrivDir = code:priv_dir(ewb),
    Lib = filename:join([PrivDir, LibBaseName]),
    erlang:load_nif(Lib, {0.1}).


shift(Shift,C_binary=#c_binary{size=Size, offset=Offset}) when Shift+Offset >=0, Shift+Offset =< Size ->
    C_binary#c_binary{offset=Offset+Shift}.

% SIZE IN BYTES
new(Size) when is_integer(Size) andalso Size >= 0->
    #c_binary{size=Size, offset=0, resource=new_nif(Size)};
new(Binary) when is_binary(Binary) ->
    C = new(size(Binary)),
    copy(Binary, C),
    C.


new_nif(_)->
    nif_not_loaded.

copy(Binary, C_binary=#c_binary{offset=C_offset, size=C_size}) when size(Binary) =< C_size - C_offset ->
    copy_nif(Binary, C_binary).

copy_nif(_, _)->
    nif_not_loaded.


to_bin(C_binary=#c_binary{offset=Offset, size=C_size}) ->
    to_bin(C_size-Offset, C_binary).

to_bin(B_size, C_binary=#c_binary{offset=Offset, size=C_size}) when B_size>=0, B_size =< C_size-Offset->
    bin_nif(B_size, C_binary).

bin_nif(_,_)->
    nif_not_loaded.