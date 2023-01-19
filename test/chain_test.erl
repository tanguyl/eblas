-module(chain_test).
-include_lib("eunit/include/eunit.hrl").

chain_test()->
    Even_list = [1.0,2.0,3.0,4.0,5.0,6.0],
    lists:foreach(
        fun({Type, Expected_size})->
            Binary = chain:ltb(Type, Even_list),
            Expected_size = size(Binary),
            Even_list = chain:btl(Type, Binary)
        end,
        [{s,24},{d,48},{c,24},{z,48}]
    ).

odd_ltb_test()->
    Odd_list = [1.0,2.0,3.0,4.0,5.0],
    lists:foreach(
        fun(Type)->
            case catch chain:ltb(Type, Odd_list) of
                {'EXIT', _} -> ok
            end
        end,
        [c,z]
    ).