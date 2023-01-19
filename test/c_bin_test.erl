-module(c_bin_test).
-include_lib("eunit/include/eunit.hrl").

-record(c_binary, {size, offset, resource}).

copy_test()->
    Cbin = c_binary:new(4),
    Bin  = <<1:8/native-integer>>,
    lists:foreach(
        fun(Offset)->
            case catch c_binary:copy(Bin, c_binary:shift(Offset, Cbin)) of
                ok when Offset>=0, Offset =< 3 -> ok;
                {'EXIT',_} when Offset > 3 -> ok;
                M -> throw(M)
            end
        end,
        [0,1,2,3,4]
    ).

to_bin_test()->
    Cbin = c_binary:new(4),
    Bin  = <<1:8/native-integer, 2:8/native-integer, 3:8/native-integer, 4:8/native-integer>>,
    Zero = <<0:32/native-integer>>,

    c_binary:copy(Zero, Cbin),
    Zero = c_binary:to_bin(Cbin),
    c_binary:copy(Bin, Cbin),
    Bin = c_binary:to_bin(Cbin),

    
    Part = <<1:8/native-integer>>,
    Full = <<0:16/native-integer, 1:8/native-integer, 0:8/native-integer>>,
    c_binary:copy(Zero, Cbin),
    c_binary:copy(Part, c_binary:shift(2, Cbin)),

    Part = c_binary:to_bin(1, c_binary:shift(2, Cbin)),
    Full = c_binary:to_bin(Cbin).


copy_to_bin_test()->
    Cbin = c_binary:new(1),
    Bin  = <<1:8/native-integer>>,
    c_binary:copy(Bin, Cbin),
    Bin = c_binary:to_bin(Cbin).

new_test()->
    {c_binary, 0, 0, _} = c_binary:new(0),
    {c_binary, 1, 0, _} = c_binary:new(1),
    case catch c_binary:new(-1) of
        {'EXIT',_} -> ok;
        _ -> throw(fail)
    end.

shift_test()->
    Max = 10,
    CBin = c_binary:new(Max),
    Shifts = [-1,0,1,Max-1,Max,Max+1],
    Results = lists:map(fun(I)->{I>=0 andalso I =< Max, catch c_binary:shift(I, CBin)} end, Shifts),

    lists:foreach(fun(C)->
        case C of
            {true, #c_binary{}} -> ok;
            {false, {'EXIT',_}} -> ok;
            _ -> throw(error)
        end
        end,
        Results
    ).