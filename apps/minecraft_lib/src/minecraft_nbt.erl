
-module(minecraft_nbt).

-export([get_type/1, get_value/1]).
-export([parse_nbt/1, parse_test/2]).

-include_lib("minecraft_lib/include/minecraft_nbt.hrl").



%%%%% ------------------------------------------------------- %%%%%


get_type(#nbt_value{ type = Type }) -> Type;
get_type(#nbt_named_value{ type = Type }) -> Type.


get_value(#nbt_value{ value = Value }) -> Value;
get_value(#nbt_named_value{ value = Value }) -> Value.


%%%%% ------------------------------------------------------- %%%%%


-spec parse_nbt( binary() ) -> {ok, #nbt_named_value{}} | type:error().

parse_nbt(Data) ->
    try
        { #nbt_named_value{ type = ?NBTTag_compound } = Value, <<>> } = decode_name_value(1, Data),
        {ok, Value}
    catch exit:Why ->
        Why
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_name_value( non_neg_integer(), binary() ) -> { nbt_type(), binary() }.

decode_name_value(Depth, <<Tag:8/unsigned-integer, Rest/binary>>) ->
    { Name, MoreRest } = decode_name(Rest),
    { #nbt_value{} = Value, MoreRest2 } = decode_value(Tag, Depth, MoreRest),
    { make_named(Name, Value), MoreRest2 }. 


decode_name(<<NameLen:16/unsigned-integer, Rest/binary>>) ->
    <<Name:NameLen/binary, MoreRest/binary>> = Rest,
    { unicode:characters_to_list(Name), MoreRest }.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_value( nbt_tag_type(), non_neg_integer(), binary() ) -> { nbt_type(), binary() }.

decode_value(_, Depth, _)
        when Depth > 11  ->
    throw({error, bad_nbt_data});

decode_value(?NBTTag_byte, _Depth, <<Byte:8/signed, Rest/binary>>) ->
    { #nbt_value{ type = ?NBTTag_byte, value = Byte }
    , Rest
    };
    
decode_value(?NBTTag_short, _Depth, <<Short:16/signed, Rest/binary>>) ->
    { #nbt_value{ type = ?NBTTag_short, value = Short }
    , Rest
    };
    
decode_value(?NBTTag_int, _Depth, <<Int:32/signed, Rest/binary>>) ->
    { #nbt_value{ type = ?NBTTag_int, value = Int }
    , Rest
    };
    
decode_value(?NBTTag_long, _Depth, <<Long:64/signed, Rest/binary>>) ->
    { #nbt_value{ type = ?NBTTag_long, value = Long }
    , Rest
    };
    
decode_value(?NBTTag_float, _Depth, <<Float:32/signed-float, Rest/binary>>) ->
    { #nbt_value{ type = ?NBTTag_float, value = Float }
    , Rest
    };
    
decode_value(?NBTTag_double, _Depth, <<Double:64/signed-float, Rest/binary>>) ->
    { #nbt_value{ type = ?NBTTag_double, value = Double }
    , Rest
    };
    
    
decode_value(?NBTTag_byte_array, _Depth, <<Length:32/signed, Rest/binary>>) ->
    <<Bin:Length/binary, MoreRest/binary>> = Rest,
    { #nbt_value{ type = ?NBTTag_byte_array, value = Bin }
    , MoreRest
    };
    
decode_value(?NBTTag_string, _Depth, <<Length:16/signed, Rest/binary>>) ->
    <<Bin:Length/binary, MoreRest/binary>> = Rest,
    { #nbt_value{ type = ?NBTTag_string, value = unicode:characters_to_list(Bin) }
    , MoreRest
    };
    
decode_value(?NBTTag_int_array, _Depth, <<Length:32/signed, Rest/binary>>) ->
    <<Bin:Length/binary, MoreRest/binary>> = Rest,
    { #nbt_value{ type = ?NBTTag_double, value = decode_int_array([], Bin) }
    , MoreRest
    };
    
    
decode_value(?NBTTag_list, Depth, <<ListType:8/unsigned-integer, Length:32/signed-integer, Rest/binary>>) ->
    { Items, MoreRest } = decode_list(ListType, Depth, Length, [], Rest),
    { #nbt_value{ type = ?NBTTag_list, value = Items }
    , MoreRest
    };
    
decode_value(?NBTTag_compound, Depth, Rest) ->
    { Items, MoreRest } = decode_compound(Depth, [], Rest),
    { #nbt_value{ type = ?NBTTag_compound, value = Items }
    , MoreRest
    };
  

decode_value(_, _, _) ->
    throw({error, bad_nbt_data}).
 
 
%%%%% ------------------------------------------------------- %%%%%


decode_int_array(Acc, <<>>) ->
    lists:reverse(Acc);
    
decode_int_array(Acc, <<Result:32/signed-integer, Rest/binary>>) ->
    decode_int_array([Result | Acc], Rest);
    
decode_int_array(_, _) ->
    throw({error, incomplete_int_array}).
    

%%%%% ------------------------------------------------------- %%%%%


-spec decode_list( nbt_value_type(), non_neg_integer(), non_neg_integer(), [nbt_type()], binary() ) -> { nbt_type(), binary }.

decode_list(_ListType, _Depth, _Length, _, <<>>) ->
    throw({error, incomplete_list});
    
decode_list(_ListType, _Depth, 0, Acc, Binary) ->
    { lists:reverse(Acc), Binary };
    
decode_list(ListType, Depth, Length, Acc, Binary) ->
    { #nbt_value{} = Value, Rest } = decode_value(ListType, Depth + 1, Binary),
    decode_list(ListType, Depth, Length - 1, [Value | Acc], Rest).

    
%%%%% ------------------------------------------------------- %%%%%


decode_compound(_Depth, _Acc, <<>>) ->
    throw({error, incomplete_compound});
    
decode_compound(_Depth, Acc, <<?NBTTag_end>>) ->
    { lists:reverse(Acc), <<>> };
    
decode_compound(_Depth, Acc, <<?NBTTag_end, Rest/binary>>) ->
    { lists:reverse(Acc), Rest };
    
decode_compound(Depth, Acc, Binary) ->
    { #nbt_named_value{} = Value, Rest } = decode_name_value(Depth + 1, Binary),
    decode_compound(Depth, [Value | Acc], Rest).


%%%%% ------------------------------------------------------- %%%%%


make_named(Name, #nbt_value{ type = Type, value = Value }) ->
    #nbt_named_value{ name = Name, type = Type, value = Value }.


  
parse_test(IsGzip, Filename) ->
    {ok, Bin} = file:read_file(Filename),
    NewBin =    case IsGzip of
                    true    -> zlib:gunzip(Bin)
                ;   false   -> Bin
                end,
    parse_nbt(NewBin).
    