
%%%%% ------------------------------------------------------- %%%%%
% nbt structure records


-define(NBTTag_end, 0).
-define(NBTTag_byte, 1).
-define(NBTTag_short, 2).
-define(NBTTag_int, 3).
-define(NBTTag_long, 4).
-define(NBTTag_float, 5).
-define(NBTTag_double, 6).
-define(NBTTag_byte_array, 7).
-define(NBTTag_string, 8).
-define(NBTTag_list, 9).
-define(NBTTag_compound, 10).
-define(NBTTag_int_array, 11).


%%%%% ------------------------------------------------------- %%%%%


-type nbt_value_type() :: ?NBTTag_byte | ?NBTTag_short | ?NBTTag_int
                        | ?NBTTag_long | ?NBTTag_float | ?NBTTag_double | ?NBTTag_byte_array
                        | ?NBTTag_string | ?NBTTag_list | ?NBTTag_compound | ?NBTTag_int_array.

-type nbt_tag_type() :: ?NBTTag_end | nbt_value_type().


-record(nbt_value,
    { type  :: nbt_value_type()
    , value :: term()
    }).
    
-record(nbt_named_value,
    { name  :: string()
    , type  :: nbt_value_type()
    , value :: term()
    }).
    
-type nbt_type() :: #nbt_value{} | #nbt_named_value{}.


%%%%% ------------------------------------------------------- %%%%%

