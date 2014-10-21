-module(emmap).

-export([open/2, open/4, close/1, pread/3, pwrite/3, read/2, read_line/1, position/2]).
-on_load(init/0).

-ifdef(TEST).
-export([simple_test/0]).
-endif.

-include_lib("kernel/include/file.hrl").

-type open_option() ::
    read | write
  | direct
  | lock | nolock
  | private | shared
  | nocache | auto_unlink
  .

-type mmap_file() :: #file_descriptor{}.

init() ->
    case code:priv_dir(emmap) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "emmap_nifs"]);
                _ ->
                    SoName = filename:join("../priv", "emmap_nifs")
            end;
        Dir ->
            SoName = filename:join(Dir, "emmap_nifs")
    end,
    erlang:load_nif(SoName, 0).


open(FileName, Options) ->
    case file:read_file_info(FileName) of
        {ok, FileInfo} ->
            open(FileName, 0, FileInfo#file_info.size, Options);
        Error ->
            Error
    end.

-spec open(File::string(),
          Offset::pos_integer(),
          Length::pos_integer(),
          Options::[ open_option() ]) ->
                 {ok, mmap_file()} | {error, term()}.

open(FileName, Off, Len, Options) ->
    case open_nif(FileName, Off, Len, Options) of
        {ok, Mem} ->
            {ok, #file_descriptor{ module=?MODULE, data=Mem }};
        Error ->
            Error
    end.

open_nif(_,_,_,_) ->
     {ok, <<>>}.

-spec close(File::mmap_file()) -> ok.

close(#file_descriptor{ module=?MODULE, data=Mem }) ->
    close_nif(Mem).

close_nif(_) ->
    ok.

-spec pread(File::mmap_file(), Offset::pos_integer(), Length::pos_integer()) ->
                   {ok, binary()} | {error, term()} | eof.

pread(#file_descriptor{ module=?MODULE, data=Mem }, Off, Len) ->
    pread_nif(Mem, Off, Len).

pread_nif(_,_,_) ->
    {ok, <<>>}.

-spec read(File::mmap_file(), Length::pos_integer()) ->
                   {ok, binary()} | {error, term()} | eof.

read(#file_descriptor{ module=?MODULE, data=Mem }, Len) ->
    read_nif(Mem, Len).

read_nif(_,_) ->
    {ok, <<>>}.


-spec read_line(File::mmap_file()) ->
                   {ok, binary()} | {error, term()} | eof.

read_line(#file_descriptor{ module=?MODULE, data=Mem }) ->
    read_line_nif(Mem).

read_line_nif(_) ->
    {ok, <<>>}.


-spec pwrite(File::mmap_file(), Position::pos_integer(), Data::binary()) ->
                    ok | {error, term()}.

pwrite(#file_descriptor{ module=?MODULE, data=Mem }, Off, Data) ->
    pwrite_nif(Mem, Off, Data).

pwrite_nif(_,_,_) ->
    ok.

-spec position(File::mmap_file(),
               Position::pos_integer() | {bof|cur|eof, Position::integer()} ) ->
                    {ok, pos_integer()} | {error, term()}.
position(#file_descriptor{ module=?MODULE, data=Mem}, At)
  when is_integer(At) ->
    position_nif(Mem, bof, At);
position(#file_descriptor{ module=?MODULE, data=Mem}, From)
  when From == 'bof'; From == 'cur'; From == 'eof' ->
    position_nif(Mem, From, 0);
position(#file_descriptor{ module=?MODULE, data=Mem}, {From, Off})
  when From == 'bof'; From == 'cur'; From == 'eof' ->
    position_nif(Mem, From, Off).

position_nif(_,_From,_Off) ->
    {ok, 0}.


-ifdef(TEST).

simple_test() ->
    {ok, File} = file:open("test.data", [raw, write]),
    ok = file:write(File, <<"abcd">>),
    ok = file:close(File),

    %% with direct+shared, the contents of a binary may change
    {ok, MFile} = emmap:open("test.data", 0, 4, [direct, shared, nolock]),
    {ok, Mem} = file:pread(MFile, 2, 2),
    <<"cd">> = Mem,
    {error, eacces} = file:pwrite(MFile, 2, <<"xx">>),

    {ok, MFile2} = emmap:open("test.data", 0, 4, [read, write, shared]),
    ok = file:pwrite(MFile2, 2, <<"xx">>),
    {ok, <<"xx">>} = file:pread(MFile, 2, 2),

    %% Woot!
    <<"xx">> = Mem,

    {ok, 0} = file:position(MFile, {cur, 0}),
    {ok, <<"ab">>} = file:read(MFile, 2),
    {ok, <<"xx">>} = file:read(MFile, 2),

    file:close(MFile),
    file:close(MFile2) .


-endif.
