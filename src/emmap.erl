-module(emmap).

-export([open/4, close/1, pread/3, pwrite/3]).
-on_load(init/0).

-ifdef(TEST).
-export([simple_test/0]).
-endif.

-include_lib("kernel/include/file.hrl").

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


-spec open(File::string(),
          Offset::pos_integer(),
          Length::pos_integer(),
          Options::[ read|write|direct|nocache|private|shared ]) ->
                 {ok, term()} | {error, term()}.


open(FileName, Off, Len, Options) ->
    {ok, Mem} = open_nif(FileName, Off, Len, Options),
    {ok, #file_descriptor{ module=?MODULE, data=Mem }}.

open_nif(_,_,_,_) ->
     {ok, <<>>}.

close(#file_descriptor{ module=?MODULE, data=Mem }) ->
    close_nif(Mem).

close_nif(_) ->
    ok.

pread(#file_descriptor{ module=?MODULE, data=Mem }, Off, Len) ->
    pread_nif(Mem, Off, Len).

pread_nif(_,_,_) ->
    {ok, <<>>}.

pwrite(#file_descriptor{ module=?MODULE, data=Mem }, Off, Data) ->
    pwrite_nif(Mem, Off, Data).

pwrite_nif(_,_,_) ->
    ok.



-ifdef(TEST).

simple_test() ->
    {ok, File} = file:open("test.data", [raw, write]),
    ok = file:write(File, <<"abcd">>),
    ok = file:close(File),

    %% with direct+shared, the contents of a binary may change
    {ok, MFile} = emmap:open("test.data", 0, 4, [direct, shared]),
    {ok, Mem} = file:pread(MFile, 2, 2),
    <<"cd">> = Mem,
    {error, eacces} = file:pwrite(MFile, 2, <<"xx">>),

    {ok, MFile2} = emmap:open("test.data", 0, 4, [read, write, shared]),
    ok = file:pwrite(MFile2, 2, <<"xx">>),
    {ok, <<"xx">>} = file:pread(MFile, 2, 2),

    %% Woot!
    <<"xx">> = Mem,

    file:close(MFile),
    file:close(MFile2) .


-endif.
