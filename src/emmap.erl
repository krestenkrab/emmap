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


close(_Ref) ->
    ok.

-spec open(File::string(),
          Offset::pos_integer(),
          Length::pos_integer(),
          Options::[ read|write|direct|nocache|private|shared ]) ->
                 {ok, term()} | {error, term()}.

open(_FileName,_Off,_Len,_Options) ->
    <<>>.

pread(_File,_Off,_Len) ->
    {ok, <<>>}.

pwrite(_File,_Off,_Data) ->
    ok.



-ifdef(TEST).

simple_test() ->
    {ok, File} = file:open("test.data", [raw, write]),
    ok = file:write(File, <<"abcd">>),
    ok = file:close(File),

    {ok, MFile} = emmap:open("test.data", 0, 4, [direct]),
    {ok, <<"cd">>} = emmap:pread(MFile, 2, 2),
    {error, eacces} = emmap:pwrite(MFile, 2, <<"xx">>),
    ok = emmap:close(MFile).


-endif.
