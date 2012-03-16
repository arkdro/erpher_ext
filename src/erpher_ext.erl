%%%
%%% erpher_ext: server for unsorted tasks
%%%
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2012-03-16 15:40
%%% @license MIT
%%% @doc server for unsorted tasks
%%% 

-module(erpher_ext).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([
         reload_config_signal/0
        ]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kernel/include/file.hrl").

-include("erpher_ext.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    prepare_updated_config(),
    C = erpher_ext_conf:get_config(),
    New = prepare_all(C),
    process_flag(trap_exit, true), % to remove pid
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, New#ext.debug, run, 1),
    {ok, New, ?PTIME}.

%------------------------------------------------------------------------------
-spec handle_call(any(), any(), #ext{}) ->
                         {stop, normal, ok, #ext{}}
                             | {reply, any(), #ext{}}.
%%
%% Handling call messages
%% @since 2011-12-20 13:34
%%
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};

handle_call(status, _From, St) ->
    {reply, St, St};

%% @doc set new debug level for facility
handle_call({set_debug_item, Facility, Level}, _From, St) ->
    % no api for this, use message passing
    New = mpln_misc_run:update_debug_level(St#ext.debug, Facility, Level),
    {reply, St#ext.debug, St#ext{debug=New}};

handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _N}, St#ext.debug, run, 2),
    {reply, {error, unknown_request}, St}.

%------------------------------------------------------------------------------
-spec handle_cast(any(), #ext{}) -> any().
%%
%% Handling cast messages
%% @since 2011-12-20 13:34
%%
handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast(reload_config_signal, St) ->
    New = process_reload_config(St),
    {noreply, New};

handle_cast(_Other, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Other},
        St#ext.debug, run, 2),
    {noreply, St}.

%------------------------------------------------------------------------------
terminate(_, #ext{pid_file=File} = State) ->
    mpln_misc_run:remove_pid(File),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE}, State#ext.debug, run, 1),
    ok.

%------------------------------------------------------------------------------
-spec handle_info(any(), #ext{}) -> any().
%%
%% Handling all non call/cast messages
%%
handle_info(timeout, St) ->
    mpln_p_debug:pr({?MODULE, 'info timeout', ?LINE}, St#ext.debug, run, 3),
    New = periodic_check(St),
    {noreply, New};

handle_info(periodic_check, State) ->
    mpln_p_debug:pr({?MODULE, 'info periodic_check', ?LINE},
                    State#ext.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'info_other', ?LINE, _Req},
                    State#ext.debug, run, 2),
    {noreply, State}.

%------------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts receiver gen_server
%% @since 2011-12-30 13:34
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
%%
%% @doc starts receiver gen_server with pre-defined config
%% @since 2011-12-20 13:34
%%
-spec start_link() -> any().

start_link() ->
    start_link(?CONF).

%%
%% @doc starts receiver gen_server with given config
%% @since 2011-12-20 13:34
%%
-spec start_link(string()) -> any().

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%-----------------------------------------------------------------------------
%%
%% @doc stops receiver gen_server
%% @since 2011-12-20 13:34
%%
-spec stop() -> any().

stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc send a message to the server to reload own config
%% @since 2012-03-02 14:49
%%
-spec reload_config_signal() -> ok.

reload_config_signal() ->
    gen_server:cast(?MODULE, reload_config_signal).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does all necessary preparations
%%
-spec prepare_all(#ext{}) -> #ext{}.

prepare_all(C) ->
    prepare_log(C),
    write_pid(C),
    erlang:send_after(?PTIME, self(), periodic_check), % for redundancy
    C.
    
%%-----------------------------------------------------------------------------
%%
%% @doc performs periodic checks, triggers timer for next periodic check
%%
-spec periodic_check(#ext{}) -> #ext{}.

periodic_check(#ext{timer=Ref} = St) ->
    mpln_misc_run:cancel_timer(Ref),
    New = check_log_rotate(St),
    Nref = erlang:send_after(?PTIME, self(), periodic_check),
    New#ext{timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc rotate log if it's time to do it. Recreate log if it disappeared
%%
-spec check_log_rotate(#ext{}) -> #ext{}.

check_log_rotate(#ext{log=Base, log_last=Last, log_rotate=Dur} = St) ->
    mpln_misc_log:recreate_log(Base),
    case mpln_misc_log:need_rotate(Last, Dur) of
        true ->
            prepare_log(St),
            St#ext{log_last = calendar:local_time()};
        false ->
            St
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches config from updated environment and stores it in the state.
%%
-spec process_reload_config(#ext{}) -> #ext{}.

process_reload_config(St) ->
    C = erpher_ext_conf:get_config(St),
    C.

%%-----------------------------------------------------------------------------
%%
%% @doc read local config file and store it into env for later use
%%
prepare_updated_config() ->
    C = erpher_ext_conf:get_config(),
    File = C#ext.local_config,
    case file:consult(File) of
        {ok, [Data]} ->
            erpher_conf:add_config(Data);
        Other ->
            mpln_p_debug:pr({?MODULE, 'prepare_updated_config error', ?LINE,
                             File, Other}, C#ext.debug, run, 0)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc prepare log if it is defined
%%
-spec prepare_log(#ext{}) -> ok.

prepare_log(#ext{log=undefined}) ->
    ok;
prepare_log(#ext{log=Log}) ->
    mpln_misc_log:prepare_log(Log).

%%-----------------------------------------------------------------------------
%%
%% @doc writes pid file
%% @since 2011-11-11 14:17
%%
-spec write_pid(#ext{}) -> ok.

write_pid(#ext{pid_file=undefined}) ->
    ok;
write_pid(#ext{pid_file=File}) ->
    mpln_misc_run:write_pid(File).

%%-----------------------------------------------------------------------------
