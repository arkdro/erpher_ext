%%%
%%% erpher_ext_conf: functions for config
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
%%% @since 2012-03-16 15:46
%%% @license MIT
%%% @doc functions related to config file read, config processing
%%%

-module(erpher_ext_conf).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_config/0, get_config/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("erpher_ext.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc reads config file for receiver, fills in csr record with configured
%% values
%% @since 2011-10-14 15:50
%%
-spec get_config() -> #ext{}.

get_config() ->
    get_config(#ext{}).

%%
%% @doc receives input config and updates it with values from environment.
%% Returns updated config.
%% @since 2012-02-15 14:45
%%
-spec get_config(#ext{}) -> #ext{}.

get_config(Src) ->
    List = get_config_list(),
    fill_config(List, Src).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc gets data from the list of key-value tuples and stores it into
%% the input record
%% @since 2011-10-14 15:50
%%
-spec fill_config(list(), #ext{}) -> #ext{}.

fill_config(List, Src) ->
    Src#ext{
      pid_file = proplists:get_value(pid_file, List, ?PID),
      local_config = proplists:get_value(local_config, List),
      debug = proplists:get_value(debug, List, []),
      log = proplists:get_value(log, List, ?LOG)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches the configuration from environment
%% @since 2011-10-14 15:50
%%
-spec get_config_list() -> list().

get_config_list() ->
    application:get_all_env('expher_ext').

%%-----------------------------------------------------------------------------
