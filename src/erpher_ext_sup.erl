%%% 
%%% erpher_ext_sup: supervisor for unsorted task server
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
%%% @since 2012-03-16 15:36
%%% @license MIT
%%% @doc main supervisor for runtime statistic server
%%% 

-module(erpher_ext_sup).
-behaviour(supervisor).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start_link/0, init/1]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(RESTARTS, 5).
-define(SECONDS, 2).

%%%----------------------------------------------------------------------------
%%% supervisor callbacks
%%%----------------------------------------------------------------------------
init(_Args) ->
    Stat = {
        erpher_ext, {erpher_ext, start_link, []},
        permanent, 1000, worker, [erpher_ext]
        },
    {ok, {{one_for_one, ?RESTARTS, ?SECONDS},
        [Stat]}}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start_link() -> any().
%%
%% @doc calls supervisor:start_link to create erpher_rt_stat_supervisor
%%
start_link() ->
    supervisor:start_link({local, erpher_rt_stat_supervisor},
                          erpher_rt_stat_sup, []).

%%-----------------------------------------------------------------------------
