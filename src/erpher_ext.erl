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
    erlang:send_after(?STAT_T, self(), periodic_check), % for redundancy
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, New#est.debug, run, 1),
    {ok, New, ?STAT_T}.

%------------------------------------------------------------------------------
-spec handle_call(any(), any(), #est{}) ->
                         {stop, normal, ok, #est{}}
                             | {reply, any(), #est{}}.
%%
%% Handling call messages
%% @since 2011-12-20 13:34
%%
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};

handle_call(status, _From, St) ->
    {reply, St, St};

handle_call({get, Start, Stop}, _From, St) ->
    mpln_p_debug:pr({?MODULE, get1, ?LINE, Start, Stop}, St#est.debug, run, 2),
    Res = get_items(St, Start, Stop),
    {reply, Res, St};

%% @doc set new debug level for facility
handle_call({set_debug_item, Facility, Level}, _From, St) ->
    % no api for this, use message passing
    New = mpln_misc_run:update_debug_level(St#est.debug, Facility, Level),
    {reply, St#est.debug, St#est{debug=New}};

handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _N}, St#est.debug, run, 2),
    {reply, {error, unknown_request}, St}.

%------------------------------------------------------------------------------
-spec handle_cast(any(), #est{}) -> any().
%%
%% Handling cast messages
%% @since 2011-12-20 13:34
%%
handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast({add, Id, Time_info, Data}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast add', ?LINE, Id, Time_info},
        St#est.debug, run, 4),
    New = add_item(St, Id, Time_info, Data),
    {noreply, New};

handle_cast({add_job, Time, Tag}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast add_job', ?LINE, Time, Tag},
        St#est.debug, run, 4),
    add_job_stat(Time, Tag),
    {noreply, St};

handle_cast({upd_job, Time, Tag, Work, Queued}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast upd_job', ?LINE, Time, Tag},
        St#est.debug, run, 4),
    upd_job_stat(Time, Tag, Work, Queued),
    {noreply, St};

handle_cast(reload_config_signal, St) ->
    New = process_reload_config(St),
    {noreply, New};

handle_cast(_Other, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Other},
        St#est.debug, run, 2),
    {noreply, St}.

%------------------------------------------------------------------------------
terminate(_, #ext{pid=File} = State) ->
    New = do_flush(State),
    stop_storage(New),
    mpln_misc_run:remove_pid(File),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE}, State#est.debug, run, 1),
    ok.

%------------------------------------------------------------------------------
-spec handle_info(any(), #est{}) -> any().
%%
%% Handling all non call/cast messages
%%
handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, 'info_timeout', ?LINE}, State#est.debug, run, 3),
    St_p = log_procs(State),
    New = periodic_check(St_p),
    {noreply, New};

handle_info(periodic_check, State) ->
    mpln_p_debug:pr({?MODULE, 'info_periodic_check', ?LINE},
                    State#est.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info(log_procs, State) ->
    mpln_p_debug:pr({?MODULE, 'log_procs', ?LINE}, State#est.debug, run, 6),
    New = log_procs(State),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'info_other', ?LINE, _Req},
                    State#est.debug, run, 2),
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
%% @doc sends input id, time, data to the server to store them in the storage
%% @since 2011-12-20 19:00
%%
-spec add(any(), any()) -> ok.

add(Id, Data) ->
    add(Id, undefined, Data).

-spec add(any(), any(), any()) -> ok.

add(Id1, Id2, Data) ->
    Now = now(),
    add(Id1, Id2, mpln_misc_time:get_gmt_time(Now), Now, Data).

-spec add(any(), any(), non_neg_integer(), tuple(), any()) -> ok.

add(Id1, Id2, Time, Now, Data) ->
    List = make_list(Data),
    gen_server:cast(?MODULE, {add, {Id1, Id2}, {Time, Now}, List}).

%%-----------------------------------------------------------------------------
%%
%% @doc receives start/stop times and returns json binary with stat items
%% for this interval. Times are unix times, namely seconds from 1970-01-01
%% @since 2011-12-20 19:03
%%
-spec get(string(), string()) -> binary().

get(Start, Stop) ->
    gen_server:call(?MODULE, {get, Start, Stop}).

%%-----------------------------------------------------------------------------
%%
%% @doc api call to add time statistic
%% @since 2012-02-02 14:09
%%
-spec add_stat_t(tuple(), any()) -> ok.

add_stat_t(Time, Tag) ->
    gen_server:cast(?MODULE, {add_job, Time, Tag}).

%%
%% @doc api call to update time statistic
%% @since 2012-02-02 14:09
%%
-spec upd_stat_t(any(), non_neg_integer(), non_neg_integer()) -> ok.

upd_stat_t(Tag, Work, Queued) ->
    upd_stat_t(now(), Tag, Work, Queued).

-spec upd_stat_t(tuple(), any(), non_neg_integer(), non_neg_integer()) -> ok.

upd_stat_t(Time, Tag, Work, Queued) ->
    gen_server:cast(?MODULE, {upd_job, Time, Tag, Work, Queued}).

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
-spec prepare_all(#est{}) -> #est{}.

prepare_all(#est{log_procs_interval=T} = C) ->
    prepare_log(C),
    write_pid(C),
    erlang:send_after(T, self(), log_procs),
    erlang:send_after(?STAT_T, self(), periodic_check), % for redundancy
    prepare_storage(C).
    
%%-----------------------------------------------------------------------------
%%
%% @doc prepares statistic
%%
prepare_stat(St) ->
    ets:new(?STAT_TAB_M, [named_table, set, protected, {keypos,1}]),
    ets:new(?STAT_TAB_H, [named_table, set, protected, {keypos,1}]),
    St.

%%-----------------------------------------------------------------------------
%%
%% @doc opens storage file
%%
prepare_storage(#est{storage_base=Base} = C) ->
    Name = mpln_misc_log:get_fname(Base),
    mpln_p_debug:pr({?MODULE, 'prepare_storage', ?LINE, Name},
                    C#est.debug, file, 2),
    filelib:ensure_dir(Name),
    case file:open(Name, [append, raw, binary]) of
        {ok, Fd} ->
            C#est{storage_fd=Fd, storage_start=now(), storage_cur_name=Name};
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'prepare_all open error', ?LINE, Reason},
                            C#est.debug, run, 0),
            C
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc flushes current data to file and closes current storage file
%%
stop_storage(#est{storage_fd=Fd} = St) ->
    do_flush(St),
    file:close(Fd).

%%-----------------------------------------------------------------------------
%%
%% @doc logs memory information, establishes a new timer for the next iteration
%%
log_procs(#est{timer_log=Ref, log_procs_interval=T} = St) ->
    mpln_misc_run:cancel_timer(Ref),
    real_log_procs(St),
    log_procs_info(St),
    Nref = erlang:send_after(T * 1000, self(), log_procs),
    St#est{timer_log=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc logs the following: pid, registered_name, memory, message_queue_len,
%% reductions. Ordered by memory, reductions, message_queue_len, pid
%% @todo make it lazy. Otherwise, for very often logging it will cause
%% troubles
%%
log_procs_info(St) ->
    L1 = [one_proc_info(X) || X <- processes()],
    L2 = lists:sort(fun compare_proc_info/2, L1),
    mpln_p_debug:pr({?MODULE, 'log_procs_info', ?LINE}, St#est.debug, stat, 5),
    mpln_p_debug:pr(L2, St#est.debug, stat, 5).

%%-----------------------------------------------------------------------------
%%
%% @doc compares memory, then reductions, then message queue length
%%
compare_proc_info(A, B) ->
    {_Pid1, _Name1, _Fun1, Mem1, Reds1, Len1} = A,
    {_Pid2, _Name2, _Fun2, Mem2, Reds2, Len2} = B,
    if  Mem1 > Mem2  -> true;
        Mem1 == Mem2 ->
            if  Reds1 > Reds2  -> true;
                Reds1 == Reds2 ->
                    if  Len1 > Len2  -> true;
                        true         -> false
                    end;
                true -> false
            end;
        true -> false
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc returns pid, registered_name, memory, message_queue_len, reductions,
%% current_function as a tuple for the given pid
%%
-spec one_proc_info(pid()) -> tuple().

one_proc_info(Pid) ->
    List =
        case process_info(Pid, [registered_name, memory, message_queue_len,
                reductions, current_function]) of
            L when is_list(L) ->
                L;
            _ ->
                []
        end,
    Fun = proplists:get_value(current_function, List),
    Mem = proplists:get_value(memory, List),
    Name = proplists:get_value(registered_name, List),
    Len = proplists:get_value(message_queue_len, List),
    Reds = proplists:get_value(reductions, List),
    {Pid, Name, Fun, Mem, Reds, Len}.

%%-----------------------------------------------------------------------------
%%
%% @doc sends sum of memory to be written to storage
%%
real_log_procs(St) ->
    {Sum, Nproc} = Res = estat_misc:get_procs_info(),
    erpher_ext_info:write_rt_info(St, Res),
    mpln_p_debug:pr({?MODULE, 'real_log_procs', ?LINE, Nproc, Sum},
                    St#est.debug, stat, 4),
    add('memory', 'num_proc', Nproc),
    add('memory', 'memory_sum', Sum).

%%-----------------------------------------------------------------------------
%%
%% @doc performs periodic checks, triggers timer for next periodic check
%%
-spec periodic_check(#est{}) -> #est{}.

periodic_check(#est{timer=Ref, flush_interval=T} = St) ->
    mpln_misc_run:cancel_timer(Ref),
    St_e = check_existing_file(St),
    St_f = do_flush(St_e),
    St_r = check_rotate(St_f),
    New = clean_old(St_r),
    Nref = erlang:send_after(T * 1000, self(), periodic_check),
    New#est{timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc stores id, time, data in the storage
%%
-spec add_item(#est{}, any(), {non_neg_integer(), tuple()}, any()) -> #est{}.

add_item(#est{storage=S} = St, Id, {Time, Now}, Data) ->
    Item = {Id, Time, Now, Data},
    New = St#est{storage = [Item | S]},
    check_flush(New).

%%-----------------------------------------------------------------------------
%%
%% @doc updates items in job statistic (minute and hourly)
%%
-spec upd_job_stat(tuple(), any(), non_neg_integer(), non_neg_integer()) ->
                          true.

upd_job_stat(Time, Tag, Work, Queued) ->
    upd_minute_job_stat(Time, Tag, Work, Queued),
    upd_hourly_job_stat(Time, Tag, Work, Queued).

upd_minute_job_stat(Time, Tag, Work, Queued) ->
    estat_misc:set_max_timed_stat(?STAT_TAB_M, 'minute', Time, {Tag, 'work'},
                                  Work),
    estat_misc:set_max_timed_stat(?STAT_TAB_M, 'minute', Time, {Tag, 'queued'},
                                  Queued).

upd_hourly_job_stat(Time, Tag, Work, Queued) ->
    estat_misc:set_max_timed_stat(?STAT_TAB_H, 'hour', Time, {Tag, 'work'},
                                  Work),
    estat_misc:set_max_timed_stat(?STAT_TAB_H, 'hour', Time, {Tag, 'queued'},
                                  Queued).


%%-----------------------------------------------------------------------------
%%
%% @doc adds item to job statistic (minute and hourly)
%%
-spec add_job_stat(tuple(), any()) -> true.

add_job_stat(Time, Tag) ->
    add_minute_job_stat(Time, Tag),
    add_hourly_job_stat(Time, Tag).

add_minute_job_stat(Time, Tag) ->
    estat_misc:add_timed_stat(?STAT_TAB_M, 'minute', Time, Tag).

add_hourly_job_stat(Time, Tag) ->
    estat_misc:add_timed_stat(?STAT_TAB_H, 'hour', Time, Tag).

%%-----------------------------------------------------------------------------
%%
%% @doc cleans old statistic and job info files
%%
-spec clean_old(#est{}) -> #est{}.

clean_old(St) ->
    clean_old_estat_files(St).

clean_old_statistic(#est{stat_limit_cnt_h=Hlimit, stat_limit_cnt_m=Mlimit}) ->
    estat_misc:clean_timed_stat(?STAT_TAB_H, Hlimit),
    estat_misc:clean_timed_stat(?STAT_TAB_M, Mlimit).

clean_old_estat_files(#est{keep_time=Keep} = St) ->
    T1 = mpln_misc_time:make_gregorian_seconds(0),
    T2a = mpln_misc_time:make_gregorian_seconds(),
    T2 = T2a - (3600 * (Keep+1)),
    List = get_files(St, T1, T2),
    [clean_one_file(St, X) || X <- List],
    St.

%%-----------------------------------------------------------------------------
%%
%% @doc cleans one old job info file
%%
clean_one_file(St, File) ->
    mpln_p_debug:pr({?MODULE, 'clean_one_file', ?LINE, File},
                    St#est.debug, file, 3),
    case file:delete(File) of
        ok ->
            ok;
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'clean_one_file error', ?LINE,
                             File, Reason}, St#est.debug, file, 0)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc checks if the current file is still here and reopens it if not
%%
-spec check_existing_file(#est{}) -> #est{}.

check_existing_file(#est{storage_cur_name=File} = St) ->
    case file:read_file_info(File) of
        {ok, _Info} ->
            St;
        {error, _} ->
            stop_storage(St),
            prepare_storage(St)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc checks whether the storage file need to be changed
%%
-spec check_rotate(#est{}) -> #est{}.

check_rotate(#est{storage_start=Start, rotate_interval=Rotate} = St) ->
    T = calendar:now_to_local_time(Start),
    case mpln_misc_log:need_rotate(T, Rotate) of
        true ->
            stop_storage(St),
            prepare_storage(St);
        false ->
            St
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc checks storage limits and flush it to disk
%%
-spec check_flush(#est{}) -> #est{}.

check_flush(#est{storage=S, flush_interval=T, flush_last=Last,
                 flush_number=N} = St) ->
    St_f = check_existing_file(St),
    Len = length(S),
    Delta = timer:now_diff(now(), Last),
    if (Delta > T * 1000000) or (Len >= N) ->
            do_flush(St_f);
       true ->
            St_f
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc does the flushing of accumulated info to storage
%%
-spec do_flush(#est{}) -> #est{}.

do_flush(#est{storage=S} = St) ->
    List = lists:reverse(S),
    L2 = lists:map(fun(X) -> create_binary_item(St, X) end, List),
    mpln_p_debug:pr({?MODULE, 'do_flush', ?LINE, L2},
                    St#est.debug, run, 6),
    proceed_flush(St, L2).

%%-----------------------------------------------------------------------------
%%
%% @doc proceeds with flushing
%%
proceed_flush(St, []) ->
    St#est{storage=[], flush_last=now()};

proceed_flush(#est{storage_fd=Fd} = St, List) ->
    check_separator(St),
    Json = mochijson2:encode(List),
    Bin = unicode:characters_to_binary(Json),
    case file:write(Fd, Bin) of
        ok ->
            ok;
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'proceed_flush error', ?LINE, Reason},
                            St#est.debug, run, 0)
    end,
    St#est{storage=[], flush_last=now()}.

%%-----------------------------------------------------------------------------
%%
%% @doc checks whether the json field separator need to be written
%%
check_separator(#est{storage_cur_name=Name} = St) ->
    case filelib:file_size(Name) of
        0 ->
            ok;
        _ ->
            add_separator(St)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc adds the json field separator to the storage file if this file has
%% some data
%%
add_separator(#est{storage_fd=Fd} = St) ->
    case file:write(Fd, <<",\n">>) of
        ok ->
            ok;
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'add_separator error', ?LINE, Reason},
                            St#est.debug, run, 0)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc tries to maintain a proper list containing {key, value} tuples. This
%% list is to be stored in a storage.
%%
make_list(undefined) ->
    undefined;

make_list({_, _} = Data) ->
    [Data];

make_list(Data) ->
    Data.

%%-----------------------------------------------------------------------------
%%
%% @doc creates json binary from the fetched item list
%%
-spec create_binary_item(#est{}, tuple()) -> list().

create_binary_item(_St, {{Ref, Stage}, _Time, Now, undefined}) ->
    [
     {<<"ref">>, mpln_misc_web:make_binary(Ref)},
     {<<"stage">>, Stage},
     {<<"time">>, list_to_binary(mpln_misc_time:get_time_str_us(Now))}
    ];

create_binary_item(_St, {{Ref, Stage}, _Time, Now, List}) ->
    [
     {<<"ref">>, mpln_misc_web:make_binary(Ref)},
     {<<"stage">>, Stage},
     {<<"time">>, list_to_binary(mpln_misc_time:get_time_str_us(Now))},
     {<<"params">>, List}
    ];

create_binary_item(St, _Item) ->
    mpln_p_debug:pr({?MODULE, 'create_binary_item unknown', ?LINE, _Item},
                    St#est.debug, run, 4),
    [].

%%-----------------------------------------------------------------------------
%%
%% @doc receives start/stop times and returns json binary with stat items
%% for this interval. Times are unix times, namely seconds from 1970-01-01,
%% local time zone.
%%
-spec get_items(#est{}, non_neg_integer(), non_neg_integer()) -> binary().

get_items(St, Start, Stop) ->
    T1 = mpln_misc_time:make_gregorian_seconds(Start),
    T2 = mpln_misc_time:make_gregorian_seconds(Stop + 3600 - 1),
    List = get_files(St, T1, T2),
    mpln_p_debug:pr({?MODULE, 'get_items', ?LINE, Start, Stop, T1, T2,
                    length(List)}, St#est.debug, file, 2),
    mpln_p_debug:pr({?MODULE, 'get_items', ?LINE, List}, St#est.debug, file, 3),
    create_binary_response(St, List).

%%-----------------------------------------------------------------------------
%%
%% @doc returns list of files matched to the interval
%%
get_files(#est{storage_base=Full_base} = St, T1, T2) ->
    Tstr1 = mpln_misc_time:make_short_str2(
              calendar:gregorian_seconds_to_datetime(T1), hour),
    Tstr2 = mpln_misc_time:make_short_str2(
              calendar:gregorian_seconds_to_datetime(T2), hour),
    mpln_p_debug:pr({?MODULE, 'get_files', ?LINE, Tstr1, Tstr2},
                    St#est.debug, file, 4),
    List = filelib:wildcard(Full_base ++ "*"),
    mpln_p_debug:pr({?MODULE, 'get_files', ?LINE, List},
                    St#est.debug, file, 5),
    Base = filename:basename(Full_base),
    Blen = length(Base) + 2,
    lists:filter(
      fun(X) ->
              mpln_p_debug:pr({?MODULE, 'get_files', ?LINE, Blen, X},
                              St#est.debug, file, 6),
              check_one_filename(St, Blen, Tstr1, Tstr2, X)
      end,
      List).

%%-----------------------------------------------------------------------------
%%
%% @doc checks if the filename is between T1 and T2 time stamps.
%% Comparison is made on strings.
%%
check_one_filename(St, Blen, T1, T2, File) ->
    Fbase = filename:basename(File),
    Fdate = string:substr(Fbase, Blen),
    mpln_p_debug:pr({?MODULE, 'check_one_filename', ?LINE,
                     T1, T2, (Fdate >= T1), (Fdate =< T2), Fbase, Fdate},
                    St#est.debug, file, 7),
    (Fdate >= T1) andalso (Fdate =< T2).

%%-----------------------------------------------------------------------------
%%
%% @doc creates json binary for the fetched file list
%%
-spec create_binary_response(#est{}, [string()]) -> binary().

create_binary_response(St, List) ->
    {Data, _Sum_size} = lists:mapfoldl(
             fun(X, Acc) ->
                     create_binary_data_item(St, X, Acc)
             end,
             0, List),
    unicode:characters_to_binary(Data).

%%-----------------------------------------------------------------------------
%%
%% @doc creates json binary for the file
%%
-spec create_binary_data_item(#est{}, string(), non_neg_integer()) ->
                                     {binary(), non_neg_integer()}
                                         | {iolist(), non_neg_integer()}.

create_binary_data_item(St, File, Sum_size) ->
    Size = filelib:file_size(File),
    mpln_p_debug:pr({?MODULE, 'create_binary_data_item size', ?LINE,
                     File, Size, Sum_size}, St#est.debug, run, 3),
    case file:read_file(File) of
        {ok, Bin} when byte_size(Bin) == 0 ->
            {<<>>, Sum_size};
        {ok, Bin} when Sum_size > 0 ->
            Sep = <<",\n">>,
            {[Sep, Bin], Sum_size + byte_size(Bin) + byte_size(Sep)};
        {ok, Bin} ->
            {Bin, byte_size(Bin)};
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'create_binary_data_item error', ?LINE,
                             File, Reason, Sum_size}, St#est.debug, run, 0),
            {<<>>, Sum_size}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches config from updated environment and stores it in the state.
%%
-spec process_reload_config(#est{}) -> #est{}.

process_reload_config(St) ->
    Stf = do_flush(St),
    stop_storage(Stf),

    C = erpher_ext_conf:get_config_stat(),
    prepare_storage(C).

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
-spec prepare_log(#ewm{}) -> ok.

prepare_log(#ewm{log=undefined}) ->
    ok;
prepare_log(#ewm{log=Log}) ->
    mpln_misc_log:prepare_log(Log).

%%-----------------------------------------------------------------------------
%%
%% @doc writes pid file
%% @since 2011-11-11 14:17
%%
-spec write_pid(#ewm{}) -> ok.

write_pid(#ewm{pid_file=undefined}) ->
    ok;
write_pid(#ewm{pid_file=File}) ->
    mpln_misc_run:write_pid(File).

%%-----------------------------------------------------------------------------
