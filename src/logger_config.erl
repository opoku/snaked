%%%-------------------------------------------------------------------
%%% File    : logger_config.erl
%%% Author  : Snaked Development Team
%%% Description : Logger configuration and initialization for Snaked game
%%%
%%% This module provides functions to initialize and configure the
%%% Erlang logger for the Snaked game application.
%%%-------------------------------------------------------------------
-module(logger_config).

-export([init/0, init/1, set_level/1, set_module_level/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize the logger with default configuration.
%% Creates the logs directory if it doesn't exist and sets up
%% console and file handlers.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    init(info).

%%--------------------------------------------------------------------
%% @doc
%% Initialize the logger with specified log level.
%% @param Level - Atom representing log level (debug, info, notice, warning, error)
%% @end
%%--------------------------------------------------------------------
-spec init(atom()) -> ok.
init(Level) ->
    %% Ensure logs directory exists
    ensure_log_directory(),
    
    %% Set primary log level
    logger:set_primary_config(level, Level),
    
    %% Configure console handler (for development)
    configure_console_handler(),
    
    %% Configure file handlers (for production)
    configure_file_handlers(),
    
    %% Log initialization
    logger:info("Snaked game logger initialized", [], #{level => Level}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Set the global log level.
%% @param Level - Atom representing log level
%% @end
%%--------------------------------------------------------------------
-spec set_level(atom()) -> ok | {error, term()}.
set_level(Level) when is_atom(Level) ->
    logger:set_primary_config(level, Level),
    logger:info("Global log level changed", [], #{new_level => Level}),
    ok;
set_level(_) ->
    {error, invalid_level}.

%%--------------------------------------------------------------------
%% @doc
%% Set log level for a specific module.
%% @param Module - Module name
%% @param Level - Atom representing log level
%% @end
%%--------------------------------------------------------------------
-spec set_module_level(atom(), atom()) -> ok | {error, term()}.
set_module_level(Module, Level) when is_atom(Module), is_atom(Level) ->
    logger:set_module_level(Module, Level),
    logger:info("Module log level changed", [], #{module => Module, level => Level}),
    ok;
set_module_level(_, _) ->
    {error, invalid_parameters}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensure the logs directory exists, create if necessary.
%% @end
%%--------------------------------------------------------------------
ensure_log_directory() ->
    LogDir = "logs",
    case filelib:is_dir(LogDir) of
        true ->
            ok;
        false ->
            case file:make_dir(LogDir) of
                ok -> ok;
                {error, eexist} -> ok;  %% Race condition, directory created by another process
                {error, Reason} ->
                    io:format("Warning: Could not create logs directory: ~p~n", [Reason]),
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Configure console handler for development environment.
%% @end
%%--------------------------------------------------------------------
configure_console_handler() ->
    %% Remove default handler first
    logger:remove_handler(default),
    
    %% Add custom console handler
    HandlerConfig = #{
        level => info,
        config => #{
            type => standard_io
        },
        formatter => {logger_formatter, #{
            single_line => false,
            template => [time, " [", level, "] ", pid, " ", msg, "\n"],
            time_designator => $\s,
            time_offset => "Z"
        }}
    },
    
    case logger:add_handler(console, logger_std_h, HandlerConfig) of
        ok -> ok;
        {error, {already_exist, _}} -> ok;
        {error, Reason} ->
            io:format("Warning: Could not add console handler: ~p~n", [Reason]),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Configure file handlers for production environment.
%% @end
%%--------------------------------------------------------------------
configure_file_handlers() ->
    %% Handler for all logs
    AllLogsConfig = #{
        level => debug,
        config => #{
            file => "logs/snaked_all.log",
            max_no_bytes => 10485760,  %% 10MB
            max_no_files => 5,
            compress_on_rotate => true
        },
        formatter => {logger_formatter, #{
            single_line => false,
            template => [time, " [", level, "] ", pid, " [", mfa, ":", line, "] ", msg, "\n"],
            time_designator => $\s,
            time_offset => "Z"
        }}
    },
    
    case logger:add_handler(file_all, logger_std_h, AllLogsConfig) of
        ok -> ok;
        {error, {already_exist, _}} -> ok;
        {error, Reason1} ->
            io:format("Warning: Could not add file_all handler: ~p~n", [Reason1]),
            ok
    end,
    
    %% Handler for error logs only
    ErrorLogsConfig = #{
        level => error,
        config => #{
            file => "logs/snaked_error.log",
            max_no_bytes => 5242880,  %% 5MB
            max_no_files => 3,
            compress_on_rotate => true
        },
        formatter => {logger_formatter, #{
            single_line => false,
            template => [time, " [", level, "] ", pid, " [", mfa, ":", line, "] ", msg, "\n"],
            time_designator => $\s,
            time_offset => "Z"
        }}
    },
    
    case logger:add_handler(file_error, logger_std_h, ErrorLogsConfig) of
        ok -> ok;
        {error, {already_exist, _}} -> ok;
        {error, Reason2} ->
            io:format("Warning: Could not add file_error handler: ~p~n", [Reason2]),
            ok
    end.
