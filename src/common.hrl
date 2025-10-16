%% Legacy logging support (backward compatibility)
-ifdef(debug).
-define(LOG(X,Args), util:format(?MODULE_DEBUG,X,Args,{?MODULE,?LINE})).
-else.
-define(LOG(X,Args), true).
-endif.

-define(MODULE_DEBUG,true).

%% Modern structured logging using Erlang's logger module
%% These macros provide different log levels with structured metadata

%% DEBUG level - detailed information for debugging (snake movement, collision detection, food generation)
-define(LOG_DEBUG(Msg), 
    logger:debug(Msg, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                             line => ?LINE, 
                             pid => self()})).

-define(LOG_DEBUG(Msg, Meta), 
    logger:debug(Msg, [], maps:merge(Meta, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                                               line => ?LINE, 
                                               pid => self()}))).

%% INFO level - general information (game start/end, player actions, score updates)
-define(LOG_INFO(Msg), 
    logger:info(Msg, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                            line => ?LINE, 
                            pid => self()})).

-define(LOG_INFO(Msg, Meta), 
    logger:info(Msg, [], maps:merge(Meta, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                                              line => ?LINE, 
                                              pid => self()}))).

%% WARNING level - potentially problematic situations (invalid moves, boundary violations)
-define(LOG_WARNING(Msg), 
    logger:warning(Msg, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                               line => ?LINE, 
                               pid => self()})).

-define(LOG_WARNING(Msg, Meta), 
    logger:warning(Msg, [], maps:merge(Meta, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                                                 line => ?LINE, 
                                                 pid => self()}))).

%% ERROR level - error events that might still allow the application to continue (game crashes, unexpected states)
-define(LOG_ERROR(Msg), 
    logger:error(Msg, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                             line => ?LINE, 
                             pid => self()})).

-define(LOG_ERROR(Msg, Meta), 
    logger:error(Msg, [], maps:merge(Meta, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                                               line => ?LINE, 
                                               pid => self()}))).

%% NOTICE level - normal but significant events
-define(LOG_NOTICE(Msg), 
    logger:notice(Msg, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                              line => ?LINE, 
                              pid => self()})).

-define(LOG_NOTICE(Msg, Meta), 
    logger:notice(Msg, [], maps:merge(Meta, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, 
                                                line => ?LINE, 
                                                pid => self()}))).

