# Snaked Game Logging Documentation

## Overview

The Snaked Erlang Snake game now includes comprehensive structured logging using Erlang/OTP's built-in `logger` module. This logging system provides visibility into game events, player actions, and system behavior while maintaining minimal performance impact.

## Log Levels

The logging system uses the following log levels:

### DEBUG
Detailed information for debugging purposes. Includes:
- Snake movement events
- Collision detection details
- Food generation attempts
- Clock tick events (when leader)
- Player position updates

**Example:**
```erlang
?LOG_DEBUG("Snake moved", #{snake_id => SnakeId, direction => 'Up', new_head => {10, 15}})
```

### INFO
General informational messages about game flow. Includes:
- Game start/end events
- Player joining/leaving
- Score updates (when snake eats food)
- Leader election events
- Game state changes

**Example:**
```erlang
?LOG_INFO("Player added to game", #{game_id => 123, player_id => player1, new_player_count => 3})
```

### WARNING
Potentially problematic situations that don't stop the game. Includes:
- Invalid moves (opposite direction)
- Missing events from players
- Game full (cannot add player)
- Timeout warnings

**Example:**
```erlang
?LOG_WARNING("Invalid move: opposite direction", #{snake_id => player1, current => 'Up', attempted => 'Down'})
```

### ERROR
Error events that indicate problems but allow the game to continue. Includes:
- Process crashes
- Failed player additions
- Network communication errors
- Move from unregistered snake

**Example:**
```erlang
?LOG_ERROR("Process died", #{pid => Pid, reason => Reason})
```

## Configuration

### Log Handlers

The system is configured with three log handlers:

1. **Console Handler** (`console`)
   - Level: INFO
   - Output: Standard output (terminal)
   - Use: Development and real-time monitoring
   - Format: `timestamp [LEVEL] pid message`

2. **All Logs File Handler** (`file_all`)
   - Level: DEBUG
   - Output: `logs/snaked_all.log`
   - Rotation: 10MB per file, 5 files max, compressed
   - Use: Complete game session history
   - Format: `timestamp [LEVEL] pid [module:function:line] message`

3. **Error Logs File Handler** (`file_error`)
   - Level: ERROR
   - Output: `logs/snaked_error.log`
   - Rotation: 5MB per file, 3 files max, compressed
   - Use: Error tracking and debugging critical issues
   - Format: `timestamp [LEVEL] pid [module:function:line] message`

### Configuration File

Logger configuration is stored in `config/snake.conf`. To modify:

```erlang
{kernel, [
    {logger_level, info},  % Change global level here
    {logger, [
        {handler, console, logger_std_h, #{
            level => info,  % Change console level here
            % ... other config
        }},
        % ... other handlers
    ]}
]}
```

### Runtime Configuration

Change log level at runtime:

```erlang
%% Change global log level
logger_config:set_level(debug).

%% Change log level for specific module
logger_config:set_module_level(game_logic, debug).
```

## Usage in Code

### Using Logging Macros

The logging macros are defined in `common.hrl` and automatically include metadata:

```erlang
%% Simple message
?LOG_INFO("Game started")

%% Message with structured metadata
?LOG_INFO("Player joined", #{player_id => player1, game_id => 123})

%% Debug level with detailed data
?LOG_DEBUG("Snake position", #{snake_id => player1, 
                                position => [{10,10}, {10,11}],
                                direction => 'Up'})
```

### Available Macros

- `?LOG_DEBUG(Msg)` / `?LOG_DEBUG(Msg, Meta)`
- `?LOG_INFO(Msg)` / `?LOG_INFO(Msg, Meta)`
- `?LOG_NOTICE(Msg)` / `?LOG_NOTICE(Msg, Meta)`
- `?LOG_WARNING(Msg)` / `?LOG_WARNING(Msg, Meta)`
- `?LOG_ERROR(Msg)` / `?LOG_ERROR(Msg, Meta)`

All macros automatically include:
- Module name
- Function name and arity
- Line number
- Process ID

### Legacy Logging

The old `?LOG(Format, Args)` macro is still supported for backward compatibility but should be migrated to the new structured logging system.

## Log Files

### Location

All log files are stored in the `logs/` directory at the project root. This directory is automatically created on first startup if it doesn't exist.

### Log Rotation

Log files automatically rotate when they reach their size limit:
- Rotated files are compressed (`.gz`)
- Old files are automatically deleted when the max file count is reached
- Rotation happens in-place without service interruption

### Example Log Output

**Console:**
```
2025-10-16 14:23:45 [INFO] <0.123.0> Game started
2025-10-16 14:23:46 [DEBUG] <0.124.0> Snake moved
2025-10-16 14:23:50 [WARNING] <0.124.0> Invalid move: opposite direction
```

**File:**
```
2025-10-16 14:23:45 [INFO] <0.123.0> [game_logic:start_game:138] Game started
2025-10-16 14:23:46 [DEBUG] <0.124.0> [game_logic:move_snake:689] Snake moved
2025-10-16 14:23:50 [WARNING] <0.124.0> [game_logic:move_snake:677] Invalid move: opposite direction
```

## Performance Considerations

### Minimal Impact Design

The logging system is designed for minimal performance impact:

1. **Lazy Evaluation**: Log messages are only formatted if the log level is enabled
2. **Asynchronous Writing**: File writes happen asynchronously in separate processes
3. **Efficient Filtering**: Log level filtering happens before message formatting
4. **Structured Logging**: Using maps instead of string formatting reduces overhead

### Best Practices

1. **Use Appropriate Levels**: 
   - Don't log DEBUG in game loop hot paths unless needed
   - Use INFO for significant events only
   - Reserve ERROR for actual error conditions

2. **Structured Data**:
   ```erlang
   %% Good - structured metadata
   ?LOG_INFO("Player scored", #{player_id => Id, score => Score})
   
   %% Bad - string formatting
   ?LOG_INFO("Player " ++ atom_to_list(Id) ++ " scored " ++ integer_to_list(Score))
   ```

3. **Debug Level for Hot Paths**:
   ```erlang
   %% Game loop events - DEBUG level to minimize production overhead
   ?LOG_DEBUG("Tick processed", #{tick => Tick, players => PlayerCount})
   ```

4. **Avoid Logging Sensitive Data**:
   - Never log passwords or authentication tokens
   - Be cautious with player IP addresses in production
   - Use player IDs instead of personal information

## Module-Specific Logging

### game_logic.erl
- **INFO**: Game start/stop, player events, score updates
- **DEBUG**: Snake movement, collision detection
- **WARNING**: Invalid moves, opposite direction attempts
- **ERROR**: Unregistered snake events

### game_manager.erl
- **INFO**: Player join/leave, leader election, game creation
- **WARNING**: Game full conditions
- **ERROR**: Process crashes, failed operations

### food.erl
- **DEBUG**: Food generation, position selection

### clock.erl
- **DEBUG**: Tick events, pause/resume operations
- **INFO**: Clock initialization and shutdown

### game_server.erl
- **INFO**: Server start/stop, game creation, player management
- **WARNING**: Invalid messages, non-existent game access
- **ERROR**: Server startup failures

## Troubleshooting

### No Logs Appearing

1. Check logger initialization:
   ```erlang
   logger_config:init().
   ```

2. Verify log level:
   ```erlang
   logger:get_primary_config().
   ```

3. Check handler configuration:
   ```erlang
   logger:get_handler_config(console).
   ```

### Log Files Not Created

1. Ensure `logs/` directory exists and is writable
2. Check file handler configuration in `config/snake.conf`
3. Verify file paths are correct and accessible

### Performance Issues

1. Reduce log level in production:
   ```erlang
   logger_config:set_level(info).  % Disable DEBUG
   ```

2. Disable console logging in production:
   ```erlang
   logger:remove_handler(console).
   ```

3. Increase rotation size to reduce file operations:
   - Edit `max_no_bytes` in `config/snake.conf`

## Examples

### Monitoring a Game Session

```erlang
%% Start with debug level
logger_config:init(debug).

%% Start game server
game_server:start().

%% Monitor all.log for game events
tail -f logs/snaked_all.log

%% Check for errors
tail -f logs/snaked_error.log
```

### Production Setup

```erlang
%% Initialize with info level (no debug overhead)
logger_config:init(info).

%% Remove console handler to reduce output
logger:remove_handler(console).

%% Monitor via log files only
```

### Debugging Specific Module

```erlang
%% Enable debug for specific module
logger_config:set_module_level(game_logic, debug).

%% Keep other modules at info level
logger_config:set_level(info).
```

## Additional Resources

- Erlang Logger Documentation: https://www.erlang.org/doc/man/logger.html
- OTP Logger User's Guide: https://www.erlang.org/doc/apps/kernel/logger_chapter.html
- Logger Configuration: https://www.erlang.org/doc/apps/kernel/logger_chapter.html#configuration

## Support

For issues or questions about the logging system:
1. Check the logs in `logs/` directory
2. Review this documentation
3. Check module-specific log output at DEBUG level
4. Examine `logger_config.erl` for configuration options
