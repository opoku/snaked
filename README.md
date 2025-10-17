# Snaked - Distributed Snake Game

A distributed multiplayer Snake game implementation written in Erlang, showcasing distributed systems concepts including concurrency, node synchronization, and fault tolerance.

## 📋 Table of Contents

- [Project Overview](#project-overview)
- [Features](#features)
- [Architecture](#architecture)
- [Requirements](#requirements)
- [Installation](#installation)
- [Configuration](#configuration)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Technical Details](#technical-details)
- [Known Issues](#known-issues)
- [Contributing](#contributing)
- [Authors](#authors)
- [License](#license)

## 🎮 Project Overview

This project implements a distributed Snake game for a distributed systems class (18-842). It leverages Erlang's powerful concurrency capabilities to create a multiplayer game where multiple players can join and play on different nodes in a distributed environment.

The game demonstrates several distributed systems concepts:
- Distributed state management
- Node discovery and coordination
- Leader election
- Event synchronization across nodes
- Fault tolerance and node failure handling

## ✨ Features

- **Multiplayer Support**: Up to 8 concurrent players
- **Distributed Architecture**: Players run on separate Erlang nodes
- **Dynamic Game Discovery**: Automatic discovery and joining of active games
- **Leader Election**: Automated leader election for game coordination
- **Real-time Synchronization**: Coordinated game state across all nodes
- **Graphical UI**: GUI interface using Erlang's GS (Graphics System)
- **Fault Tolerance**: Handles node disconnections and player removal
- **Color-coded Snakes**: Each player gets a unique color

## 🏗️ Architecture

### Core Components

1. **Game Server** (`game_server.erl`)
   - Centralized server for game registration and discovery
   - Maintains list of active games and players
   - Handles player addition and removal

2. **Game Manager** (`game_manager.erl`)
   - Manages local game instance
   - Handles node connections and game joining
   - Implements leader election logic
   - Coordinates with other nodes

3. **Game Logic** (`game_logic.erl`)
   - Core game mechanics (snake movement, collision detection)
   - Maintains game state (snakes, food, obstacles)
   - Processes player events and timer ticks

4. **Snake UI** (`snake_ui.erl`)
   - Graphical user interface
   - Renders snakes, food, and obstacles
   - Captures keyboard input (arrow keys)

5. **Message Passer** (`message_passer.erl`)
   - Network communication layer
   - Broadcasts game events to all players
   - Handles message ordering and synchronization

6. **Communication Modules**
   - `tcp_comm.erl`: TCP-based communication
   - `udp_comm.erl`: UDP-based communication

7. **Supporting Modules**
   - `clock.erl`: Game clock/timer
   - `food.erl`: Food generation and management
   - `keyboard.erl`: Keyboard input handling
   - `util.erl`: Utility functions

## 📦 Requirements

- **Erlang/OTP**: Version R13B or later (tested with R13B04)
  - The GS (Graphics System) library for GUI functionality
  - Standard Erlang libraries: `gen_tcp`, `queue`, `lists`, etc.
- **Operating System**: Linux, macOS, or Unix-like system (recommended)
- **Network**: Local network connectivity for distributed gameplay

## 🔧 Installation

### 1. Install Erlang

#### On Ubuntu/Debian:
```bash
sudo apt-get update
sudo apt-get install erlang erlang-gs
```

#### On macOS (using Homebrew):
```bash
brew install erlang
```

#### On other systems:
Download and install from [erlang.org](https://www.erlang.org/downloads)

### 2. Clone the Repository

```bash
git clone <repository-url>
cd snaked
```

### 3. Build the Project

```bash
make
```

This will compile all Erlang source files in the `src/` directory and place the compiled beam files in the `ebin/` directory.

To clean build artifacts:
```bash
make clean
```

## ⚙️ Configuration

### Server Configuration

Edit `resources/server-config.txt` to configure the game server:

```erlang
%% Server hostname/IP
{127,0,0,1}.

%% Server port
9999.
```

For distributed gameplay across multiple machines:
1. Update the IP address to the server's actual IP
2. Ensure the port is accessible through any firewalls

### Snake Configuration

The `config/snake.conf` file can be used for additional game configurations (currently empty but available for custom settings).

## 🚀 Usage

### Starting the Game Server

First, start the central game server (only needed once for a group of players):

```bash
cd src
erl -pa ../ebin
```

Then in the Erlang shell:
```erlang
game_server:start().
```

### Starting a Game Client

#### Method 1: Using the Run Script

```bash
./run.sh <node_name> <port>
```

Example:
```bash
./run.sh player1 5001
```

#### Method 2: Manual Start

```bash
erl -pa ebin/ -run game_manager start <node_name> <port>
```

Example:
```bash
erl -pa ebin/ -run game_manager start player1 5001
```

### Starting Multiple Players

Open separate terminal windows for each player:

**Player 1:**
```bash
./run.sh player1 5001
```

**Player 2:**
```bash
./run.sh player2 5002
```

**Player 3:**
```bash
./run.sh player3 5003
```

### Game Controls

- **↑ (Up Arrow)**: Move snake up
- **↓ (Down Arrow)**: Move snake down
- **← (Left Arrow)**: Move snake left
- **→ (Right Arrow)**: Move snake right

### Game Flow

1. **First Player**: Creates a new game and becomes the leader
2. **Subsequent Players**: Automatically discover and join existing games
3. **During Game**: All players' movements are synchronized across nodes
4. **Player Death**: Dead players are removed from the game state
5. **Leader Failure**: Automatic re-election of a new leader

### Stopping the Game

To stop a client:
```erlang
game_manager:stop().
```

To stop the server:
```erlang
game_server:stop().
```

To reset the server (clear all games):
```erlang
game_server:reset().
```

## 📁 Project Structure

```
snaked/
├── README.md                 # This file
├── Makefile                  # Top-level build file
├── run.sh                    # Convenience script to start game
├── bugs.txt                  # Known issues and bug tracking
├── config/
│   └── snake.conf           # Snake configuration (currently unused)
├── doc/
│   └── Final Design Report.pdf  # Project documentation
├── resources/
│   ├── border.txt           # Border/obstacle definitions
│   ├── food.png             # Food sprite (if used)
│   └── server-config.txt    # Server connection configuration
├── src/
│   ├── Makefile             # Source compilation rules
│   ├── Emakefile            # Erlang make configuration
│   ├── common.hrl           # Common header file
│   ├── game_def.hrl         # Game definitions
│   ├── game_state.hrl       # Game state record definitions
│   ├── game_manager.erl     # Game management and coordination
│   ├── game_server.erl      # Central game server
│   ├── game_logic.erl       # Core game logic
│   ├── snake_ui.erl         # Graphical user interface
│   ├── message_passer.erl   # Network message handling
│   ├── clock.erl            # Game timer
│   ├── food.erl             # Food generation
│   ├── keyboard.erl         # Keyboard input
│   ├── tcp_comm.erl         # TCP communication
│   ├── udp_comm.erl         # UDP communication
│   └── util.erl             # Utility functions
└── ebin/                    # Compiled beam files (generated)
```

## 🔬 Technical Details

### Distributed State Management

The game maintains a consistent state across all nodes using:
- **Event Broadcasting**: All player actions are broadcast to other nodes
- **Tick Synchronization**: Each node processes events at the same game tick
- **Acknowledgment System**: Ensures all nodes receive critical updates

### Leader Election

- The first player to create a game becomes the leader
- Leaders coordinate with the game server for player management
- Priority-based election when the leader disconnects
- Lower priority numbers = higher precedence

### Network Communication

- **TCP**: Used for reliable game server communication
- **UDP**: Can be used for real-time game events (implementation dependent)
- **Message Passer**: Abstracts network communication with broadcast/unicast APIs

### Game Loop

1. Clock generates periodic ticks
2. Players send movement events
3. Game logic collects events from all nodes for current tick
4. State is updated (snake positions, collision detection)
5. UI is refreshed
6. Broadcast updated state to all nodes

### Synchronization

- Each node maintains a queue of received moves
- Events are processed in tick order
- Missing events are detected and handled
- Dead players are removed from synchronization

## 🐛 Known Issues

(See `bugs.txt` for detailed tracking)

### Open Issues

1. **State Corruption**: In some cases, players may believe they are the only ones in the game due to state corruption
2. **Event Detection**: Partial implementation of ensuring events are received from all nodes during each time period

### Resolved Issues

- ✅ Different snake colors for each player
- ✅ Missing event detection
- ✅ Player removal on death
- ✅ Proper cleanup when players die

### Limitations

- Maximum 8 players per game
- Requires GS (Graphics System) which may not be available on all Erlang installations
- Network latency can affect gameplay smoothness
- Limited error recovery in some edge cases

## 🤝 Contributing

This project was created as a class project. If you'd like to improve it:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Make your changes
4. Test with multiple nodes
5. Commit your changes (`git commit -am 'Add improvement'`)
6. Push to the branch (`git push origin feature/improvement`)
7. Create a Pull Request

### Development Guidelines

- Follow Erlang/OTP conventions
- Add comments for complex logic
- Test distributed scenarios with multiple nodes
- Update documentation for new features
- Log important events using the `?LOG` macro

### Testing

Test with multiple concurrent players:

```bash
# Terminal 1 (Server)
cd src && erl -pa ../ebin
> game_server:start().

# Terminal 2 (Player 1)
./run.sh test1 5001

# Terminal 3 (Player 2)
./run.sh test2 5002

# Terminal 4 (Player 3)
./run.sh test3 5003
```

## 👥 Authors

**ROAM DS Group** - Spring 2010, CMU 18-842 (Distributed Systems)

- **Rama** (rbupath)
- **Aditi** (apandya)
- **Osei Poku** (opoku)
- **Manan Patel** (mdpatel)

## 📄 License

This project was created for educational purposes as part of a distributed systems course at Carnegie Mellon University.

---

## 📚 Additional Resources

- [Erlang Official Documentation](https://www.erlang.org/docs)
- [Learn You Some Erlang](http://learnyousomeerlang.com/)
- [Erlang Distributed Programming](https://www.erlang.org/doc/reference_manual/distributed.html)

## 🙏 Acknowledgments

- Carnegie Mellon University, 18-842 Distributed Systems course
- Erlang/OTP development team for the excellent concurrent programming platform

---

**Note**: This project was created in 2010 and demonstrates distributed systems concepts. Some APIs and practices may be outdated compared to modern Erlang/OTP standards.

For questions or issues, please refer to the course materials or contact the original authors.
