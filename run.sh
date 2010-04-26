#!/bin/sh

erl -noshell -pa ebin/ -run game_manager start manan0 5000  &
erl -noshell -pa ebin/ -s game_manager start manan1 5001 &
erl -noshell -pa ebin/ -s game_manager start manan2 5002 &
#erl -noshell -pa ebin/ -s game_manager start [manan4,5004] &
#erl -noshell -pa ebin/ -s game_manager start [manan5,5005] &
#erl -noshell -pa ebin/ -s game_manager start [manan6,5006] &
#erl -noshell -pa ebin/ -s game_manager start [manan7,5007] &
#erl -noshell -pa ebin/ -s game_manager start [manan8,5008] &
#erl -noshell -pa ebin/ -s game_manager start [manan9,5009] &
#erl -noshell -pa ebin/ -s game_manager start [manan10,5010] &
#erl -noshell -pa ebin/ -s game_manager start [manan11,5011] &



