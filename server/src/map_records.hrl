-record(laser, {name, p1, p2, enabled, interval}).
-record(start, {point, direction}).
-record(point, {x, y}).
-record(map, {tiles, lasers, start, goal}).