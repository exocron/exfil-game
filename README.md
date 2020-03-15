Exfiltrate
==========

This game is a concept from WolverineSoft's Shammy Game Jam 2020 where two players (one an operative and the other a hacker) work together to infiltrate and extract items from enemy bases.

The operative plays in 2D top-down orthographical levels with the goal of finding items to extract and avoiding traps.

The hacker is presented with a DOS-style command prompt with the goal of discovering how to use the commands to disable traps and assist the operative.

How Do I Play?
--------------

You don't. At least not right now.

Everything that's been implemented so far is listed below:

* WebSocket server with support for multiple games
* Screens to start/join session, set nickname, choose role
* A terminal client for the hacker that currently does nothing
* Empty demo map (that's not currently accessible)

Here's what needs to be implemented before the game is runnable:

* DOS-style prompt and commands on the WebSocket server
* Trap objects
* Communicating trap object state over WebSocket
* A playable character for the operative (incl sprites)

And that's just the bare minimum.

License
-------

GPL 3 (see COPYING)
