#ifndef CONFIG_H
#define CONFIG_H

#define IP(a,b,c,d) (((a) << 24) | ((b) << 16) | ((c) << 8) | (d))

#define GAME_SERVER IP(127,0,0,1)
#define GAME_SERVER_PORT 8080

#endif
