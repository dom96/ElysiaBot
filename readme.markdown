# ElysiaBot
ElysiaBot is a modular haskell IRC Bot. It uses the SimpleIRC library to connect to IRC.

## Configuration
ElysiaBot uses two configuration files, users.ini and elysia.ini. They both 
have to reside in ~/.ElysiaBot.

elysia.ini has to contain the connection info. Here is a sample elysia.ini file:

    nick = ElysiaBot-dev
    user = Elysia
    real = Elysia Chlorotica Bot

    pidfile = ElysiaBot.pid

    [server]
    address = irc.freenode.net
    port    = 6667
    chans   = #()

    [server1]
    address = irc.gimp.org
    port    = 6667
    chans   = #HSBotTest
    
You can have as many servers as you want.

The users.ini file stores user information.

    [dom96]
    pass  = p44ss
    admin = True

You can add more users if you wish.
