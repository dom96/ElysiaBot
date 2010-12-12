# ElysiaBot
ElysiaBot is a highly extensible haskell IRC Bot. It uses the SimpleIRC library to connect to IRC.

A very robust and easy to use plugin framework is being developed which will allow the writing of plugins in almost any programming language.

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

## Contributing
I welcome _any_ contribution openly, and once I review it and find it useful, I will be very happy to include it.
So feel free to fork ElysiaBot and help in whatever way you can. Also if you 
would like to discuss what you could work on, you can find me on freenode on `#()` (among other channels) as _dom96_.

Take a look at todo.markdown for things to do ;)
