# Todo

## Modules(Plugins) as processes.
ElysiaBot would spawn processes and communicate with them over stdin, these would then communicate back through stdout and stderr.

This kind of approach will allow many advantages: plugins may be written in any language, plugins may be reloaded, plugins can be monitored for memory usage etc.

The potential disadvantage this may have is memory usage.

A suitable communication protocol will have to be made, with which the plugins communicate with Elysia.

### Dependancies
Plugins could send messages to each other, therefore depend on each other.
Perhaps an ini file could be used to store that kind of info, and detect any dependancy issues.

### Compilation
Elysia needs to know how to (compile and) run the plugin, since every plugin
might be in a language other than haskell, for example an interpreted language.

A run.sh(.bat) file will be executed at startup, or when the plugin is reloaded. 

Such a file might contain something like this for Python.

    python plugin.py
  
or for haskell

    ghc --make Plugin.hs
    ./Plugin
    
    -- or
    
    runhaskell Plugin.hs

### Communication
Plugins will communicate through stdout and stderr, JSON is probably the best
format to encode the IrcMessage and other useful information.

Pid message, this is required. It tells Elysia what the processes PID is.

``{ "type": "pid", "pid": "27515" }``

#### Message from Elysia to the plugin:

Message received from the server
  
    { 
      "type": "recv",
      "IrcMessage": 
      {
        "nick":   "someone",
        "user":   "some",
        "host":   "user.com",
        "server": null,
        "code":   "PRIVMSG",
        "msg":    "|hello!",
        "chan":   "#()",
        "other":  [],
        "raw":    ":someone!~some@user.com PRIVMSG #() :|hello!",
      },
      "IrcServer":
      {
        "address":  "irc.freenode.net",
        "nickname": "ElysiaBot",
        "username": "Elysia",
        "chans"   : ["#chan"]
      }
    }
  
quit request

    {
      "type": "quit"
    }
  
#### Messages from the plugin to Elysia:

success

    {
      "type": "success"
    }

failure

    {
      "type": "failure"
    }

sending a message

    {
      "type":   "send",
      "server": "irc.freenode.net",
      "msg":    "PRIVMSG #chan :Message"
    }
      
binding commands

    {
      "type": "cmdadd",
      "command": "cmd"
    }


  
