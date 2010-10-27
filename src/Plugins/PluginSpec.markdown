# Plugin behaviour 

ElysiaBot spawns processes and communicates with them over stdin, the plugins then communicate back through stdout/stderr.

This kind of approach has many advantages: plugins may be written in any language, plugins may be reloaded, plugins can be monitored for memory usage etc.

The potential disadvantage this may have is memory usage.

The communication protocol is based on JSON-RPC 1.0 and it is described below.

### Dependancies
**WARNING:** This is not yet implemented.

Plugins will be able to send messages to each other, therefore depend on each other.
An ini file will be used to store that kind of info, and detect any dependancy issues.

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
# Plugin specification

## Messages from *Elysia* to the *plugin*:

### Requests

Message received from one of the IRC Servers that Elysia is connected to.

    { "method": "recv",
      "params": [ {
                     "mNick":   "someone",
                     "mUser":   "some",
                     "mHost":   "user.com",
                     "mServer": null,
                     "mCode":   "PRIVMSG",
                     "mMsg":    "|hello!",
                     "mChan":   "#()",
                     "mOther":  [],
                     "mRaw":    ":someone!~some@user.com PRIVMSG #() :|hello!"
                   },
                   {
                     "address":  "irc.freenode.net",
                     "nickname": "ElysiaBot",
                     "username": "Elysia",
                     "chans"   : ["#chan"]
                   }
                ],
      "id": null
    }

A message which begins with the prefix has been received, e.g. "|hello" (If the prefix is "|")

This is basically the same as the above, except for another two params, the prefix and command. e.g. prefix = "|" and command = "hello" in the above example, and also of course the method name is "cmd".

### Responses

Success:

    { "result": "Operation finished successfully.",
      "error": null,
      "id": 0
    }

Error:

    { "result": "error",
      "error": "Detailed description of error.",
      "id": 0
    }

## Messages from the *plugin* to *Elysia*:

Sending a message to a server:

    { "method": "send",
      "params": [ "irc.freenode.net", "PRIVMSG #chan :Message" ],
      "id": 0
    }

This will return a confirmation whether the server was found and the message was sent. (It does not check whether the server accepted the message)

Binding commands:

    { "method": "cmdadd",
      "params": [ "cmd" ],
      "id": 0
    }

This will return a confirmation whether the command was successfully added.

PID -- This should be sent to Elysia as soon as the plugin starts:

    { "method": "pid",
      "params": [ "1234" ]
      "id": null
    }

