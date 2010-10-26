# Plugin specification

## Messages from *Elysia* to the *plugin*:

### Requests

Message received from one of the IRC Servers that Elysia is connected to.

    { "method": "recv",
      "params": [ {
                     "mNick":   {"Just": "someone"},
                     "mUser":   {"Just": "some"},
                     "mHost":   {"Just": "user.com"},
                     "mServer": {"Just": "Nothing"},
                     "mCode":   "PRIVMSG",
                     "mMsg":    "|hello!",
                     "mChan":   {"Just": "#()"},
                     "mOther":  {"Just": []},
                     "mRaw":    ":someone!~some@user.com PRIVMSG #() :|hello!"
                   },
                   {
                     "address":  "irc.freenode.net",
                     "nickname": "ElysiaBot",
                     "username": "Elysia",
                     "chans"   : ["#chan"]
                   }
                ],
      "id": "Nothing"
    }

A message which begins with the prefix has been received, e.g. "|hello" (If the prefix is "|")

This is basically the same as the above, except for another two params, the prefix and command. e.g. prefix = "|" and command = "hello" in the above example, and also of course the method name is "cmd".

**Note:** The `{"Just": "Whatever"}` are important, if just a string is specified, then hard to interpret errors will happen.
They need to be placed wherever a Maybe Type is expected by Elysia. Error such as:

    MsgRecv {ircMessage = *** Exception: fromJSON: unknown constructor: someone DataType {tycon = "Prelude.Maybe", datarep = AlgRep [Nothing,Just]}

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
      "id": "Nothing"
    }

