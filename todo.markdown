# Todo

## Modules(Plugins) as processes.
ElysiaBot would spawn processes and communicate with them over stdin, these would then communicate back through stdout and stderr.

This kind of approach will allow many advantages: plugins may be written in any language, plugins may be reloaded etc.

The potential disadvantage this may have is memory usage.

A suitable communication protocol will have to be made, with which the plugins communicate with Elysia.
