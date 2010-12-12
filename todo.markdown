# TODO

* Modules(Plugins) as processes
    * <del>Loading, unloading, and plugin list.</del>
    * Compile & run files for each language -- `compile.haskell` & `run.haskell` in the *plugins* directory.
        * Plugins should only be compiled if a) the `run` file failed or binary doesn't exist, or b) ElysiaBot was launched with --force-comp.
        * If a `run` file doesn't exist, ElysiaBot should just try running the executable by the name of the plugin(taken from the .ini file)
    * Clean up the PluginSpec
    * MonoDB database
    * Add plugin functions for:
        * reading the configuration file
        * checking whether a specific user is logged and has admin rights.
    * Dependencies
    * Fix any TODO's
    * _Refactor Plugins.hs and PluginUtils.hs_
    * _Write PluginUtils for other languages, Python, Ruby, Factor perhaps._
* Run hlint through the code and clean up.

