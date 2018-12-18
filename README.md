# Hydra

Hydra (Hybrid Deliberate Reasoning and Acting) is a tool for planning in hydrid domains.
It support planning for temporal PDDL and a subset of the ANML languages to specify the discrete part of the planning problem. And uses a SMT based planner for solving.

The continuous domain definition models the limits of the controller of the system and can be used to model, e.g., vehicles with differential driving.

## Dependencies

### Java

Hydra is written in Scala and Java and a Java Runtime Environment is required to run it.
It requires at least Java 8 and should run without problem on any later version.

### Z3

Hydra uses Z3 as a backend SMT solver. In order to run hydra the shared libraries `libz3.so` and `libz3java.so` should be in your library path. Those are runtime dependencies.
Latest binaries for Z3 can be found on [the project's GitHub](https://github.com/Z3Prover/z3/releases).
Once installed, add the target directory to the `LD_LIBRARY_PATH` environment variable for java to pick them up.

### Mill

We use mill as a build tool for the scala and java sources of the project. You either [install mill](http://www.lihaoyi.com/mill/) or use the wrapper script `./mill` provided in the repository that will download it to `out/mill-bin`. In the following the `mill` command will refer to the mill executable and should be replaced with `./mill` if you use the wrapper script.


## Build


Compilation is done 
```
mill hydra.compile    # compile 
mill hydra.assembly   # Build a standalone jar with all dependencies included
                      # and write it to  out/hydra/assembly/dest/out.jar
```

You can run hydra directly from mill, which will ensure that the the build is up to date.
```
mill hydra.run [options]
```

If you have already built an assembly with `mill hydra.assembly`, you can directly run the resulting jar:
```
java -jar out/hydra/assembly/dest/out.jar  [options]
```