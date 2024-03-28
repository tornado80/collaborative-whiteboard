Backend
=====

To run the backend, you have two options. One is to have docker installed and build the image using the Dockerfile
and run a container. Service will be available at [http://localhost:8080](http://localhost:8080) 
so don't forget to expose the port. The other option is to install Erlang and Rebar3 and run the service locally. You
need to have Erlang/OTP 25 and Rebar3 installed. You can get Erlang/OTP binaries 
from the [Erlang Solutions website](https://www.erlang-solutions.com/resources/download.html) or 
use [kerl](https://github.com/kerl/kerl) to build it from source. 
(Useful information can be found in [Elixir Installation Help](https://elixir-lang.org/install.html) and 
[AdoptingErlang](https://adoptingerlang.org/docs/development/setup/#installing-erlang-otp).) 
You can install Rebar3 by following the instructions on 
the [Rebar3 website](https://www.rebar3.org/docs/getting-started).

Run with Docker
-----

    $ docker build -t whiteboard-backend .
    $ docker run -p 8080:8080 --rm whiteboard-backend

You may run the container in detached mode by adding the `-d` flag to the `docker run` command. 
If you want to see the logs, run in interactive mode with the `-it` flags.

Run with Rebar3
-----

    $ rebar3 shell

This command first fetches dependencies, compiles the project, starts the service, and opens the Erlang shell. 
You can explore with exported functions of services in the shell. Note that database files are stored in the
user data directory (usually `~/.local/share` but strictly speaking, the value of environment variable `XDG_DATA_HOME`) 
under `whiteboard-backend`. 
You can change the directory by setting the `app_data_directory` in
`sys.config` file.

You can create a release by running the following command:

    $ rebar3 as prod release

After the release is created under the `_build/prod` directory, you can start the service with the following command:

    $ _build/prod/rel/backend/bin/backend foreground

You can also start the service in the background by running:

    $ _build/prod/rel/backend/bin/backend deamon

Check out the [Rebar3 extended start script](https://rebar3.org/docs/deployment/releases/#extended-start-script) 
for more information.

Run tests
-----

You can run Eunit and Common Test tests with the following commands:

    $ rebar3 do eunit, ct

Use `rebar3 ct --verbose` to see the output of Common Test testcases.