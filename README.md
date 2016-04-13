# JIRA Tempo CLI

"Tempo CLI" is a developers' answer to repetitive and boring task of filling in time tracking information in Tempo
JIRA plug-in. For us who already implicitly "track" their activity in Git (bad luck for SVN users), this tool will
attempt to report time based on commit history.


## Status

Work in Progress


## Features

 - log time manually from command line
 - log time based on Git history


## Install

To install locally, install [Haskell Stack](http://docs.haskellstack.org/en/stable/README.html) first, and then use:

    $ stack install

New binaries are installed into `~/.local/bin/`, so make sure to add it to `$PATH`.

Next, see `Configuration` and `Usage` below.


## Build locally

To build, install [Haskell Stack](http://docs.haskellstack.org/en/stable/README.html) and use:

    $ stack build


## Configuration

In order to use this tool, you will need to create a configuration file. This can be done
with `tempo-config` command. It will giude you through the process and validate your 
configiration at once:

    $ tempo-config

Alternatively, you could create configuration file manually in `~/.tempo.conf` with following
content:

    [git]
    repos = /comma/separated,/list/of,/your/repositories

    [jira]
    projects = CODES,OF,JIRA,PROJECTS,WITH,ISSUES,TO,LOG,TO
    host = mycompany.atlassian.net
    user = username
    pass = password in base64

Note that `user` and `password` should be those of your "native" JIRA user: that is, if you
use SSO with Google for example, this is **not** your Google user, but JIRA user. If you
never created a JIRA user/password, or don't know its attributes - reset a password
from JIRA login form indicating your e-mail you use for SSO.

Finally, `pass` should contain your password encoded in base 64. This is to at least avoid
having it in plain text in this file. Yes, it is not encrypted otherwise, so keep this file
secret.


## Usage

To log from Git history, use `tempo-git`. It accepts any **filtering** arguments valid for
`git log` command, which you can use to limit the Git history selection to search in. In
current version you may not use `--author` however (it is taken directly from your Git global
config). For example:

    $ tempo-git --since=3.days

`tempo-git` will not log to the days which are already "filled in", but will only log to 
complete the missing information. It means, that in a typical case, you don't even need to
use any filtering criteria and it is safe to run simply:

    $ tempo-git

The command is interactive in any case, so you will be asked to confir the work log before
it is submitted to JIRA.

To log a work log entry defined manually, you can use `tempo-simple`:

    # tempo-simple [yyyy/mm/dd] [ISSUE] [HOURS]
    $ tempo-simple 2016/04/01 FUN-011 8


## Run from source

You can use `stack` to execute same commands from within the project directory:

    $ stack exec tempo-git -- --since=1.week
