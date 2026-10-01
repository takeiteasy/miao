# miao

**M**odel **I**ntegration **A**nd **O**rchestration

An agent core in Common Lisp, built on
[meow](https://github.com/takeiteasy/meow), for others to build agent
harnesses on. Tools, model adapters and the agent loop are meow services
under one root context, so they mount in any order, restart under
supervision, and are discovered through the registry.

`cli/` and `launcher/` ship as example front ends, useful for testing this
core before a full harness exists.

Runs on SBCL.

## Docs

- [Getting started](docs/getting-started.md)
- [Tools](docs/tools.md)
- [Introspection](docs/introspection.md)
- [The plan gate](docs/plan.md)
- [The allowlist gate](docs/gate.md)
- [Protocols](docs/protocols.md)
- [Providers](docs/providers.md)
- [The agent loop](docs/agent.md)
- [The front-end contract](docs/ui.md)
- [Client state](docs/client-state.md)
- [Parameter schemas](docs/schema.md)
- [Checkpoints and rollback](docs/checkpoints.md)
- [Forking a conversation](docs/forking.md)
- [Image generations](docs/images.md)
- [The launcher](docs/launcher.md)
- [The command line](docs/cli.md)
- [Chat](docs/chat.md)
- [Self-modification](docs/self.md)
- [The vault](docs/vault.md)
- [The call log](docs/calls.md)
- [Interceptor hooks](docs/hooks.md)
- [Redelivered inputs](docs/inputs.md)

## License

```
miao
Copyright (C) 2026 George Watson

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
```
