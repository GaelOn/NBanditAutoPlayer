# NBanditAutoPlayer

An F# exploration of sequential decision-making under uncertainty, implementing automated strategies for the **multi-armed bandit problem**.

Developed by Gael Dupire in 2016, this project connects mathematical modelling, statistical computation and software design. It is preserved as an experimental project illustrating how decision algorithms can be expressed through explicit types, interchangeable strategies and focused tests.

## The mathematical problem

Imagine several slot machines, each with an unknown reward distribution. At each round, a player selects one machine and observes its reward. The objective is to maximize the total reward over successive rounds.

The central challenge is balancing:

- **Exploration:** trying alternatives to learn about their rewards.
- **Exploitation:** selecting the option that currently appears most rewarding.

For background, see the English Wikipedia article on the [multi-armed bandit problem](https://en.wikipedia.org/wiki/Multi-armed_bandit).

## Strategies explored

The project includes greedy and Upper Confidence Bound (UCB) strategy implementations and associated tests.

| Strategy family | Approach |
| --- | --- |
| Greedy | Use observed rewards to guide selection, with configurable exploration behaviour. |
| UCB | Combine observed rewards with an uncertainty allowance when choosing an arm. |

The strategy model includes basic and decreasing-exploration greedy variants, alongside UCB1 and UCB-Tuned options. These are experimental implementations; this repository does not establish theoretical guarantees or comparative performance results.

## Software design

The solution separates the simulated environment, player strategies and statistical support. F# records and discriminated unions express strategy choices and their parameters explicitly.

| Project | Responsibility |
| --- | --- |
| `Bandit` / `BanditContract` | Bandit implementation and associated contracts. |
| `NBanditAutoPlayerStrategy` | Automated player strategies. |
| `AutoPlayerStrategyContract` | Strategy types, parameters and player contracts. |
| `Math.Statistical` | Statistical calculations and random-generation support. |
| `WorkFlow` | Workflow support. |
| `AutoPlayNBanditArmed` | Application scaffold and bandit/player binding. |
| `BanditTest` | NUnit tests for bandits, strategies and statistical components. |

## Exploring the code

Start with:

1. [`PlayerType.fs`](AutoPlayerStrategyContract/PlayerType.fs) for the strategy model.
2. [`NBanditAutoPlayerStrategy`](NBanditAutoPlayerStrategy/) for the decision algorithms.
3. [`BanditTest`](BanditTest/) for examples exercising greedy strategies, UCB strategies and statistical behaviour.

The console entry point is a scaffold rather than a complete simulation interface. The tests and strategy implementations are the main entry points for understanding the project.

## Toolchain and project status

This repository uses legacy F# project files targeting .NET Framework 4.5, with Math.NET Numerics and NUnit dependencies. It has not been migrated to modern SDK-style .NET projects.

To investigate the original solution, open `NBanditArmed.sln` in an environment compatible with its F# build targets and framework references. Current build and test compatibility has not been verified; modern .NET CLI commands are not provided as a validated setup procedure.

The repository retains historical dependency packages and generated build outputs. It is an educational and algorithmic prototype, rather than a maintained production library.

## Author

[Gael Dupire](https://github.com/GaelOn)
