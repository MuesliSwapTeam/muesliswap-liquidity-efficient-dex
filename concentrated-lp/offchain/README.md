# MuesliSwap Match Maker 🥣

The matchmaking algorithm powering the MuesliSwap DEX for Cardano.
It searches the contract for orders that can be fulfilled
and tries to bring them together.

### Running the script

The matchmaker is written in python and supposed to be run on python3.9 or newer.
In order to run the script, install all libraries denoted in `requirements.txt`.
Also, correctly set the values in `secret_template.py` and copy it
to `secret.py`.

The script comprises two main parts, querier and batchers (see `matchmaking/querier` and `matchmaking/batcher`).
The querier continuously scans the contract for open orders and up to date pools and writes them into an external file.
The matchmaker listens to file changes of that file and scans it for orders that match.
Upon finding a matching pair, it will try to build a match transaction and submit it to the network.

In order to run the queriers, run
```bash
$ bash scripts/run_querier.sh &
$ bash scripts/run_pool_querier.sh &
```

Creating an enterprise wallet for the matchmaker is facilitated by running
Send funds to the printed wallet address and start the matchmaker configured to use
this address and the corresponding signing key to start operating.
```bash
$ bash scripts/new_wallet.sh <wallet_name>
```

In order to run the matchmaker, run
```bash
$ bash scripts/run_matchmaking.sh keys/<wallet_name>.skey
```

Finally find out about usage of the matchmaker via

```bash
$ bash scripts/run_matchmaking.sh --help
```

### Further scripts

There are a few more scripts that need to be run for a fully operational DEX.
This includes batchers (to process liquidity provision and withdrawal) and swapmakers (to process trades against liquidity pools).
Both of these come in variations for concentrated liquidity pools as well.
Finally, there is a multi-hop version of the script that can be used to trade between tokens that are not directly tradable (using several pools).
Concretely, for an operational DEX all of the following queriers and batchers should run:

- `run_querier.sh` (for orders)
- `run_pool_querier.sh` (for pools)
- `run_batching.sh` (for constant product liquidity provision)
- `run_batching_clp.sh` (for concentrated liquidity provision)
- `run_matchmaking.sh` (for orderbook trades)
- `run_swapmaking.sh` (for constant product swaps)
- `run_swapmaking_clp.sh` (for constant product swaps)
- `run_swapmaking_multihop.sh` (for swaps using several pools)
- `run_dustcleaning.sh` (for swaps of tokens with very low liquidity)

There also is a sweeper script to remove all temporary files created by the matchmaker.
Moreover pool creation pools are available, the normal one is intended to be run as a continuous processing job, processing user requests for pool creation.

- `run_sweeper.sh` (for cleaning up temporary files)
- `run_create_pool.sh` (for creating pools)

### Speed improvements

... are always welcome. At least for the querier however, benchmarking has shown that `pypy` is not the solution.

> In congested times its always good to have someone that covers your back

At least we found that it help to submit a transaction to several nodes. Why? :shrug:

> Mount a RAM directory in `./data`

The service writes *a lot* to `./data`. It is a good idea to mount ./data to RAM, and drastically reduce IO

```
mount -t tmpfs -o size=512m tmpfs ./data
```

> Setting up a kupo instance

The service provides a convenient file to start an instance of kupo in the local directory. Run the following command to let kupo sync from scratch.

```
bash scripts/run_kupo.sh
```

If you created a new wallet following the instructions above, consider adding them to kupo using the following command.

```
bash scripts/add_to_kupo.sh keys/<wallet_name>.vkey
```

Make sure to adjust the run script as described in the output so that you will not have trouble after a restart of kupo.
