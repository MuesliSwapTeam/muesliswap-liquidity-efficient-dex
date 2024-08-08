# OpShin Orderbook

## Build instructions

Contracts for the `onchain/orderbook` part, and the `onchain/concentrated-lp` part of the repository have to be built seperately. In the following, we give detailed instructions for each.

### OpShin Orderbook build instructions

#### Setup

1. Install Python 3.8, 3.9 or 3.10.

2. Ensure `python3 --version` works in your command line. Open a Terminal in the browser VSCode interface (F1 -> Terminal: Create New Terminal)
In Windows, you can do this by copying the `python.exe` file to `python3.exe` in your `PATH` environment variable.

3. Install python poetry.

On demeter.run or Linux/Ubuntu run 
```bash
curl -sSL https://install.python-poetry.org | python3 -
echo 'export PATH=/config/.local/bin:$PATH' >> ~/.bashrc
bash
```

Otherwise, follow the official documentation [here](https://python-poetry.org/docs/#installation).

4. Install a python virtual environment with poetry:
```bash
# install python dependencies
poetry install
# run a shell with the virtual environment activated
poetry shell
```

#### Running the tests

To run all the tests, simply do

```bash
pytest tests
```

#### Compiling the contracts

Compilation requires being inside the shell and having set up the [plutonomy-cli](https://github.com/OpShin/plutonomy-cli) in the `PATH`.

To compile all contracts, simply run the following command.

```bash
python3 -m scripts.build
```

This compiles and compresses (using the plutonomy-cli) all contracts.
