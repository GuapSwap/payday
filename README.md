# payday
Payday CLI to retrieve GuapSwap protocol fee funds.

## Usage

### Usage Steps

1. Run your own node.
2. Download the latest release to minimize your own risk, or clone/download repository if you are adventurous. 
3. Install Java (JRE, JDK, or OpenJDK); you should have already if you are running a node. 
4. Modify the settings in the config file, and insert one of the available token tickers.
5. Run `java -jar payday-1.0.0.jar --help` to get command usage directions. Use the `--help` flag after any command to get usage directions.
6. If you would like to compile the jar yourself, download sbt and run `sbt assembly` within the repository/source folder.

### Usage WARNING

1. Protocol fee address must contain at least 1 ERG. 

### Available Commands
1. `payday [--onetime] <protocol_fee_address>`
4. `list <protocol_fee_address>`