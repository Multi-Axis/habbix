# habbix

> Haskell army knife for Zabbix's database.

**Note:** This requires esqueleto >= 2 which is not yet in hackage.

## Configuration

Copy the file `config.default.yaml` to `config.yaml` and edit the parameters in
it.

Make sure both databases exist.

## Usage

- Create local database schema: `habbix migrate-db`.
- Populate or update local db with data from remote db: `habbix sync-db`.
