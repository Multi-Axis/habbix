# habbix

> Haskell army knife for Zabbix's database.

**Note:** This requires esqueleto >= 2 which is not yet in hackage.

## Configuration

Copy the file `config.default.yaml` to `config.yaml` and edit the parameters in
it.

Make sure both databases exist.

## Usage

- Create local database schema: `habbix migrate-db`.
- Populate or update local db with data from remote db: `habbix sync-db -s600`.
- Update recent history data only `habbix sync-db`

Careful with the populate/update outputs, by default they output a *lot* of
debug msgs to stderr.

## Futures

History synchronization is done for items present in the `item_future` table.
You need to add items to this table yourself. Note that we couple history with
future: when adding an `item` to be synced, you must specify a future prediction
model for it.

Available prediction models are stored in `future_model` table.
`future_model.name` describes a binary in
`$PWD/forecast_models/$future_model.name`, that gets executed every time a
synchronization is done. The prediction program is fed a `Event` JSON object in
its standard input, and a `Result` JSON object is expected as standard output.

## Static linking?

Very tricky, because haskell package `postgresql-libpq` requires `pq` library
which requires `krb5` library, and linking (recentish) `krb5` statically is
unsupported and won't work.
