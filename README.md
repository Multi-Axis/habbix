# habbix

> Haskell army knife for Zabbix's database.

**Note:** This requires esqueleto >= 2 which is not yet in hackage.

## Static linking?

Very tricky, because haskell package `postgresql-libpq` requires `pq` library
which requires `krb5` library, and linking (recentish) `krb5` statically is
unsupported and won't work.

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
