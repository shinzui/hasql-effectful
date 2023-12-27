default:
  just --list

process-up:
  process-compose --tui=false up

create-test-database:
  psql -lqt | cut -d \| -f 1 | grep -qw $PGDATABASE || createdb $PGDATABASE
