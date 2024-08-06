use rusqlite::Connection;
use std::fmt;
use std::fs;
use std::io;

enum MyError {
    Rusqlite(rusqlite::Error),
    Filesystem(io::Error),
    Csv(csv::Error),
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MyError::Rusqlite(rusqlite) => write!(f, "{}", rusqlite),
            MyError::Filesystem(io) => write!(f, "{}", io),
            MyError::Csv(csv) => write!(f, "{}", csv),
        }
    }
}

impl From<rusqlite::Error> for MyError {
    fn from(err: rusqlite::Error) -> Self {
        MyError::Rusqlite(err)
    }
}

impl From<io::Error> for MyError {
    fn from(err: io::Error) -> Self {
        MyError::Filesystem(err)
    }
}

impl From<csv::Error> for MyError {
    fn from(err: csv::Error) -> Self {
        MyError::Csv(err)
    }
}

fn main() -> Result<(), String> {
    main_2().map_err(|e| e.to_string())
}

fn main_2() -> Result<(), MyError> {
    let conn = Connection::open_in_memory()?;

    let creation_sql = fs::read_to_string("../src/structure.sql")?;

    conn.execute_batch(&creation_sql)?;

    for entry in fs::read_dir("../feeds")? {
        let path = entry?.path();
        let feed_name = path
            .file_stem()
            .expect(&format!("Invalid filename: {:?}", path))
            .to_string_lossy()
            .to_string();

        if feed_name == "de-2024" {
            // continue;
        }

        println!("Loading feed {feed_name}");
        if path.is_dir() {
            conn.pragma_update(None, "synchronous", "OFF")?;
            conn.pragma_update(None, "journal_mode", "MEMORY")?;
            conn.execute("BEGIN", ())?;

            let mut path_entries = fs::read_dir(path)?
                .into_iter()
                .filter_map(|table| Some(table.ok()?.path()))
                .collect::<Vec<_>>();
            path_entries.sort_by_key(|table_path| {
                match &*table_path
                    .file_stem()
                    .expect(&format!("Invalid filename: {:?}", table_path))
                    .to_string_lossy()
                    .to_string()
                {
                    "feed_info" => 0,
                    "agency" => 1,
                    "levels" => 2,
                    "stops" => 3,
                    "routes" => 4,
                    "trips" => 5,
                    "location_groups" => 6,
                    "stop_times" => 7,
                    "calendar" => 8,
                    "calendar_dates" => 9,
                    "areas" => 10,
                    "stop_areas" => 11,
                    "networks" => 12,
                    "route_networks" => 13,
                    "shapes" => 14,
                    "frequencies" => 15,
                    "pathways" => 16,

                    _ => 70,
                }
            });
            for table_path in path_entries.into_iter() {
                if let Some(extension) = table_path.extension() {
                    if extension.to_string_lossy().to_string() != "txt" {
                        continue;
                    }
                } else {
                    continue;
                }

                let table_name = table_path
                    .file_stem()
                    .expect(&format!("Invalid filename: {:?}", table_path))
                    .to_string_lossy()
                    .to_string();

                if table_name == "shapes" {
                    continue;
                }

                println!("  Loading table {table_name}");

                let mut reader = csv::Reader::from_path(table_path)?;
                let headers = reader.headers()?;
                let columns = headers.into_iter().collect::<Vec<_>>().join(", ");
                let values = headers
                    .into_iter()
                    .map(|name| format!(":{name}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut statement = conn.prepare(&format!(
                    "INSERT INTO {table_name} (feed, {columns}) VALUES ('{feed_name}', {values})"
                ))?;

                if table_name == "stops" {
                    let parent_station_index = headers.iter().position(|n| n == "parent_station");
                    let mut rows = reader.into_records().collect::<Vec<_>>();
                    if let Some(parent_station_index) = parent_station_index {
                        rows.sort_by_key(|r| {
                            if let Ok(row) = r {
                                if let Some(parent_station) = row.get(parent_station_index) {
                                    if !parent_station.is_empty() {
                                        1
                                    } else {
                                        0
                                    }
                                } else {
                                    0
                                }
                            } else {
                                0
                            }
                        })
                    };
                    insert_all(&mut statement, rows)?;
                } else {
                    insert_all(&mut statement, reader.into_records())?;
                };
            }
            println!("Feed loaded, committing");
            conn.execute("COMMIT", ())?;
        }
    }

    let _ = fs::remove_file("../feeds.sqlite");
    conn.backup(rusqlite::MAIN_DB, "../feeds.sqlite", None)?;

    Ok(())
}

fn insert_all<'a, T>(statement: &mut rusqlite::Statement, rows: T) -> Result<(), MyError>
where
    T: IntoIterator<Item = csv::Result<csv::StringRecord>>,
{
    for row in rows {
        statement.execute(rusqlite::params_from_iter(row?.into_iter().map(|v| {
            if v.is_empty() {
                None
            } else {
                Some(v)
            }
        })))?;
    }

    Ok(())
}
